(*
 * Copyright (C) 2006-2007 XenSource Ltd.
 * Copyright (C) 2008      Citrix Ltd.
 * Author Vincent Hanquez <vincent.hanquez@eu.citrix.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

let debug fmt = Logging.debug "domains" fmt

let error fmt = Logging.error "domains" fmt

let warn fmt = Logging.warn "domains" fmt

let load_plug fname =
  let fail_with fmt = Printf.ksprintf failwith fmt in
  let fname = Dynlink.adapt_filename fname in
  if Sys.file_exists fname then
    try Dynlink.loadfile fname with
    | Dynlink.Error err as e ->
        error "ERROR loading plugin '%s': %s\n%!" fname
          (Dynlink.error_message err) ;
        raise e
    | _ ->
        fail_with "Unknown error while loading plugin"
  else
    fail_with "Plugin file '%s' does not exist" fname

let () =
  (* Do not link with the plugin when being tested *)
  if not Testing_status.under_testing then (
    let plugins_dir =
      Filename.concat Paths.libexec "/ocaml/xsd_glue/xenctrl_plugin/"
    in
    let filepath = plugins_dir ^ "domain_getinfo_v1.cmxs" in
    debug "Trying to load plugin '%s'\n%!" filepath ;
    let list_files = Sys.readdir plugins_dir in
    debug "Directory listing of '%s'\n%!" plugins_dir ;
    Array.iter (fun x -> debug "\t%s\n%!" x) list_files ;
    Dynlink.allow_only ["Plugin_interface_v1"] ;
    load_plug filepath
  )

module Plugin =
  ( val Plugin_interface_v1.get_plugin_v1 ()
      : Plugin_interface_v1.Domain_getinfo_V1
    )

let handle = Plugin.interface_open ()

type domains = {
    eventchn: Event.t
  ; gnttab: Gnt.Gnttab.interface
  ; table: (Plugin.domid, Domain.t) Hashtbl.t
  ; (* N.B. the Queue module is not thread-safe but oxenstored is single-threaded. *)
    (* Domains queue up to regain conflict-credit; we have a queue for
       	   domains that are carrying some penalty and so are below the
       	   maximum credit, and another queue for domains that have run out of
       	   credit and so have had their access paused. *)
    doms_conflict_paused: Domain.t option ref Queue.t
  ; doms_with_conflict_penalty: Domain.t option ref Queue.t
  ; (* A callback function to be called when we go from zero to one paused domain.
       	   This will be to reset the countdown until the next unit of credit is issued. *)
    on_first_conflict_pause: unit -> unit
  ; (* If config is set to use individual instead of aggregate conflict-rate-limiting,
       	   we use these counts instead of the queues. The second one includes the first. *)
    mutable n_paused: int (* Number of domains with zero or negative credit *)
  ; mutable n_penalised: int
        (* Number of domains with less than maximum credit *)
}

let init eventchn gnttab on_first_conflict_pause =
  {
    eventchn
  ; gnttab
  ; table= Hashtbl.create 10
  ; doms_conflict_paused= Queue.create ()
  ; doms_with_conflict_penalty= Queue.create ()
  ; on_first_conflict_pause
  ; n_paused= 0
  ; n_penalised= 0
  }

let del doms id = Hashtbl.remove doms.table id

let exist doms id = Hashtbl.mem doms.table id

let find doms id = Hashtbl.find doms.table id

let number doms = Hashtbl.length doms.table

let iter doms fct = Hashtbl.iter (fun _ b -> fct b) doms.table

let eventchn doms = doms.eventchn

let rec is_empty_queue q =
  Queue.is_empty q
  ||
  if !(Queue.peek q) = None then (
    ignore (Queue.pop q) ;
    is_empty_queue q
  ) else
    false

let all_at_max_credit doms =
  if !Define.conflict_rate_limit_is_aggregate then
    (* Check both becuase if burst limit is 1.0 then a domain can go straight
       		 * from max-credit to paused without getting into the penalty queue. *)
    is_empty_queue doms.doms_with_conflict_penalty
    && is_empty_queue doms.doms_conflict_paused
  else
    doms.n_penalised = 0

(* Functions to handle queues of domains given that the domain might be deleted while in a queue. *)
let push dom queue = Queue.push (ref (Some dom)) queue

let rec pop queue =
  match !(Queue.pop queue) with None -> pop queue | Some x -> x

let remove_from_queue dom queue =
  Queue.iter
    (fun d -> match !d with None -> () | Some x -> if x = dom then d := None)
    queue

let cleanup doms =
  let notify = ref false in
  let dead_dom = ref [] in

  Hashtbl.iter
    (fun id _ ->
      if id <> 0 then (
        try
          let info = Plugin.domain_getinfo handle id in
          if info.Plugin.shutdown || info.Plugin.dying then (
            debug "Domain %u died (dying=%b, shutdown %b -- code %d)" id
              info.Plugin.dying info.Plugin.shutdown info.Plugin.shutdown_code ;
            if info.Plugin.dying then
              dead_dom := id :: !dead_dom
            else
              notify := true
          )
        with Plugin.Error _ ->
          debug "Domain %u died -- no domain info" id ;
          dead_dom := id :: !dead_dom
      )
    )
    doms.table ;
  List.iter
    (fun id ->
      let dom = Hashtbl.find doms.table id in
      Domain.close dom ;
      Hashtbl.remove doms.table id ;
      if dom.Domain.conflict_credit <= !Define.conflict_burst_limit then (
        remove_from_queue dom doms.doms_with_conflict_penalty ;
        if dom.Domain.conflict_credit <= 0. then
          remove_from_queue dom doms.doms_conflict_paused
      )
    )
    !dead_dom ;
  (!notify, !dead_dom)

let resume _doms _domid = ()

let create doms ?local_port ~remote_port domid mfn =
  let mapping = Gnt.(Gnttab.map_exn doms.gnttab {domid; ref= xenstore} true) in
  (*let interface = Gnt.Gnttab.Local_mapping.*)
  let interface = Gnt.Gnttab.Local_mapping.to_pages doms.gnttab mapping in
  let dom =
    Domain.make ?local_port ~remote_port domid mfn interface doms.eventchn
  in
  Hashtbl.add doms.table domid dom ;
  dom

let xenstored_kva = ref ""

let xenstored_port = ref ""

let create0 ?local_port doms =
  let remote_port = Utils.read_file_single_integer !xenstored_port in

  let interface =
    let fd = Unix.openfile !xenstored_kva [Unix.O_RDWR] 0o600 in
    let interface =
      Xenmmap.mmap fd Xenmmap.RDWR Xenmmap.SHARED (Xenmmap.getpagesize ()) 0
    in
    Unix.close fd ; interface
  in

  let dom =
    Domain.make ?local_port ~remote_port 0 Nativeint.zero interface
      doms.eventchn
  in
  Hashtbl.add doms.table 0 dom ;
  Domain.notify dom ;
  dom

let decr_conflict_credit doms dom =
  dom.Domain.caused_conflicts <- Int64.add 1L dom.Domain.caused_conflicts ;
  let before = dom.Domain.conflict_credit in
  let after = max (-1.0) (before -. 1.0) in
  debug "decr_conflict_credit dom%d %F -> %F" (Domain.get_id dom) before after ;
  dom.Domain.conflict_credit <- after ;
  let newly_penalised =
    before >= !Define.conflict_burst_limit
    && after < !Define.conflict_burst_limit
  in
  let newly_paused = before > 0.0 && after <= 0.0 in
  if !Define.conflict_rate_limit_is_aggregate then
    if newly_penalised && after > 0.0 then
      push dom doms.doms_with_conflict_penalty
    else if newly_paused then (
      let first_pause = Queue.is_empty doms.doms_conflict_paused in
      push dom doms.doms_conflict_paused ;
      if first_pause then doms.on_first_conflict_pause ()
    ) else
      ( (* The queues are correct already: no further action needed. *) )
  else (
    if newly_penalised then doms.n_penalised <- doms.n_penalised + 1 ;
    if newly_paused then (
      doms.n_paused <- doms.n_paused + 1 ;
      if doms.n_paused = 1 then doms.on_first_conflict_pause ()
    )
  )

(* Give one point of credit to one domain, and update the queues appropriately. *)
let incr_conflict_credit_from_queue doms =
  let process_queue q requeue_test =
    let d = pop q in
    let before = d.Domain.conflict_credit in
    (* just for debug-logging *)
    d.Domain.conflict_credit <-
      min (d.Domain.conflict_credit +. 1.0) !Define.conflict_burst_limit ;
    debug "incr_conflict_credit_from_queue: dom%d: %F -> %F" (Domain.get_id d)
      before d.Domain.conflict_credit ;
    if requeue_test d.Domain.conflict_credit then
      push d q (* Make it queue up again for its next point of credit. *)
  in
  let paused_queue_test cred = cred <= 0.0 in
  let penalty_queue_test cred = cred < !Define.conflict_burst_limit in
  try process_queue doms.doms_conflict_paused paused_queue_test
  with Queue.Empty -> (
    try process_queue doms.doms_with_conflict_penalty penalty_queue_test
    with Queue.Empty -> () (* Both queues are empty: nothing to do here. *)
  )

let incr_conflict_credit doms =
  if !Define.conflict_rate_limit_is_aggregate then
    incr_conflict_credit_from_queue doms
  else (* Give a point of credit to every domain, subject only to the cap. *)
    let inc dom =
      let before = dom.Domain.conflict_credit in
      let after = min (before +. 1.0) !Define.conflict_burst_limit in
      dom.Domain.conflict_credit <- after ;
      debug "incr_conflict_credit dom%d: %F -> %F" (Domain.get_id dom) before
        after ;

      if before <= 0.0 && after > 0.0 then
        doms.n_paused <- doms.n_paused - 1 ;

      if
        before < !Define.conflict_burst_limit
        && after >= !Define.conflict_burst_limit
      then
        doms.n_penalised <- doms.n_penalised - 1
    in
    if doms.n_penalised > 0 then iter doms inc
