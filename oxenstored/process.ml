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

let error fmt = Logging.error "process" fmt

let warn fmt = Logging.warn "process" fmt

let info fmt = Logging.info "process" fmt

let debug fmt = Logging.debug "process" fmt

open Printf
open Stdext

exception Transaction_again

exception Transaction_nested

exception Domain_not_match

exception Invalid_Cmd_Args

(* This controls the do_debug fn in this module, not the debug logging-function. *)
let allow_debug = ref false

let c_int_of_string s =
  let v = ref 0 in
  let is_digit c = c >= '0' && c <= '9' in
  let len = String.length s in
  let i = ref 0 in
  while !i < len && not (is_digit s.[!i]) do
    incr i
  done ;
  while !i < len && is_digit s.[!i] do
    let x = Char.code s.[!i] - Char.code '0' in
    v := (!v * 10) + x ;
    incr i
  done ;
  !v

(* when we don't want a limit, apply a max limit of 8 arguments.
   no arguments take more than 3 currently, which is pointless to split
   more than needed. *)
let split limit c s =
  let limit = match limit with None -> 8 | Some x -> x in
  String.split ~limit c s

let split_one_path data con =
  let args = split (Some 2) '\000' data in
  match args with
  | [path; ""] ->
      Store.Path.create path (Connection.get_path con)
  | _ ->
      raise Invalid_Cmd_Args

let process_watch source t cons =
  let oldroot = t.Transaction.oldroot in
  let newroot = Store.get_root t.Transaction.store in
  let ops = Transaction.get_paths t |> List.rev in
  let do_op_watch op cons =
    let recurse, oldroot, root =
      match fst op with
      | Xenbus.Xb.Op.Write | Xenbus.Xb.Op.Mkdir ->
          (false, None, newroot)
      | Xenbus.Xb.Op.Rm ->
          (true, None, oldroot)
      | Xenbus.Xb.Op.Setperms ->
          (false, Some oldroot, newroot)
      | _ ->
          raise (Failure "huh ?")
    in
    Connections.fire_watches ?oldroot source root cons (snd op) recurse
  in
  List.iter (fun op -> do_op_watch op cons) ops ;
  Connections.send_watchevents source

let create_implicit_path t perm path =
  let dirname = Store.Path.get_parent path in
  if not (Transaction.path_exists t dirname) then
    let rec check_path p =
      match p with
      | [] ->
          []
      | h :: l ->
          if Transaction.path_exists t h then
            check_path l
          else
            p
    in
    let ret = check_path (List.tl (Store.Path.get_hierarchy dirname)) in
    List.iter (fun s -> Transaction.mkdir ~with_watch:false t perm s) ret

module LiveUpdate = struct
  type t = {
      binary: string
    ; cmdline: string list
    ; deadline: float
    ; force: bool
    ; result: string list
    ; pending: bool
  }

  let state =
    ref
      {
        binary= Sys.executable_name
      ; cmdline= Sys.argv |> Array.to_list |> List.tl
      ; deadline= 0.
      ; force= false
      ; result= []
      ; pending= false
      }

  let debug = Printf.eprintf

  let forced_args = ["--live"; "--restart"]

  let args_of_t t =
    let filtered =
      List.filter (fun x -> not @@ List.mem x forced_args) t.cmdline
    in
    (t.binary, forced_args @ filtered)

  let string_of_t t =
    let executable, rest = args_of_t t in
    Filename.quote_command executable rest

  let launch_exn t =
    let executable, rest = args_of_t t in
    let args = Array.of_list (executable :: rest) in
    info "Launching %s, args: %s" executable (String.concat " " rest) ;
    Unix.execv args.(0) args

  let validate_exn t =
    (* --help must be last to check validity of earlier arguments *)
    let t' = {t with cmdline= t.cmdline @ ["--help"]} in
    let cmd = string_of_t t' in
    debug "Executing %s" cmd ;
    match Unix.fork () with
    | 0 -> (
      try launch_exn t' with _ -> exit 2
    )
    | pid -> (
      match Unix.waitpid [] pid with
      | _, Unix.WEXITED 0 ->
          debug "Live update validated cmdline %s" cmd ;
          t
      | _, Unix.WEXITED n ->
          invalid_arg (Printf.sprintf "Command %s exited with code %d" cmd n)
      | _, Unix.WSIGNALED n ->
          invalid_arg
            (Printf.sprintf "Command %s killed by ocaml signal number %d" cmd n)
      | _, Unix.WSTOPPED n ->
          invalid_arg
            (Printf.sprintf "Command %s stopped by ocaml signal number %d" cmd n)
    )

  let parse_live_update args =
    try
      (state :=
         match args with
         | ["-f"; file] ->
             validate_exn {!state with binary= file}
         | ["-a"] ->
             debug "Live update aborted" ;
             {!state with pending= false; result= []}
         | "-c" :: cmdline ->
             validate_exn {!state with cmdline= !state.cmdline @ cmdline}
         | "-s" :: _ -> (
           match (!state.pending, !state.result) with
           | true, _ ->
               !state (* no change to state, avoid resetting timeout *)
           | false, _ :: _ ->
               !state (* we got a pending result to deliver *)
           | false, [] ->
               let timeout = ref 60 in
               let force = ref false in
               Arg.parse_argv ~current:(ref 0) (Array.of_list args)
                 [
                   ( "-t"
                   , Arg.Set_int timeout
                   , "timeout in seconds to wait for active transactions to \
                      finish"
                   )
                 ; ( "-F"
                   , Arg.Set force
                   , "force live update to happen even with running \
                      transactions after timeout elapsed"
                   )
                 ]
                 (fun x -> raise (Arg.Bad x))
                 "live-update -s" ;
               debug "Live update process queued" ;
               {
                 !state with
                 deadline= Unix.gettimeofday () +. float !timeout
               ; force= !force
               ; pending= true
               }
         )
         | _ ->
             invalid_arg ("Unknown arguments: " ^ String.concat "," args)
      ) ;
      match (!state.pending, !state.result) with
      | true, _ ->
          Some "BUSY"
      | false, (_ :: _ as result) ->
          (* xenstore-control has read the result, clear it *)
          state := {!state with result= []} ;
          Some (String.concat "\n" result)
      | false, [] ->
          None
    with
    | Arg.Bad s | Arg.Help s | Invalid_argument s ->
        Some s
    | Unix.Unix_error (e, fn, args) ->
        Some (Printf.sprintf "%s(%s): %s" fn args (Unix.error_message e))

  let should_run cons =
    let t = !state in
    if t.pending then (
      match Connections.prevents_quit cons with
      | [] ->
          true
      | _ when Unix.gettimeofday () < t.deadline ->
          false
      | l ->
          warn "timeout reached: have to wait, migrate or shutdown %d domains:"
            (List.length l) ;
          let msgs =
            List.rev_map
              (fun con ->
                Printf.sprintf "%s: %d tx, out: %b, perm: %s"
                  (Connection.get_domstr con)
                  (Connection.number_of_transactions con)
                  (Connection.has_output con)
                  (Connection.get_perm con |> Perms.Connection.to_string)
              )
              l
          in
          List.iter (warn "Live-update: %s") msgs ;
          if t.force then (
            warn "Live update forced, some domain connections may break!" ;
            true
          ) else (
            warn "Live update aborted (see above for domains preventing it)" ;
            state := {t with pending= false; result= msgs} ;
            false
          )
    ) else
      false

  let completed () = state := {!state with result= ["OK"]}
end

(* packets *)
let do_debug con t _domains cons data =
  if (not (Connection.is_dom0 con)) && not !allow_debug then
    None
  else
    try
      match split None '\000' data with
      | "live-update" :: params ->
          let dropped_trailing_nul =
            params |> List.rev |> List.tl |> List.rev
          in
          LiveUpdate.parse_live_update dropped_trailing_nul
      | "print" :: msg :: _ ->
          Logging.xb_op ~tid:0 ~ty:Xenbus.Xb.Op.Debug ~con:"=======>" msg ;
          None
      | "quota" :: domid :: _ ->
          let domid = Utils.int_of_string_exn domid in
          let quota = Store.get_quota t.Transaction.store in
          Some (Quota.to_string quota domid ^ "\000")
      | "watches" :: _ ->
          let watches = Connections.debug cons in
          Some (watches ^ "\000")
      | "compact" :: _ ->
          Gc.compact () ; Some "Compacted"
      | "trim" :: _ ->
          History.trim () ; Some "trimmed"
      | "txn" :: domid :: _ ->
          let domid = Utils.int_of_string_exn domid in
          let con = Connections.find_domain cons domid in
          let b = Buffer.create 128 in
          let () =
            con.transactions
            |> Hashtbl.iter @@ fun _id tx ->
               Printf.bprintf b "paths: %d, operations: %d, quota_reached: %b\n"
                 (List.length tx.Transaction.paths)
                 (List.length tx.Transaction.operations)
                 tx.Transaction.quota_reached
          in
          Some (Buffer.contents b)
      | "xenbus" :: domid :: _ ->
          let domid = Utils.int_of_string_exn domid in
          let con = Connections.find_domain cons domid in
          let s =
            Printf.sprintf
              "xenbus: %s; overflow queue length: %d, can_input: %b, \
               has_more_input: %b, has_old_output: %b, has_new_output: %b, \
               has_more_work: %b."
              (Xenbus.Xb.debug con.xb)
              (Connection.source_pending_watchevents con)
              (Connection.can_input con)
              (Connection.has_more_input con)
              (Connection.has_old_output con)
              (Connection.has_new_output con)
              (Connection.has_more_work con)
          in
          Some s
      | "mfn" :: domid :: _ ->
          let domid = Utils.int_of_string_exn domid in
          let con = Connections.find_domain cons domid in
          may
            (fun dom -> Printf.sprintf "%nd\000" (Domain.get_mfn dom))
            (Connection.get_domain con)
      | _ ->
          None
    with _ -> None

let do_directory con t _domains _cons data =
  let path = split_one_path data con in
  let entries = Transaction.ls t (Connection.get_perm con) path in
  if entries <> [] then
    Utils.join_by_null entries ^ "\000"
  else
    ""

let do_directory_part con t _domains _cons data =
  (* Call only available to Dom0 *)
  if not (Connection.is_dom0 con) then
    raise Domain_not_match ;

  let directory_cache = Option.get con.Connection.directory_cache in

  let split_two_args data con =
    let args = split (Some 3) '\000' data in
    match args with
    | path :: offset :: _ ->
        ( Store.Path.create path (Connection.get_path con)
        , Utils.int_of_string_exn offset
        )
    | _ ->
        raise Invalid_Cmd_Args
  in

  (* First arg is node name. Second arg is childlist offset. *)
  let path, offset = split_two_args data con in

  let generation, children =
    Transaction.ls_partial t (Connection.get_perm con) path directory_cache
      con.Connection.directory_cache_gen_count
  in

  let generation_s = Printf.sprintf "%Ld\000" generation in
  let genlen = String.length generation_s in
  let children_length = String.length children in

  (* Offset behind list: just return a list with an empty string. *)
  if offset >= children_length then
    generation_s ^ "\000"
  else
    let buffer_length = ref 0 in
    let maxlen = Connection.xenstore_payload_max - genlen - 1 in
    let child_length = ref 0 in
    let i = ref offset in
    let cache_length = String.length children in
    while !buffer_length + !child_length < maxlen && !i < cache_length do
      child_length := !child_length + 1 ;

      if children.[!i] = '\000' then (
        if !buffer_length + !child_length < maxlen then
          buffer_length := !buffer_length + !child_length ;

        child_length := 0
      ) ;
      i := !i + 1
    done ;
    let last_chunk = offset + !buffer_length = children_length in
    let buffer =
      Bytes.create (!buffer_length + genlen + if last_chunk then 1 else 0)
    in
    String.blit generation_s 0 buffer 0 genlen ;
    String.blit children offset buffer genlen !buffer_length ;
    if last_chunk then
      Bytes.set buffer (!buffer_length + genlen) '\000' ;
    Bytes.to_string buffer

let do_read con t _domains _cons data =
  let path = split_one_path data con in
  Transaction.read t (Connection.get_perm con) path

let do_getperms con t _domains _cons data =
  let path = split_one_path data con in
  let perms = Transaction.getperms t (Connection.get_perm con) path in
  Perms.Node.to_string perms ^ "\000"

let do_getdomainpath _con _t _domains _cons data =
  let domid =
    match split None '\000' data with
    | [domid; ""] ->
        c_int_of_string domid
    | _ ->
        raise Invalid_Cmd_Args
  in
  sprintf "/local/domain/%u\000" domid

let do_write con t _domains _cons data =
  let path, value =
    match split (Some 2) '\000' data with
    | [path; value] ->
        (Store.Path.create path (Connection.get_path con), value)
    | _ ->
        raise Invalid_Cmd_Args
  in
  create_implicit_path t (Connection.get_perm con) path ;
  Transaction.write t (Connection.get_perm con) path value

let do_mkdir con t _domains _cons data =
  let path = split_one_path data con in
  create_implicit_path t (Connection.get_perm con) path ;
  try Transaction.mkdir t (Connection.get_perm con) path
  with Define.Already_exist -> ()

let do_rm con t _domains _cons data =
  let path = split_one_path data con in
  try Transaction.rm t (Connection.get_perm con) path
  with Define.Doesnt_exist -> ()

let do_setperms con t _domains _cons data =
  let path, perms =
    match split (Some 2) '\000' data with
    | path :: perms :: _ ->
        ( Store.Path.create path (Connection.get_path con)
        , Perms.Node.of_string perms
        )
    | _ ->
        raise Invalid_Cmd_Args
  in
  Transaction.setperms t (Connection.get_perm con) path perms

let do_error _con _t _domains _cons _data = raise Define.Unknown_operation

let do_isintroduced con _t domains _cons data =
  if not (Connection.is_dom0 con) then
    raise Define.Permission_denied ;
  let domid =
    match split None '\000' data with
    | domid :: _ ->
        Utils.int_of_string_exn domid
    | _ ->
        raise Invalid_Cmd_Args
  in
  if domid = Define.domid_self || Domains.exist domains domid then
    "T\000"
  else
    "F\000"

(* only in xen >= 4.2 *)
let do_reset_watches con _t _domains cons _data =
  Connections.del_watches cons con ;
  Connection.del_transactions con

(* only in >= xen3.3                                                                                    *)
let do_set_target con _t _domains cons data =
  if not (Connection.is_dom0 con) then
    raise Define.Permission_denied ;
  match split None '\000' data with
  | [domid; target_domid; ""] ->
      Connections.set_target cons (c_int_of_string domid)
        (c_int_of_string target_domid)
  | _ ->
      raise Invalid_Cmd_Args

(*------------- Generic handling of ty ------------------*)
let send_response ty con t rid response =
  match response with
  | Packet.Ack f ->
      Connection.send_ack con (Transaction.get_id t) rid ty ;
      (* Now do any necessary follow-up actions *)
      f ()
  | Packet.Reply ret ->
      Connection.send_reply con (Transaction.get_id t) rid ty ret
  | Packet.Error e ->
      Connection.send_error con (Transaction.get_id t) rid e

let reply_ack fct con t doms cons data =
  fct con t doms cons data ;
  Packet.Ack
    (fun () ->
      if Transaction.get_id t = Transaction.none then
        process_watch con t cons
    )

let reply_data fct con t doms cons data =
  let ret = fct con t doms cons data in
  Packet.Reply ret

let reply_data_or_ack fct con t doms cons data =
  match fct con t doms cons data with
  | Some ret ->
      Packet.Reply ret
  | None ->
      Packet.Ack (fun () -> ())

let reply_none fct con t doms cons data =
  (* let the function reply *)
  fct con t doms cons data

(* Functions for 'simple' operations that cannot be part of a transaction *)
let function_of_type_simple_op ty =
  match ty with
  | Xenbus.Xb.Op.Debug
  | Xenbus.Xb.Op.Watch
  | Xenbus.Xb.Op.Unwatch
  | Xenbus.Xb.Op.Transaction_start
  | Xenbus.Xb.Op.Transaction_end
  | Xenbus.Xb.Op.Introduce
  | Xenbus.Xb.Op.Release
  | Xenbus.Xb.Op.Isintroduced
  | Xenbus.Xb.Op.Resume
  | Xenbus.Xb.Op.Set_target
  | Xenbus.Xb.Op.Reset_watches
  | Xenbus.Xb.Op.Invalid ->
      error "called function_of_type_simple_op on operation %s"
        (Xenbus.Xb.Op.to_string ty) ;
      raise (Invalid_argument (Xenbus.Xb.Op.to_string ty))
  | Xenbus.Xb.Op.Directory ->
      reply_data do_directory
  | Xenbus.Xb.Op.Directory_part ->
      reply_data do_directory_part
  | Xenbus.Xb.Op.Read ->
      reply_data do_read
  | Xenbus.Xb.Op.Getperms ->
      reply_data do_getperms
  | Xenbus.Xb.Op.Getdomainpath ->
      reply_data do_getdomainpath
  | Xenbus.Xb.Op.Write ->
      reply_ack do_write
  | Xenbus.Xb.Op.Mkdir ->
      reply_ack do_mkdir
  | Xenbus.Xb.Op.Rm ->
      reply_ack do_rm
  | Xenbus.Xb.Op.Setperms ->
      reply_ack do_setperms
  | _ ->
      reply_ack do_error

let input_handle_error ~cons ~doms ~fct ~con ~t ~req =
  let reply_error e = Packet.Error e in
  try
    Transaction.check_quota_exn ~perm:(Connection.get_perm con) t ;
    fct con t doms cons req.Packet.data
  with
  | Define.Invalid_path ->
      reply_error "EINVAL"
  | Define.Already_exist ->
      reply_error "EEXIST"
  | Define.Doesnt_exist ->
      reply_error "ENOENT"
  | Define.Lookup_Doesnt_exist _ ->
      reply_error "ENOENT"
  | Define.Permission_denied ->
      reply_error "EACCES"
  | Not_found ->
      reply_error "ENOENT"
  | Invalid_Cmd_Args ->
      reply_error "EINVAL"
  | Invalid_argument _ ->
      reply_error "EINVAL"
  | Transaction_again ->
      reply_error "EAGAIN"
  | Transaction_nested ->
      reply_error "EBUSY"
  | Domain_not_match ->
      reply_error "EINVAL"
  | Quota.Limit_reached ->
      reply_error "EQUOTA"
  | Quota.Data_too_big ->
      reply_error "E2BIG"
  | Quota.Transaction_opened ->
      reply_error "EQUOTA"
  | Utils.ConversionFailed _ ->
      reply_error "EINVAL"
  | Define.Unknown_operation ->
      reply_error "ENOSYS"

let write_access_log ~ty ~tid ~con ~data = Logging.xb_op ~ty ~tid ~con data

let write_answer_log ~ty ~tid ~con ~data = Logging.xb_answer ~ty ~tid ~con data

let write_response_log ~ty ~tid ~con ~response =
  match response with
  | Packet.Ack _ ->
      write_answer_log ~ty ~tid ~con ~data:""
  | Packet.Reply x ->
      write_answer_log ~ty ~tid ~con ~data:x
  | Packet.Error e ->
      write_answer_log ~ty:Xenbus.Xb.Op.Error ~tid ~con ~data:e

let record_commit ~con ~tid ~before ~after =
  let inc r = r := Int64.add 1L !r in
  let finish_count = inc Transaction.counter ; !Transaction.counter in
  History.push {History.con; tid; before; after; finish_count}

(* Replay a stored transaction against a fresh store, check the responses are
   all equivalent: if so, commit the transaction. Otherwise send the abort to
   the client. *)
let transaction_replay c t doms cons =
  match t.Transaction.ty with
  | Transaction.No ->
      error "attempted to replay a non-full transaction" ;
      false
  | Transaction.Full (id, _oldstore, cstore) ->
      let tid = Connection.start_transaction c cstore in
      let replay_t = Transaction.make ~internal:true tid cstore in
      let con = sprintf "r(%d):%s" id (Connection.get_domstr c) in

      let perform_exn ~wlog txn (request, response) =
        if wlog then
          write_access_log ~ty:request.Packet.ty ~tid ~con
            ~data:request.Packet.data ;
        let fct = function_of_type_simple_op request.Packet.ty in
        let response' =
          input_handle_error ~cons ~doms ~fct ~con:c ~t:txn ~req:request
        in
        if wlog then
          write_response_log ~ty:request.Packet.ty ~tid ~con ~response:response' ;
        if not (Packet.response_equal response response') then
          raise Transaction_again
      in
      finally
        (fun () ->
          try
            Logging.start_transaction ~con ~tid ;
            List.iter
              (perform_exn ~wlog:true replay_t)
              (Transaction.get_operations t) ;

            (* May throw EAGAIN *)
            Logging.end_transaction ~con ~tid ;
            Transaction.commit ~con replay_t
          with
          | Transaction_again ->
              Transaction.failed_commits :=
                Int64.add !Transaction.failed_commits 1L ;
              let victim_domstr = Connection.get_domstr c in
              debug "Apportioning blame for EAGAIN in txn %d, domain=%s" id
                victim_domstr ;
              let punish guilty_con =
                debug "Blaming domain %s for conflict with domain %s txn %d"
                  (Connection.get_domstr guilty_con)
                  victim_domstr id ;
                Connection.decr_conflict_credit doms guilty_con
              in
              let judge_and_sentence hist_rec =
                let can_apply_on store =
                  let store = Store.copy store in
                  let trial_t =
                    Transaction.make ~internal:true Transaction.none store
                  in
                  try
                    List.iter
                      (perform_exn ~wlog:false trial_t)
                      (Transaction.get_operations t) ;
                    true
                  with Transaction_again -> false
                in
                if
                  can_apply_on hist_rec.History.before
                  && not (can_apply_on hist_rec.History.after)
                then (
                  punish hist_rec.History.con ;
                  true
                ) else
                  false
              in
              let guilty_cons =
                History.filter_connections ~ignore:c
                  ~since:t.Transaction.start_count ~f:judge_and_sentence
              in
              if Hashtbl.length guilty_cons = 0 then (
                debug
                  "Found no culprit for conflict in %s: must be self or not in \
                   history."
                  con ;
                Transaction.failed_commits_no_culprit :=
                  Int64.add !Transaction.failed_commits_no_culprit 1L
              ) ;
              false
          | e ->
              info "transaction_replay %d caught: %s" tid (Printexc.to_string e) ;
              false
        )
        (fun () -> ignore @@ Connection.end_transaction c tid None)

let do_watch con _t _domains cons data =
  let node, token, depth =
    match split None '\000' data with
    | [node; token; depth; ""] ->
        (node, token, Some (Utils.int_of_string_exn depth ~unsigned:true))
    | [node; token; ""] ->
        (node, token, None)
    | _ ->
        raise Invalid_Cmd_Args
  in
  let watch = Connections.add_watch cons con node token depth in
  Packet.Ack
    (fun () ->
      (* xenstore.txt says this watch is fired immediately,
         implying even if path doesn't exist or is unreadable *)
      Connection.fire_single_watch_unchecked con watch
    )

let do_unwatch con _t _domains cons data =
  let node, token =
    match split None '\000' data with
    | [node; token; ""] ->
        (node, token)
    | _ ->
        raise Invalid_Cmd_Args
  in
  ignore @@ Connections.del_watch cons con node token

let do_transaction_start con t _domains _cons _data =
  if Transaction.get_id t <> Transaction.none then
    raise Transaction_nested ;
  let store = Transaction.get_store t in
  string_of_int (Connection.start_transaction con store) ^ "\000"

let do_transaction_end con t domains cons data =
  let commit =
    match split None '\000' data with
    | "T" :: _ ->
        true
    | "F" :: _ ->
        false
    | x :: _ ->
        raise (Invalid_argument x)
    | _ ->
        raise Invalid_Cmd_Args
  in
  let commit = commit && not (Transaction.is_read_only t) in
  let success =
    let commit =
      if commit then
        Some (fun con trans -> transaction_replay con trans domains cons)
      else
        None
    in
    History.end_transaction t con (Transaction.get_id t) commit
  in
  if not success then
    raise Transaction_again ;
  if commit then (
    process_watch con t cons ;
    match t.Transaction.ty with
    | Transaction.No ->
        () (* no need to record anything *)
    | Transaction.Full (id, oldstore, cstore) ->
        record_commit ~con ~tid:id ~before:oldstore ~after:cstore
  )

let do_introduce con t domains cons data =
  if not (Connection.is_dom0 con) then
    raise Define.Permission_denied ;
  let domid, mfn, remote_port =
    match split None '\000' data with
    | domid :: mfn :: remote_port :: _ ->
        ( Utils.int_of_string_exn domid
        , Nativeint.of_string mfn
        , Utils.int_of_string_exn remote_port
        )
    | _ ->
        raise Invalid_Cmd_Args
  in
  let dom =
    if Domains.exist domains domid then (
      let edom = Domains.find domains domid in
      if Domain.get_mfn edom = mfn && Connections.find_domain cons domid != con
      then (* Use XS_INTRODUCE for recreating the xenbus event-channel. *)
        Domain.rebind_evtchn edom remote_port ;
      edom
    ) else
      try
        let ndom = Domains.create ~remote_port domains domid mfn in
        Connections.add_domain cons ndom ;
        Connections.fire_spec_watches (Transaction.get_root t) cons
          Store.Path.introduce_domain domid ;
        ndom
      with _ -> raise Invalid_Cmd_Args
  in
  if Domain.get_remote_port dom <> remote_port || Domain.get_mfn dom <> mfn then
    raise Domain_not_match

let do_release con t domains cons data =
  if not (Connection.is_dom0 con) then
    raise Define.Permission_denied ;
  let domid =
    match split None '\000' data with
    | [domid; ""] ->
        Utils.int_of_string_exn domid
    | _ ->
        raise Invalid_Cmd_Args
  in
  let fire_spec_watches = Domains.exist domains domid in
  Domains.del domains domid ;
  Connections.del_domain cons domid ;
  Store.reset_permissions (Transaction.get_store t) domid ;
  if fire_spec_watches then
    Connections.fire_spec_watches (Transaction.get_root t) cons
      Store.Path.release_domain domid
  else
    raise Invalid_Cmd_Args

let do_resume con _t domains _cons data =
  if not (Connection.is_dom0 con) then
    raise Define.Permission_denied ;
  let domid =
    match split None '\000' data with
    | domid :: _ ->
        Utils.int_of_string_exn domid
    | _ ->
        raise Invalid_Cmd_Args
  in
  if Domains.exist domains domid then
    Domains.resume domains domid
  else
    raise Invalid_Cmd_Args

let function_of_type ty =
  match ty with
  | Xenbus.Xb.Op.Debug ->
      reply_data_or_ack do_debug
  | Xenbus.Xb.Op.Watch ->
      reply_none do_watch
  | Xenbus.Xb.Op.Unwatch ->
      reply_ack do_unwatch
  | Xenbus.Xb.Op.Transaction_start ->
      reply_data do_transaction_start
  | Xenbus.Xb.Op.Transaction_end ->
      reply_ack do_transaction_end
  | Xenbus.Xb.Op.Introduce ->
      reply_ack do_introduce
  | Xenbus.Xb.Op.Release ->
      reply_ack do_release
  | Xenbus.Xb.Op.Isintroduced ->
      reply_data do_isintroduced
  | Xenbus.Xb.Op.Resume ->
      reply_ack do_resume
  | Xenbus.Xb.Op.Set_target ->
      reply_ack do_set_target
  | Xenbus.Xb.Op.Reset_watches ->
      reply_ack do_reset_watches
  | Xenbus.Xb.Op.Invalid ->
      reply_ack do_error
  | _ ->
      function_of_type_simple_op ty

(**
 * Determines which individual (non-transactional) operations we want to retain.
 * We only want to retain operations that have side-effects in the store since
 * these can be the cause of transactions failing.
*)
let retain_op_in_history ty =
  match ty with
  | Xenbus.Xb.Op.Write
  | Xenbus.Xb.Op.Mkdir
  | Xenbus.Xb.Op.Rm
  | Xenbus.Xb.Op.Setperms ->
      true
  | Xenbus.Xb.Op.Debug
  | Xenbus.Xb.Op.Directory
  | Xenbus.Xb.Op.Directory_part
  | Xenbus.Xb.Op.Read
  | Xenbus.Xb.Op.Getperms
  | Xenbus.Xb.Op.Watch
  | Xenbus.Xb.Op.Unwatch
  | Xenbus.Xb.Op.Transaction_start
  | Xenbus.Xb.Op.Transaction_end
  | Xenbus.Xb.Op.Introduce
  | Xenbus.Xb.Op.Release
  | Xenbus.Xb.Op.Getdomainpath
  | Xenbus.Xb.Op.Watchevent
  | Xenbus.Xb.Op.Error
  | Xenbus.Xb.Op.Isintroduced
  | Xenbus.Xb.Op.Resume
  | Xenbus.Xb.Op.Set_target
  | Xenbus.Xb.Op.Reset_watches
  | Xenbus.Xb.Op.Invalid ->
      false

let maybe_ignore_transaction = function
  | Xenbus.Xb.Op.Watch | Xenbus.Xb.Op.Unwatch ->
      fun tid ->
        if tid <> Transaction.none then
          debug "Ignoring transaction ID %d for watch/unwatch" tid ;
        Transaction.none
  | _ ->
      fun x -> x

let () = Printexc.record_backtrace true

(**
 * Nothrow guarantee.
*)
let process_packet ~store ~cons ~doms ~con ~req =
  let ty = req.Packet.ty in
  let tid = maybe_ignore_transaction ty req.Packet.tid in
  let rid = req.Packet.rid in
  try
    let fct = function_of_type ty in
    let t =
      if tid = Transaction.none then
        Transaction.make tid store
      else
        Connection.get_transaction con tid
    in

    let execute () = input_handle_error ~cons ~doms ~fct ~con ~t ~req in

    let response =
      (* Note that transactions are recorded in history separately. *)
      if tid = Transaction.none && retain_op_in_history ty then (
        let before = Store.copy store in
        let response = execute () in
        let after = Store.copy store in
        record_commit ~con ~tid ~before ~after ;
        response
      ) else
        execute ()
    in

    let response =
      try
        Transaction.check_quota_exn ~perm:(Connection.get_perm con) t ;
        if tid <> Transaction.none then
          (* Remember the request and response for this operation in case we need to replay the transaction *)
          Transaction.add_operation t req response ;
        response
      with Quota.Limit_reached -> Packet.Error "EQUOTA"
    in

    (* Put the response on the wire *)
    send_response ty con t rid response
  with exn ->
    let bt = Printexc.get_backtrace () in
    error "process packet: %s. %s" (Printexc.to_string exn) bt ;
    Connection.send_error con tid rid "EIO"

let do_input store cons doms con =
  let newpacket =
    try
      if Connection.can_input con then
        Connection.do_input con
      else
        None
    with
    | Xenbus.Xb.Reconnect ->
        info "%s requests a reconnect" (Connection.get_domstr con) ;
        History.reconnect con ;
        info "%s reconnection complete" (Connection.get_domstr con) ;
        None
    | Invalid_argument exp | Failure exp ->
        error "caught exception %s" exp ;
        error "got a bad client %s" (sprintf "%-8s" (Connection.get_domstr con)) ;
        Connection.mark_as_bad con ;
        None
  in

  match newpacket with
  | None ->
      ()
  | Some packet ->
      let tid, rid, ty, data = Xenbus.Xb.Packet.unpack packet in
      let req = {Packet.tid; Packet.rid; Packet.ty; Packet.data} in

      (* As we don't log IO, do not call an unnecessary sanitize_data
         		   info "[%s] -> [%d] %s \"%s\""
         		         (Connection.get_domstr con) tid
         		         (Xenbus.Xb.Op.to_string ty) (sanitize_data data); *)
      process_packet ~store ~cons ~doms ~con ~req ;
      write_access_log ~ty ~tid ~con:(Connection.get_domstr con) ~data ;
      Connection.incr_ops con

let do_output _store _cons _doms con =
  Connection.source_flush_watchevents con ;
  if Connection.has_output con then (
    ( if Connection.has_new_output con then
        let packet = Connection.peek_output con in
        let tid, _rid, ty, data = Xenbus.Xb.Packet.unpack packet in
        (* As we don't log IO, do not call an unnecessary sanitize_data
           			   info "[%s] <- %s \"%s\""
           			         (Connection.get_domstr con)
           			         (Xenbus.Xb.Op.to_string ty) (sanitize_data data);*)
        write_answer_log ~ty ~tid ~con:(Connection.get_domstr con) ~data
    ) ;
    try ignore (Connection.do_output con)
    with Xenbus.Xb.Reconnect ->
      info "%s requests a reconnect" (Connection.get_domstr con) ;
      History.reconnect con ;
      info "%s reconnection complete" (Connection.get_domstr con)
  )
