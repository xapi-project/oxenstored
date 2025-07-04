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

exception End_of_file

open Stdext

let xenstore_payload_max = 4096 (* xen/include/public/io/xs_wire.h *)

(** a bounded sender accepts an ['a] item and returns:
    None - if there is no room to accept the item
    Some () -  if it has successfully accepted/sent the item
*)
type 'a bounded_sender = 'a -> unit option

module BoundedPipe : sig
  type 'a t

  val create : capacity:int -> destination:'a bounded_sender -> 'a t
  (** [create ~capacity ~destination] creates a bounded pipe with a
      	    local buffer holding at most [capacity] items.  Once the buffer is
      	    full it will not accept further items.  items from the pipe are
      	    flushed into [destination] as long as it accepts items.  The
      	    destination could be another pipe.
      	 *)

  val is_empty : _ t -> bool
  (** [is_empty t] returns whether the local buffer of [t] is empty. *)

  val length : _ t -> int
  (** [length t] the number of items in the internal buffer *)

  val flush_pipe : _ t -> unit
  (** [flush_pipe t] sends as many items from the local buffer as possible,
      			which could be none. *)

  val push : 'a t -> 'a bounded_sender
  (** [push t item] tries to [flush_pipe] and then push [item]
      into the pipe if its [capacity] allows.
      Returns [None] if there is no more room *)
end = struct
  (* items are enqueued in [q], and then flushed to [connect_to] *)
  type 'a t = {q: 'a Queue.t; destination: 'a bounded_sender; capacity: int}

  let create ~capacity ~destination =
    {q= Queue.create (); capacity; destination}

  let rec flush_pipe t =
    if not Queue.(is_empty t.q) then
      let item = Queue.peek t.q in
      match t.destination item with
      | None ->
          () (* no room *)
      | Some () ->
          (* successfully sent item to next stage *)
          let _ = Queue.pop t.q in
          (* continue trying to send more items *)
          flush_pipe t

  let push t item =
    (* first try to flush as many items from this pipe as possible to make room,
       it is important to do this first to preserve the order of the items
    *)
    flush_pipe t ;
    if Queue.length t.q < t.capacity then (
      (* enqueue, instead of sending directly.
         this ensures that [out] sees the items in the same order as we receive them
      *)
      Queue.push item t.q ;
      Some (flush_pipe t)
    ) else
      None

  let is_empty t = Queue.is_empty t.q

  let length t = Queue.length t.q
end

type watch = {
    con: t
  ; token: string
  ; path: string
  ; base: string
  ; depth:
      int option (* directory levels below [path] to consider for a match *)
  ; path_depth: int
        (* directory levels of [path]. useful as a separate value
           because watch.path gets replaced with the event path
           during triggering and to avoid constant List.length calls *)
  ; is_relative: bool
  ; pending_watchevents: Xenbus.Xb.Packet.t BoundedPipe.t
}

and t = {
    xb: Xenbus.Xb.t
  ; dom: Domain.t option
  ; transactions: (int, Transaction.t) Hashtbl.t
  ; directory_cache_gen_count: int64 ref
        (* Used to provide connection-unique generation counts in case a cache entry
           was evicted and TODO *)
  ; directory_cache:
      (Store.Path.t, Store.Node.t * int64 * string * float) Hashtbl.t option
        (* Used for partial_directory calls, maps
           path -> (node, generation_count, children, timestamp)

           Generation count is used by the client to determine if the node has
           changed inbetween partial directory calls (OXenstored itself uses
           physical comparison between nodes, this exists for compatibility
           with the original protocol implemented by CXenstored). Documentation
           specifies that "<gencnt> being the same for multiple reads guarantees
           the node hasn't changed", so we fake a per-node generation count
           (CXenstored uses a global generation count that gets modified on every
           node write, incremented on transaction creation instead)

           This needs to be per-connection rather than per-transaction because
           clients might not necessarily create a transaction for all partial
           directory calls, we should keep the cache inbetween the transactions
           as well.
        *)
  ; poll_status: Poll.event option
        (* Connection's poll status. Only [Some Poll.event] when [dom = None],
           i.e. a connection is 'anonymous', otherwise it's [None] *)
  ; mutable next_tid: int
  ; watches: (string, watch list) Hashtbl.t
  ; mutable nb_watches: int
  ; anonid: int
  ; mutable stat_nb_ops: int
  ; mutable perm: Perms.Connection.t
  ; pending_source_watchevents: (watch * Xenbus.Xb.Packet.t) BoundedPipe.t
}

let update_poll_status_on_push con poll_status =
  (* event-based poll status needs to be updated only if it's currently
     the opposite of what a push (call to Xb.queue) can make it.

     So, if we couldn't write before, and the write queue became non-empty,
     set poll_status.write <- true.
     If we could read before, but the queue's capacity got filled up,
     set poll_status.read <- false. *)
  if (not poll_status.Poll.write) && Xenbus.Xb.output_len con.xb > 0 then
    poll_status.Poll.write <- true ;
  if poll_status.Poll.read && not (Xenbus.Xb.can_input con.xb) then
    poll_status.Poll.read <- false

let update_poll_status_on_pop con is_partial_empty poll_status =
  (* event-based poll status needs to be updated only if it's currently
     the opposite of what a pop (call to Xb.output) can make it.

     If we could write but now both partial and packet queues are empty,
     set poll_status.write <- false
     If we couldn't read before, but a pop freed up the queues,
     set poll_status.read <- true *)
  let no_more_writes = is_partial_empty && Xenbus.Xb.output_len con.xb = 0 in
  if poll_status.Poll.write && no_more_writes then
    poll_status.Poll.write <- false ;
  if (not poll_status.Poll.read) && Xenbus.Xb.can_input con.xb then
    poll_status.Poll.read <- true

let source_flush_watchevents t =
  BoundedPipe.flush_pipe t.pending_source_watchevents ;
  (* pending_source_watchevents is possibly empty now, check if
     poll_status.read should be set to true *)
  Option.iter
    (fun poll_status ->
      if
        (not poll_status.Poll.read)
        && BoundedPipe.is_empty t.pending_source_watchevents
        && Xenbus.Xb.can_input t.xb
      then
        poll_status.Poll.read <- true
    )
    t.poll_status

let source_pending_watchevents t =
  BoundedPipe.length t.pending_source_watchevents

let mark_as_bad con =
  match con.dom with None -> () | Some domain -> Domain.mark_as_bad domain

let initial_next_tid = 1

let do_reconnect con =
  Xenbus.Xb.reconnect con.xb ;
  (* dom is the same *)
  Hashtbl.clear con.transactions ;
  con.next_tid <- initial_next_tid ;
  Hashtbl.clear con.watches ;
  (* anonid is the same *)
  con.nb_watches <- 0 ;
  con.stat_nb_ops <- 0 ;
  (* perm is the same *)
  ()

let get_path con =
  Printf.sprintf "/local/domain/%i/"
    (match con.dom with None -> 0 | Some d -> Domain.get_id d)

let get_watch_path_with_base base path =
  if path.[0] = '@' || path.[0] = '/' then
    path
  else
    base ^ path

let watch_create ~con ~path ~token ~depth =
  let base = get_path con in
  let path_depth =
    get_watch_path_with_base base path |> Store.Path.of_string |> List.length
  in
  {
    con
  ; token
  ; path
  ; base
  ; depth
  ; path_depth
  ; is_relative= path.[0] <> '/' && path.[0] <> '@'
  ; pending_watchevents=
      BoundedPipe.create ~capacity:!Define.maxwatchevents
        ~destination:(Xenbus.Xb.queue con.xb)
      (* NOTE: Every piece of code that interacts with this pipe needs to update
         poll_status accordingly *)
  }

let get_con w = w.con

let number_of_transactions con = Hashtbl.length con.transactions

let get_domain con = con.dom

let anon_id_next = ref 1

let get_domstr con =
  match con.dom with
  | None ->
      Printf.sprintf "DOM0 (ANON CONN #%d)" con.anonid
  | Some dom ->
      Printf.sprintf "DOM%d" (Domain.get_id dom)

let make_perm dom =
  let domid = match dom with None -> 0 | Some d -> Domain.get_id d in
  Perms.Connection.create ~perms:[Perms.READ; Perms.WRITE] domid

let create xbcon dom =
  let destination (watch, pkt) =
    let r = BoundedPipe.push watch.pending_watchevents pkt in
    Option.iter (update_poll_status_on_push watch.con) watch.con.poll_status ;
    r
  in
  let id =
    match dom with
    | None ->
        let old = !anon_id_next in
        incr anon_id_next ; old
    | Some _ ->
        0
  in
  let directory_cache =
    match dom with
    | Some dom ->
        if Domain.get_id dom = 0 then
          Some (Hashtbl.create 8)
        else
          None
    | None ->
        Some (Hashtbl.create 8)
  in
  (* Initially, the write queues are empty, so we can't write, but our ability
     to read is technically determined by the limits and capacity set.
     Only set poll status for anonymous connections *)
  let poll_status =
    let read = Xenbus.Xb.can_input xbcon in
    (* && BoundedPipe.is_empty pending_source_watchevents is always true on
       creation, no need to check *)
    match dom with
    | Some _ ->
        None
    | None ->
        Some Poll.{read; write= false; can_read= false; can_write= false}
  in
  let con =
    {
      xb= xbcon
    ; dom
    ; transactions= Hashtbl.create 5
    ; directory_cache_gen_count=
        ref 1L
        (* Start the generation count with 1, since 0 is a special
           NULL value in CXenstored *)
    ; directory_cache
    ; poll_status
    ; next_tid= initial_next_tid
    ; watches= Hashtbl.create 8
    ; nb_watches= 0
    ; anonid= id
    ; stat_nb_ops= 0
    ; perm= make_perm dom
    ; (* the actual capacity will be lower, this is used as an overflow
         buffer: anything that doesn't fit elsewhere gets put here, only
         limited by the amount of watches that you can generate with a
         single xenstore command (which is finite, although possibly very
         large in theory for Dom0).  Once the pipe here has any contents the
         domain is blocked from sending more commands until it is empty
         again though.
      *)
      pending_source_watchevents=
        BoundedPipe.create ~capacity:Sys.max_array_length ~destination
    }
  in
  Logging.new_connection ~tid:Transaction.none ~con:(get_domstr con) ;
  con

let get_fd con = Xenbus.Xb.get_fd con.xb

let close con =
  Logging.end_connection ~tid:Transaction.none ~con:(get_domstr con) ;
  Xenbus.Xb.close ~under_testing:Testing_status.under_testing con.xb

let get_perm con = con.perm

let set_target con target_domid =
  con.perm <-
    Perms.Connection.set_target (get_perm con) ~perms:[Perms.READ; Perms.WRITE]
      target_domid

let is_backend_mmap con = Xenbus.Xb.is_mmap con.xb

let packet_of _con tid rid ty data =
  if String.length data > xenstore_payload_max then
    Xenbus.Xb.Packet.create tid rid Xenbus.Xb.Op.Error "E2BIG\000"
  else
    Xenbus.Xb.Packet.create tid rid ty data

let send_reply con tid rid ty data =
  let result = Xenbus.Xb.queue con.xb (packet_of con tid rid ty data) in

  Option.iter (update_poll_status_on_push con) con.poll_status ;

  (* should never happen: we only process an input packet when there is room for an output packet *)
  (* and the limit for replies is different from the limit for watch events *)
  assert (result <> None)

let send_error con tid rid err =
  send_reply con tid rid Xenbus.Xb.Op.Error (err ^ "\000")

let send_ack con tid rid ty = send_reply con tid rid ty "OK\000"

let get_watch_path con path =
  if path.[0] = '@' || path.[0] = '/' then
    path
  else
    let rpath = get_path con in
    rpath ^ path

let get_watches (con : t) path =
  Option.value ~default:[] (Hashtbl.find_opt con.watches path)

let get_children_watches con path =
  let path = path ^ "/" in
  List.concat
    (Hashtbl.fold
       (fun p w l -> if String.starts_with ~prefix:path p then w :: l else l)
       con.watches []
    )

let is_dom0 con = Perms.Connection.is_dom0 (get_perm con)

let add_watch con (path, apath) token depth =
  if
    !Quota.activate
    && !Define.maxwatch > 0
    && (not (is_dom0 con))
    && con.nb_watches > !Define.maxwatch
  then
    raise Quota.Limit_reached ;
  let l = get_watches con apath in
  if List.exists (fun w -> w.token = token) l then
    raise Define.Already_exist ;
  let watch = watch_create ~con ~token ~path ~depth in
  Hashtbl.replace con.watches apath (watch :: l) ;
  con.nb_watches <- con.nb_watches + 1 ;
  watch

let del_watch con path token =
  let apath = get_watch_path con path in
  let ws = Hashtbl.find con.watches apath in
  let w = List.find (fun w -> w.token = token) ws in
  let filtered = Utils.list_remove w ws in
  if filtered <> [] then
    Hashtbl.replace con.watches apath filtered
  else
    Hashtbl.remove con.watches apath ;
  con.nb_watches <- con.nb_watches - 1 ;
  (apath, w)

let del_watches con =
  Hashtbl.reset con.watches ;
  con.nb_watches <- 0

let del_transactions con = Hashtbl.reset con.transactions

let list_watches con =
  let ll =
    Hashtbl.fold
      (fun _ watches acc ->
        List.map (fun watch -> (watch.path, watch.token, watch.depth)) watches
        :: acc
      )
      con.watches []
  in
  List.concat ll

let dbg fmt = Logging.debug "connection" fmt

let info fmt = Logging.info "connection" fmt

let lookup_watch_perm path = function
  | None ->
      []
  | Some root -> (
    try
      Store.Path.apply root path @@ fun parent name ->
      Store.Node.get_perms parent
      ::
      ( try [Store.Node.get_perms (Store.Node.find parent name)]
        with Not_found -> []
      )
    with Define.Invalid_path | Not_found -> []
  )

let lookup_watch_perms oldroot root path =
  lookup_watch_perm path oldroot @ lookup_watch_perm path (Some root)

let fire_single_watch_unchecked source watch =
  let data = Utils.join_by_null [watch.path; watch.token; ""] in
  let pkt =
    packet_of watch.con Transaction.none 0 Xenbus.Xb.Op.Watchevent data
  in

  match BoundedPipe.push source.pending_source_watchevents (watch, pkt) with
  | Some () ->
      (* pending_source_watchevents is no longer empty,
         set poll_status.read <- false *)
      Option.iter
        (fun poll_status ->
          if poll_status.Poll.read then poll_status.Poll.read <- false
        )
        watch.con.poll_status ;
      () (* packet queued *)
  | None ->
      (* a well behaved Dom0 shouldn't be able to trigger this,
         if it happens it is likely a Dom0 bug causing runaway memory usage
      *)
      failwith "watch event overflow, cannot happen"

let fire_single_watch source (oldroot, root) depth watch =
  let abspath =
    get_watch_path_with_base watch.base watch.path |> Store.Path.of_string
  in
  (* If the watch is "special" or does not have a depth specified (all watches
     before the extended watches feature), it's always triggered. Otherwise
     it is to be triggered if the difference between the directory level of
     the event's child path and watch's specified path is less or equal to
     the watch's specified depth

     e.g., for a watch registered on the path /a with different depths,
     the table specifies whether or not the watch will be triggered:
     .----------------------------------.-------.-------.------.
     | Trigger event path \ Watch depth |   0   |   1   |  2   |
     :----------------------------------+-------+-------+------:
     | /a                               | true  | true  | true |
     :----------------------------------+-------+-------+------:
     | /a/b                             | false | true  | true |
     :----------------------------------+-------+-------+------:
     | /a/b/c                           | false | false | true |
     '----------------------------------'-------'-------'------'
  *)
  let depth_satisfied =
    if watch.path.[0] = '@' then
      true
    else
      match watch.depth with
      | None ->
          true
      | Some x when x >= depth - watch.path_depth ->
          true
      | _ ->
          false
  in
  if depth_satisfied then
    let perms = lookup_watch_perms oldroot root abspath in
    if Perms.can_fire_watch watch.con.perm perms then
      fire_single_watch_unchecked source watch
    else
      let perms =
        perms |> List.map (Perms.Node.to_string ~sep:" ") |> String.concat ", "
      in
      let con = get_domstr watch.con in
      Logging.watch_not_fired ~con perms (Store.Path.to_string abspath)

let fire_watch source roots watch path depth =
  let new_path =
    if watch.is_relative && path.[0] = '/' then
      let n = String.length watch.base and m = String.length path in
      String.sub path n (m - n)
    else
      path
  in
  fire_single_watch source roots depth {watch with path= new_path}

(* Search for a valid unused transaction id. *)
let rec valid_transaction_id con proposed_id =
  (*
	 * Clip proposed_id to the range [1, 0x3ffffffe]
	 *
	 * The chosen id must not trucate when written into the uint32_t tx_id
	 * field, and needs to fit within the positive range of a 31 bit ocaml
	 * integer to function when compiled as 32bit.
	 *
	 * Oxenstored therefore supports only 1 billion open transactions.
	 *)
  let id =
    if proposed_id <= 0 || proposed_id >= 0x3fffffff then 1 else proposed_id
  in

  if Hashtbl.mem con.transactions id then
    (* Outstanding transaction with this id.  Try the next. *)
    valid_transaction_id con (id + 1)
  else
    id

let start_transaction con store =
  if
    !Define.maxtransaction > 0
    && (not (is_dom0 con))
    && Hashtbl.length con.transactions > !Define.maxtransaction
  then
    raise Quota.Transaction_opened ;
  let id = valid_transaction_id con con.next_tid in
  con.next_tid <- id + 1 ;
  let ntrans = Transaction.make id store in
  Hashtbl.add con.transactions id ntrans ;
  Logging.start_transaction ~tid:id ~con:(get_domstr con) ;
  id

let end_transaction con tid commit =
  let trans = Hashtbl.find con.transactions tid in
  Hashtbl.remove con.transactions tid ;
  Logging.end_transaction ~tid ~con:(get_domstr con) ;
  match commit with
  | None ->
      true
  | Some transaction_replay_f ->
      Transaction.commit ~con:(get_domstr con) trans
      || transaction_replay_f con trans

let get_transaction con tid = Hashtbl.find con.transactions tid

let do_input con = Xenbus.Xb.input con.xb

let has_partial_input con = Xenbus.Xb.has_partial_input con.xb

let has_more_input con = Xenbus.Xb.has_more_input con.xb

let can_input con =
  Xenbus.Xb.can_input con.xb
  && BoundedPipe.is_empty con.pending_source_watchevents

let has_output con = Xenbus.Xb.has_output con.xb

let has_old_output con = Xenbus.Xb.has_old_output con.xb

let has_new_output con = Xenbus.Xb.has_new_output con.xb

let peek_output con = Xenbus.Xb.peek_output con.xb

let do_output con =
  let is_partial_empty = Xenbus.Xb.output con.xb in
  Option.iter (update_poll_status_on_pop con is_partial_empty) con.poll_status ;
  is_partial_empty

let is_bad con =
  match con.dom with None -> false | Some dom -> Domain.is_bad_domain dom

(* oxenstored currently only dumps limited information about its state.
   A live update is only possible if any of the state that is not dumped would be empty.
   Compared to https://xenbits.xen.org/docs/unstable/designs/xenstore-migration.html:
     * GLOBAL_DATA: not strictly needed, systemd is giving the socket FDs to us
     * CONNECTION_DATA: PARTIAL
       * for domains: PARTIAL, see Connection.dump -> Domain.dump, only if data and tdomid is empty
       * for sockets (Dom0 toolstack): NO
     * WATCH_DATA: OK, see Connection.dump
     * TRANSACTION_DATA: NO
     * NODE_DATA: OK (except for transactions), see Store.dump_fct and DB.to_channel

   Also xenstored will never talk to a Domain once it is marked as bad,
   so treat it as idle for live-update.

   Restrictions below can be relaxed once xenstored learns to dump more
   of its live state in a safe way *)
let has_extra_connection_data con =
  let has_in = has_partial_input con in
  let has_out = has_output con in
  let has_nondefault_perms = make_perm con.dom <> con.perm in
  has_in
  || has_out
  (* TODO: what about SIGTERM, should use systemd to store FDS
     	|| has_socket (* dom0 sockets not * dumped yet *) *)
  || has_nondefault_perms (* set_target not dumped yet *)

let has_transaction_data con =
  let n = number_of_transactions con in
  dbg "%s: number of transactions = %d" (get_domstr con) n ;
  n > 0

let prevents_live_update con =
  (not (is_bad con))
  && (has_extra_connection_data con || has_transaction_data con)

let has_more_work con =
  (has_more_input con && can_input con)
  || ((not (has_old_output con)) && has_new_output con)

let incr_ops con = con.stat_nb_ops <- con.stat_nb_ops + 1

let stats con = (Hashtbl.length con.watches, con.stat_nb_ops)

let dump con chan =
  let id =
    match con.dom with
    | Some dom ->
        let domid = Domain.get_id dom in
        (* dump domain *)
        Domain.dump dom chan ; domid
    | None ->
        let fd = con |> get_fd |> Utils.FD.to_int in
        Printf.fprintf chan "socket,%d\n" fd ;
        -fd
  in
  (* dump watches *)
  List.iter
    (fun (path, token, depth) ->
      let depth =
        match depth with None -> "" | Some x -> Printf.sprintf ",%d" x
      in
      Printf.fprintf chan "watch,%d,%s,%s%s\n" id (Utils.hexify path)
        (Utils.hexify token) depth
    )
    (list_watches con)

let debug con =
  let domid = get_domstr con in
  let watches =
    List.map
      (fun (path, token, depth) ->
        let depth =
          match depth with
          | None ->
              "None"
          | Some x ->
              Printf.sprintf "Some %d" x
        in
        Printf.sprintf "watch %s: %s %s %s\n" domid path token depth
      )
      (list_watches con)
  in
  String.concat "" watches

let decr_conflict_credit doms con =
  match con.dom with
  | None ->
      ()
      (* It's a socket connection. We don't know which domain we're in, so treat it as if it's free to conflict *)
  | Some dom ->
      Domains.decr_conflict_credit doms dom
