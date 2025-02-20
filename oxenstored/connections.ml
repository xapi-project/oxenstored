(*
 * Copyright (C) 2006-2007 XenSource Ltd.
 * Copyright (C) 2008      Citrix Ltd.
 * Author Vincent Hanquez <vincent.hanquez@eu.citrix.com>
 * Author Thomas Gazagnaire <thomas.gazagnaire@eu.citrix.com>
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

module Xeneventchn = Eventchn

let debug fmt = Logging.debug "connections" fmt

type t = {
    anonymous: (Unix.file_descr, Connection.t * int) Hashtbl.t
        (* (fd -> Connection.t, index) where index maps to the poll_status array *)
  ; mutable poll_status: (Unix.file_descr * Poll.event) array
        (* Poll.event points to immutable con.poll_status, whose fields are mutable *)
  ; domains: (int, Connection.t) Hashtbl.t
  ; ports: (Xeneventchn.t, Connection.t) Hashtbl.t
  ; mutable watches: Connection.watch list Trie.t
}

let create () =
  {
    anonymous= Hashtbl.create 37
  ; poll_status= [||]
  ; domains= Hashtbl.create 37
  ; ports= Hashtbl.create 37
  ; watches= Trie.create ()
  }

let get_capacity () =
  (* not multiplied by maxwatch on purpose: 2nd queue in watch itself! *)
  {
    Xenbus.Xb.maxoutstanding= !Define.maxoutstanding
  ; maxwatchevents= !Define.maxwatchevents
  }

let default_poll_status () = (Unix.stdin, Poll.init_event ())

let spec_poll_status () =
  Poll.{read= true; write= false; can_read= false; can_write= false}

let add_anonymous cons fd =
  let capacity = get_capacity () in
  let xbcon = Xenbus.Xb.open_fd fd ~capacity in
  let con = Connection.create xbcon None in
  Hashtbl.replace cons.anonymous fd (con, Array.length cons.poll_status) ;
  (* SAFETY: con.poll_status is always Some for anonymous connections *)
  cons.poll_status <-
    Array.append cons.poll_status [|(fd, Option.get con.poll_status)|]

let add_domain cons dom =
  let capacity = get_capacity () in
  let xbcon =
    Xenbus.Xb.open_mmap ~capacity (Domain.get_interface dom) (fun () ->
        Domain.notify dom
    )
  in
  let con = Connection.create xbcon (Some dom) in
  Hashtbl.replace cons.domains (Domain.get_id dom) con ;
  Hashtbl.replace cons.ports (Domain.get_local_port dom) con

let find cons fd =
  let c, _ = Hashtbl.find cons.anonymous fd in
  c

let find_domain cons = Hashtbl.find cons.domains

let find_domain_by_port cons port = Hashtbl.find cons.ports port

let del_watches_of_con con watches =
  match List.filter (fun w -> Connection.get_con w != con) watches with
  | [] ->
      None
  | ws ->
      Some ws

let del_watches cons (con : Connection.t) =
  if con.nb_watches > 0 then (
    Connection.del_watches con ;
    cons.watches <- Trie.map (del_watches_of_con con) cons.watches
  )

(* Reallocate the poll_status array, update indices pointing to it *)
let resize_anonymous cons spec_fds =
  cons.poll_status <-
    Array.make
      (Hashtbl.length cons.anonymous + List.length spec_fds)
      (default_poll_status ())
  (* TODO: Work around an unnecessary allocation here *) ;

  (* Keep the special fds at the beginning *)
  let i =
    List.fold_left
      (fun index fd ->
        cons.poll_status.(index) <- (fd, spec_poll_status ()) ;
        index + 1
      )
      0 spec_fds
  in

  let _ =
    Hashtbl.fold
      (fun key (con, _) i ->
        Hashtbl.replace cons.anonymous key (con, i) ;
        (* SAFETY: con.poll_status is always Some for anonymous connections *)
        cons.poll_status.(i) <-
          (Connection.get_fd con, Option.get con.poll_status) ;
        i + 1
      )
      cons.anonymous i
  in
  ()

(* SAFETY: If del_anonymous is called with resize=false, then resize_anonymous
   must be called to shrink the array to avoid runaway memory usage *)
let del_anonymous ?(resize = true) cons con spec_fds =
  try
    Hashtbl.remove cons.anonymous (Connection.get_fd con) ;
    del_watches cons con ;
    Connection.close con ;

    (* Several connections can be removed one after another. There is no point
       in resizing the array after each one. Wait for the last one, then resize.
       cons.anonymous Hashtbl is still correct without resizing *)
    if resize then
      resize_anonymous cons spec_fds
  with exn -> debug "del anonymous %s" (Printexc.to_string exn)

let del_domain cons id =
  try
    let con = find_domain cons id in
    Hashtbl.remove cons.domains id ;
    ( match Connection.get_domain con with
    | Some d ->
        Hashtbl.remove cons.ports (Domain.get_local_port d)
    | None ->
        ()
    ) ;
    del_watches cons con ; Connection.close con
  with exn -> debug "del domain %u: %s" id (Printexc.to_string exn)

let iter_domains cons fct = Hashtbl.iter (fun _ c -> fct c) cons.domains

let iter_anonymous cons fct =
  Hashtbl.iter (fun _ (c, _) -> fct c) cons.anonymous

let iter cons fct = iter_domains cons fct ; iter_anonymous cons fct

let has_more_work cons =
  Hashtbl.fold
    (fun _id con acc -> if Connection.has_more_work con then con :: acc else acc)
    cons.domains []

let key_of_str path =
  if path.[0] = '@' then
    [path]
  else
    "" :: Store.Path.to_string_list (Store.Path.of_string path)

let key_of_path path = "" :: Store.Path.to_string_list path

let add_watch cons con path token =
  let apath = Connection.get_watch_path con path in
  (* fail on invalid paths early by calling key_of_str before adding watch *)
  let key = key_of_str apath in
  let watch = Connection.add_watch con (path, apath) token in
  let watches =
    if Trie.mem cons.watches key then
      Trie.find cons.watches key
    else
      []
  in
  cons.watches <- Trie.set cons.watches key (watch :: watches) ;
  watch

let del_watch cons con path token =
  let apath, watch = Connection.del_watch con path token in
  let key = key_of_str apath in
  let watches = Utils.list_remove watch (Trie.find cons.watches key) in
  if watches = [] then
    cons.watches <- Trie.unset cons.watches key
  else
    cons.watches <- Trie.set cons.watches key watches ;
  watch

(* path is absolute *)
let fire_watches ?oldroot source root cons path recurse =
  let key = key_of_path path in
  let path = Store.Path.to_string path in
  let roots = (oldroot, root) in
  let fire_watch _ = function
    | None ->
        ()
    | Some watches ->
        List.iter (fun w -> Connection.fire_watch source roots w path) watches
  in
  let fire_rec _x = function
    | None ->
        ()
    | Some watches ->
        List.iter (Connection.fire_single_watch source roots) watches
  in
  Trie.iter_path fire_watch cons.watches key ;
  if recurse then
    Trie.iter fire_rec (Trie.sub cons.watches key)

let send_watchevents con = Connection.source_flush_watchevents con

let fire_spec_watches root cons specpath =
  let source = find_domain cons 0 in
  iter cons (fun con ->
      List.iter
        (Connection.fire_single_watch source (None, root))
        (Connection.get_watches con specpath)
  )

let set_target cons domain target_domain =
  let con = find_domain cons domain in
  Connection.set_target con target_domain

let number_of_transactions cons =
  let res = ref 0 in
  let aux con = res := Connection.number_of_transactions con + !res in
  iter cons aux ; !res

let stats cons =
  let nb_ops_anon = ref 0
  and nb_watchs_anon = ref 0
  and nb_ops_dom = ref 0
  and nb_watchs_dom = ref 0 in
  iter_anonymous cons (fun con ->
      let con_watchs, con_ops = Connection.stats con in
      nb_ops_anon := !nb_ops_anon + con_ops ;
      nb_watchs_anon := !nb_watchs_anon + con_watchs
  ) ;
  iter_domains cons (fun con ->
      let con_watchs, con_ops = Connection.stats con in
      nb_ops_dom := !nb_ops_dom + con_ops ;
      nb_watchs_dom := !nb_watchs_dom + con_watchs
  ) ;
  ( Hashtbl.length cons.anonymous
  , !nb_ops_anon
  , !nb_watchs_anon
  , Hashtbl.length cons.domains
  , !nb_ops_dom
  , !nb_watchs_dom
  )

let debug cons =
  let anonymous =
    Hashtbl.fold
      (fun _ (con, _) accu -> Connection.debug con :: accu)
      cons.anonymous []
  in
  let domains =
    Hashtbl.fold
      (fun _ con accu -> Connection.debug con :: accu)
      cons.domains []
  in
  String.concat "" (domains @ anonymous)

let filter ~f cons =
  let fold _ v acc = if f v then v :: acc else acc in
  let fold_a _ (v, _) acc = if f v then v :: acc else acc in
  [] |> Hashtbl.fold fold_a cons.anonymous |> Hashtbl.fold fold cons.domains

let prevents_quit cons = filter ~f:Connection.prevents_live_update cons
