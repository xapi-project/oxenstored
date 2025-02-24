(*
 * Copyright (C) 2014 Zheng Li <dev@zheng.li>
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

(* The [read], [write] are fields mapped to the POLLIN/OUT
   subscription flags used by poll, which have a correspondence to the
   readfds, writefds concept as in select.
   [can_read], [can_write] correspond to the same flags in [revents]
*)
type event = {
    mutable read: bool
  ; mutable write: bool
  ; mutable can_read: bool
  ; mutable can_write: bool
}

let init_event () =
  {read= false; write= false; can_read= false; can_write= false}

external select_on_poll : (Unix.file_descr * event) array -> int -> int
  = "stub_select_on_poll"

external set_fd_limit : int -> unit = "stub_set_fd_limit"

(* The rlim_max given to setrlimit must not go above the system level nr_open,
   which we can read from /proc/sys. *)
let get_sys_fs_nr_open () =
  try
    let ch = open_in "/proc/sys/fs/nr_open" in
    let v = Utils.int_of_string_exn (input_line ch) in
    close_in_noerr ch ; v
  with _ -> 1024 * 1024

let poll_select fdarr timeout =
  let n = select_on_poll fdarr (int_of_float (timeout *. 1000.)) in
  let r = ([], []) in
  if n = 0 then
    r
  else
    Array.fold_right
      (fun (fd, event) (r, w) ->
        ( (if event.can_read then fd :: r else r)
        , if event.can_write then fd :: w else w
        )
      )
      fdarr r

let () = set_fd_limit (get_sys_fs_nr_open ())
