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

open Printf
open Stdext

exception ConversionFailed of string

let int_of_string_exn ?(unsigned = false) x =
  match int_of_string_opt x with
  | Some i when unsigned && i < 0 ->
      raise (ConversionFailed x)
  | None ->
      raise (ConversionFailed x)
  | Some i ->
      i

(* lists utils *)
let filter_out filter l = List.filter (fun x -> not (List.mem x filter)) l

let filter_in filter l = List.filter (fun x -> List.mem x filter) l

let list_remove element l = List.filter (fun e -> e != element) l

let list_tl_multi n l =
  let rec do_tl i x = if i = 0 then x else do_tl (i - 1) (List.tl x) in
  do_tl n l

(* string utils *)
let get_hierarchy path =
  let l = List.length path in
  let revpath = List.rev path in
  let rec sub i =
    let x = List.rev (list_tl_multi (l - i) revpath) in
    if i = l then [x] else x :: sub (i + 1)
  in
  sub 0

let hexify s =
  let hexseq_of_char c = sprintf "%02x" (Char.code c) in
  let hs = Bytes.create (String.length s * 2) in
  String.iteri
    (fun i c ->
      let seq = hexseq_of_char c in
      Bytes.set hs (i * 2) seq.[0] ;
      Bytes.set hs ((i * 2) + 1) seq.[1]
    )
    s ;
  Bytes.unsafe_to_string hs

let unhexify hs =
  let char_of_hexseq seq0 seq1 =
    Char.chr (int_of_string_exn (sprintf "0x%c%c" seq0 seq1))
  in
  let b = Bytes.create (String.length hs / 2) in
  for i = 0 to Bytes.length b - 1 do
    Bytes.set b i (char_of_hexseq hs.[i * 2] hs.[(i * 2) + 1])
  done ;
  Bytes.unsafe_to_string b

let trim_path path =
  try
    let rindex = String.rindex path '/' in
    String.sub path 0 rindex
  with Not_found -> ""

let join_by_null ls = String.concat "\000" ls

(* unix utils *)
let create_unix_socket name =
  Unixext.unlink_safe name ;
  Unixext.mkdir_rec (Filename.dirname name) 0o700 ;
  let sockaddr = Unix.ADDR_UNIX name in
  let sock = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Unix.bind sock sockaddr ; Unix.listen sock 1 ; sock

let read_file_single_integer filename =
  let fd = Unix.openfile filename [Unix.O_RDONLY] 0o640 in
  let buf = Bytes.make 20 '\000' in
  let sz = Unix.read fd buf 0 20 in
  Unix.close fd ;
  int_of_string_exn (Bytes.sub_string buf 0 sz)

(* @path may be guest data and needs its length validating.  @connection_path
 * is generated locally in xenstored and always of the form "/local/domain/$N/" *)
let path_validate path connection_path =
  let len = String.length path in

  if len = 0 then raise Define.Invalid_path ;

  let abs_path =
    match String.get path 0 with
    | '/' | '@' ->
        path
    | _ ->
        connection_path ^ path
  in

  (* Regardless whether client specified absolute or relative path,
     	   canonicalize it (above) and, for domain-relative paths, check the
     	   length of the relative part.

     	   This prevents paths becoming invalid across migrate when the length
     	   of the domid changes in @param connection_path.
  *)
  let len = String.length abs_path in
  let on_absolute _ _ = len in
  let on_relative _ offset = len - offset in
  let len =
    Scanf.ksscanf abs_path on_absolute "/local/domain/%d/%n" on_relative
  in
  if len > !Define.path_max then raise Define.Invalid_path ;

  abs_path

module FD : sig
  type t = Unix.file_descr

  val of_int : int -> t

  val to_int : t -> int
end = struct
  type t = Unix.file_descr

  (* This is like Obj.magic but just for these types,
     	   and relies on Unix.file_descr = int *)
  external to_int : t -> int = "%identity"

  external of_int : int -> t = "%identity"
end
