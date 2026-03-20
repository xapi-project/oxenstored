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

module Server_feature = struct
  type t = Reconnection | Error_indicator | Watch_depth

  (* See xenstore-ring.txt for the required order *)
  let bit_mapping = [|Reconnection; Error_indicator; Watch_depth|]

  let bitmap_size = Array.length bit_mapping

  (* See xenstore-ring.txt for bit numbers *)
  let feature_to_bit = function
    | Reconnection ->
        0
    | Error_indicator ->
        1
    | Watch_depth ->
        2

  let to_string = function
    | Reconnection ->
        "Reconnection"
    | Error_indicator ->
        "Error_indicator"
    | Watch_depth ->
        "Watch_depth"
end

module Server_features = Set.Make (struct
  type t = Server_feature.t

  let compare = compare
end)

(* Set of features from bitmask *)
let of_cval n =
  let features = ref Server_features.empty in
  for i = 0 to Server_feature.bitmap_size - 1 do
    if (n lsr i) land 1 = 1 then
      let feature = Server_feature.bit_mapping.(i) in
      features := Server_features.add feature !features
  done ;
  !features

(* Bitmask from set of features *)
let to_cval features =
  Server_features.fold
    (fun feature bitmap ->
      let i = Server_feature.feature_to_bit feature in
      bitmap lor (1 lsl i)
    )
    features 0

external read : Xenmmap.mmap_interface -> bytes -> int -> int
  = "ml_interface_read"

external write_substring : Xenmmap.mmap_interface -> string -> int -> int
  = "ml_interface_write"

external _internal_set_server_features : Xenmmap.mmap_interface -> int -> unit
  = "ml_interface_set_server_features"
[@@noalloc]

external _internal_get_server_features : Xenmmap.mmap_interface -> int
  = "ml_interface_get_server_features"
[@@noalloc]

let get_server_features mmap =
  let x = _internal_get_server_features mmap in
  of_cval x

let set_server_features mmap set =
  let x = to_cval set in
  _internal_set_server_features mmap x

external close : Xenmmap.mmap_interface -> unit = "ml_interface_close"
[@@noalloc]
