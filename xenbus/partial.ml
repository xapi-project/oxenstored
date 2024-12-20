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

type pkt = {tid: int; rid: int; ty: Op.operation; len: int; buf: Buffer.t}

external header_size : unit -> int = "stub_header_size"

external header_of_string_internal : string -> int * int * int * int
  = "stub_header_of_string"

let xenstore_payload_max = 4096 (* xen/include/public/io/xs_wire.h *)

let xenstore_rel_path_max = 2048 (* xen/include/public/io/xs_wire.h *)

let of_string s =
  let tid, rid, opint, dlen = header_of_string_internal s in
  (* A packet which is bigger than xenstore_payload_max is illegal.
     	   This will leave the guest connection is a bad state and will
     	   be hard to recover from without restarting the connection
     	   (ie rebooting the guest) *)
  let dlen = max 0 (min xenstore_payload_max dlen) in
  {tid; rid; ty= Op.of_cval opint; len= dlen; buf= Buffer.create dlen}

let append pkt s sz =
  if Buffer.length pkt.buf + sz > xenstore_payload_max then
    failwith "Buffer.add: cannot grow buffer" ;
  Buffer.add_substring pkt.buf s 0 sz

let to_complete pkt = pkt.len - Buffer.length pkt.buf
