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

type t

type mmap_interface

type mmap_prot_flag = RDONLY | WRONLY | RDWR

type mmap_map_flag = SHARED | PRIVATE

val mmap : Unix.file_descr -> mmap_prot_flag -> mmap_map_flag -> int -> int -> t

val unmap : t -> unit

val make : ?unmap:(mmap_interface -> unit) -> mmap_interface -> t

val to_interface : t -> mmap_interface

external getpagesize : unit -> int = "stub_mmap_getpagesize"
