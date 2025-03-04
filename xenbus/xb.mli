module Op : sig
  type operation = Op.operation =
    | Debug
    | Directory
    | Read
    | Getperms
    | Watch
    | Unwatch
    | Transaction_start
    | Transaction_end
    | Introduce
    | Release
    | Getdomainpath
    | Write
    | Mkdir
    | Rm
    | Setperms
    | Watchevent
    | Error
    | Isintroduced
    | Resume
    | Set_target
    | Reset_watches
    | Directory_part
    | Invalid

  val operation_c_mapping : operation array

  val size : int

  val array_search : 'a -> 'a array -> int

  val of_cval : int -> operation

  val to_cval : operation -> int

  val to_string : operation -> string
end

module Packet : sig
  type t = Packet.t = {tid: int; rid: int; ty: Op.operation; data: string}

  exception Error of string

  exception DataError of string

  external string_of_header : int -> int -> int -> int -> string
    = "stub_string_of_header"

  val create : int -> int -> Op.operation -> string -> t

  val of_partialpkt : Partial.pkt -> t

  val to_string : t -> string

  val unpack : t -> int * int * Op.operation * string

  val get_tid : t -> int

  val get_ty : t -> Op.operation

  val get_data : t -> string

  val get_rid : t -> int
end

exception End_of_file

exception Eagain

exception Noent

exception Invalid

exception Reconnect

type backend_mmap = {
    mmap: Xenmmap.t
  ; eventchn_notify: unit -> unit
  ; mutable work_again: bool
}

type backend_fd = {fd: Unix.file_descr}

type backend = Fd of backend_fd | Xenmmap of backend_mmap

type partial_buf = HaveHdr of Partial.pkt | NoHdr of int * bytes

type capacity = {maxoutstanding: int; maxwatchevents: int}

type t

val init_partial_in : unit -> partial_buf

val reconnect : t -> unit

val queue : t -> Packet.t -> unit option

val read_fd : backend_fd -> 'a -> bytes -> int -> int

val read_mmap : backend_mmap -> 'a -> bytes -> int -> int

val read : t -> bytes -> int -> int

val write_fd : backend_fd -> 'a -> string -> int -> int

val write_mmap : backend_mmap -> 'a -> string -> int -> int

val write : t -> string -> int -> int

val output : t -> bool

val input : t -> Packet.t option

val newcon : capacity:capacity -> backend -> t

val open_fd : Unix.file_descr -> capacity:capacity -> t

val open_mmap :
  Xenmmap.t -> (unit -> unit) -> under_testing:bool -> capacity:capacity -> t

val close : t -> under_testing:bool -> unit

val is_fd : t -> bool

val is_mmap : t -> bool

val output_len : t -> int

val can_input : t -> bool

val has_new_output : t -> bool

val has_old_output : t -> bool

val has_output : t -> bool

val peek_output : t -> Packet.t

val has_partial_input : t -> bool

val has_more_input : t -> bool

val is_selectable : t -> bool

val get_fd : t -> Unix.file_descr

val debug : t -> string

(**/**)

(* WARNING: Functions below this point are to be used in testing only *)

val unsafe_pop_output : t -> Packet.t
(** UNSAFE: Only use this in unit tests *)
