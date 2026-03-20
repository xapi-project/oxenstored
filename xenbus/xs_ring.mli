module Server_feature : sig
  type t = Reconnection | Error_indicator | Watch_depth

  val to_string : t -> string
end

module Server_features : module type of Set.Make (struct
  type t = Server_feature.t

  let compare = compare end)

val read : Xenmmap.mmap_interface -> bytes -> int -> int

val write_substring : Xenmmap.mmap_interface -> string -> int -> int

val get_server_features : Xenmmap.mmap_interface -> Server_features.t

val set_server_features : Xenmmap.mmap_interface -> Server_features.t -> unit

val close : Xenmmap.mmap_interface -> unit
