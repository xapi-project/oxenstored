(*
 * Copyright (C) 2008-2009 Citrix Ltd.
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

(** Basic Implementation of polymorphic tries (ie. prefix trees) *)

(** The type of tries. ['a] the type of values.
    	Internally, a trie is represented as a labeled tree, where node contains values
    	of type [string * 'a option]. *)
type 'a t

val create : unit -> 'a t
(** Creates an empty trie. *)

val mem : 'a t -> string list -> bool
(** [mem t k] returns true if a value is associated with the key [k] in the trie [t].
    	Otherwise, it returns false. *)

val find : 'a t -> string list -> 'a
(** [find t k] returns the value associated with the key [k] in the trie [t].
    	Returns [Not_found] if no values are associated with [k] in [t]. *)

val set : 'a t -> string list -> 'a -> 'a t
(** [set t k v] associates the value [v] with the key [k] in the trie [t]. *)

val unset : 'a t -> string list -> 'a t
(** [unset k v] removes the association of value [v] with the key [k] in the trie [t].
    	Moreover, it automatically clean the trie, ie. it removes recursively
    	every nodes of [t] containing no values and having no chil. *)

val iter : (string -> 'a option -> unit) -> 'a t -> unit
(** [iter f t] applies the function [f] to every node of the trie [t].
    	As nodes of the trie [t] do not necessary contains a value, the second argument of
    	[f] is an option type. *)

val iter_path : (string -> 'a option -> unit) -> 'a t -> string list -> unit
(** [iter_path f t p] iterates [f] over nodes associated with the path [p] in the trie [t].
    	If [p] is not a valid path of [t], it iterates on the longest valid prefix of [p]. *)

val fold : (string -> 'a option -> 'c -> 'c) -> 'a t -> 'c -> 'c
(** [fold f t x] fold [f] over every nodes of [t], with [x] as initial value. *)

val map : ('a -> 'b option) -> 'a t -> 'b t
(** [map f t] maps [f] over every values stored in [t]. The return value of [f] is of type 'c option
    	as one may wants to remove value associated to a key. This function is not tail-recursive. *)

val sub : 'a t -> string list -> 'a t
(** [sub t p] returns the sub-trie associated with the path [p] in the trie [t].
    	If [p] is not a valid path of [t], it returns an empty trie. *)
