(* Copyright (c) 2018 Kai Luo <gluokai@gmail.com>. All rights reserved. *)
(* ord-map-sig.sml
 *
 * COPYRIGHT (c) 2012 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * COPYRIGHT (c) 1996 by AT&T Research.  See COPYRIGHT file for details.
 *
 * Abstract signature of an applicative-style finite maps (dictionaries)
 * structure over ordered monomorphic keys.
 *)

signature MAP = sig

type key_t

type 'a map

val empty : 'a map

val isEmpty : 'a map -> bool

val put : 'a map -> (key_t * 'a) -> 'a map

val get : 'a map -> key_t -> 'a option

val contains : 'a map -> key_t -> bool

val remove : 'a map -> key_t -> ('a map * 'a) option

end
