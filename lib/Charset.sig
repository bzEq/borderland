(* Copyright (c) 2018 Kai Luo <gluokai@gmail.com>. All rights reserved. *)

signature CHARSET = sig
type t
type substring

val isEmpty : substring -> bool

val length : substring -> int

val sub : substring * int -> t

val slice : substring * int * int option -> substring

val toString : substring -> string


end
