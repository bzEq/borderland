(* Copyright (c) 2018 Kai Luo <gluokai@gmail.com>. All rights reserved. *)

signature ORD_SET = sig

structure Key : COMPARABLE

type set

val empty : set

val isEmpty : set -> bool

val add : set -> Key.t -> set

val contains : set -> Key.t -> bool

val remove : set -> Key.t -> set

end
