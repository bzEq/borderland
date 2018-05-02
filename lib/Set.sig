(* Copyright (c) 2018 Kai Luo <gluokai@gmail.com>. All rights reserved. *)

signature SET = sig

type elem

type set

val empty : set

val isEmpty : set -> bool

val add : set -> elem -> set

val contains : set -> elem -> bool

val remove : set -> elem -> set

end
