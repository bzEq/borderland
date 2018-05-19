(* Copyright (c) 2018 Kai Luo <gluokai@gmail.com>. All rights reserved. *)

structure Stack = struct

type 'a stack = 'a list

exception EmptyStack

val empty = []

fun push s x = x::s

fun isEmpty [] = true
  | isEmpty _ = false

fun top [] = raise EmptyStack
  | top (t::_) = t

fun pop [] = raise EmptyStack
  | pop (_::rest) = rest

end
