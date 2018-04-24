(* Copyright (c) 2018 Kai Luo <gluokai@gmail.com>. All rights reserved. *)

functor SplaySetFn(K : COMPARABLE) : ORD_SET = struct

structure Key = K

structure Set = SplayMapFn (Key)

type set = unit Set.map

val empty : set = Set.empty

val isEmpty = Set.isEmpty

fun add (s : set) x = Set.put s (x, ())

fun contains (s : set) x =
    case (Set.get s x) of
        NONE => false
      | _ => true

fun remove (s : set) x =
    case (Set.remove s x) of
        NONE => s
      | SOME (s', _) => s'

end
