(* Copyright (c) 2018 Kai Luo <gluokai@gmail.com>. All rights reserved. *)

structure CharRope = RopeFn(
    struct
    type t = char
    type substring = Substring.substring
    val isEmpty = Substring.isEmpty
    val length = Substring.size
    val sub = Substring.sub
    val slice = Substring.slice
    val toString = Substring.string
    end
)
