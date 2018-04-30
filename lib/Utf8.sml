(* Copyright (c) 2018 Kai Luo <gluokai@gmail.com>. All rights reserved. *)

structure Utf8 :> CHARSET = struct
open Utf8Decoder

type t = utf8char

type substring = utf8char ArraySlice.slice

val isEmpty = ArraySlice.isEmpty

val length = ArraySlice.length

val sub = ArraySlice.sub

val slice = ArraySlice.subslice

fun toString s =
    ArraySlice.foldr
        (fn (ch : t, acc) => (Substring.string (#slice ch)) ^ acc)
        ""
        s

end
