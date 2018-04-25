(* Copyright (c) 2018 Kai Luo <gluokai@gmail.com>. All rights reserved. *)
(*
 * Copyright (c) 2008-2009 Bjoern Hoehrmann <bjoern@hoehrmann.de>
 * See http://bjoern.hoehrmann.de/utf-8/decoder/dfa/ for details.
 *)

structure Utf8Decoder = struct

val accept = 0 and reject = 1

local
    val state = [
            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, (* 00..1f *)
            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, (* 20..3f *)
            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, (* 40..5f *)
            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, (* 60..7f *)
            1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9, (* 80..9f *)
            7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7, (* a0..bf *)
            8,8,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2, (* c0..df *)
            0xa,0x3,0x3,0x3,0x3,0x3,0x3,0x3,0x3,0x3,0x3,0x3,0x3,0x4,0x3,0x3, (* e0..ef *)
            0xb,0x6,0x6,0x6,0x5,0x8,0x8,0x8,0x8,0x8,0x8,0x8,0x8,0x8,0x8,0x8, (* f0..ff *)
            0x0,0x1,0x2,0x3,0x5,0x8,0x7,0x1,0x1,0x1,0x4,0x6,0x1,0x1,0x1,0x1, (* s0..s0 *)
            1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,1,1,1,0,1,0,1,1,1,1,1,1, (* s1..s2 *)
            1,2,1,1,1,1,1,2,1,2,1,1,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1,1, (* s3..s4 *)
            1,2,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1,1,1,1,1,1,1,3,1,3,1,1,1,1,1,1, (* s5..s6 *)
            1,3,1,1,1,1,1,3,1,3,1,1,1,1,1,1,1,3,1,1,1,1,1,1,1,1,1,1,1,1,1,1  (* s7..s8 *)
        ]
in
val utf8d = Array.fromList (List.map (fn x => Word8.fromInt (x)) state)
end

fun decode (state : Word32.word, codepoint : Word32.word , byte : Word32.word) = let
    val ty : Word32.word = Word32.fromInt (Word8.toInt (Array.sub (utf8d, (Word32.toInt byte))))
    val codepoint' =
        if (Word32.toInt state) <> accept then
            Word32.orb ((Word32.andb (byte, 0wx3f)), (Word32.<< (codepoint, 0w6)))
        else
            Word32.andb (Word32.>> (Word32.fromLarge 0wxff,  ty), byte)
    val state' = Word32.fromInt (Word8.toInt (Array.sub (utf8d, Word32.toInt ((state * 0w16) + ty + 0w256))))
in
    (state', codepoint')
end

fun countCodePoints s = let
    val s' = Substring.full s
    fun parseAcc (result as (state, codepoint, count)) sub = (
        case (Substring.getc sub) of
            NONE => result
          | SOME (c, rest) => let
              val (state', codepoint') =
                          decode (state, codepoint, Word32.fromInt (Char.ord c))
          in
              if (Word32.toInt state') = accept then
                  parseAcc (state', codepoint', count + 1) rest
              else
                  parseAcc (state', codepoint', count) rest
          end
    )
    val (state, codepoint, count) =
        parseAcc (Word32.fromInt 0, Word32.fromInt 0, 0) s'
in
    if (Word32.toInt state) = accept then
        SOME count
    else
        NONE
end

end
