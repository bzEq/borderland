(* Copyright (c) 2018 Kai Luo <gluokai@gmail.com>. All rights reserved. *)

structure Utf8DecoderTest = struct

val utf8txt = "unittests/utf8.txt"

val gbktxt = "unittests/gbk.txt"

fun GbkTest () = let
    val s = Support.readFromFile gbktxt
    val x = Utf8Decoder.countCodePoints s
in
    Testing.assertFalse (Option.isSome x)
end

fun Utf8Test () = let
    val s = Support.readFromFile utf8txt
    val x = Utf8Decoder.countCodePoints s
in
    Testing.assertTrue (Option.isSome x);
    Testing.assertTrue ((Option.valOf x) = 703)
end

end

val _ = Testing.addTest "Utf8DecoderTest.Utf8Test" Utf8DecoderTest.Utf8Test

val _ = Testing.addTest "Utf8DecoderTest.GbkTest" Utf8DecoderTest.GbkTest
