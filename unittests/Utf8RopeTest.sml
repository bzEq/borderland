(* Copyright (c) 2018 Kai Luo <gluokai@gmail.com>. All rights reserved. *)

structure Utf8RopeTest = struct
open Testing
open Support

fun toArraySlice s =
    case (Utf8Decoder.toUtf8String s) of
        NONE => raise Fail "It's not a valid utf8 string"
      | SOME str => (ArraySlice.full (Array.fromList str))

fun testInit () = let
    val s = readFromFile "unittests/utf8.txt"
    val r = Utf8Rope.init (toArraySlice s)
in
    assertTrue ((Utf8Rope.length r) = 703);
    assertTrue ((Utf8Rope.toString r) = s)
end

end

val _ = Testing.addTest "Utf8RopeTest.testInit" Utf8RopeTest.testInit
