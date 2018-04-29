(* Copyright (c) 2018 Kai Luo <gluokai@gmail.com>. All rights reserved. *)

structure CharRopeTest = struct
open Testing
open Support

fun testInit () = let
    val s = CharRope.init
                (Substring.full "Copyright (c) 2018 Kai Luo <gluokai@gmail.com>. All rights reserved.")
in
    assertTrue ((CharRope.toString s) = "Copyright (c) 2018 Kai Luo <gluokai@gmail.com>. All rights reserved.")
end

fun testConcat () = let
    val s = CharRope.init (Substring.full "Copyright (c) 2018 Kai Luo <gluokai@gmail.com>. ")
    val t = CharRope.init (Substring.full "All rights reserved.")
    val r = CharRope.toString (CharRope.concat s t)
in
    assertTrue
        (r = "Copyright (c) 2018 Kai Luo <gluokai@gmail.com>. All rights reserved.")
end

end

val _ = Testing.addTest "CharRopeTest.testInit" CharRopeTest.testInit
val _ = Testing.addTest "CharRopeTest.testConcat" CharRopeTest.testConcat
