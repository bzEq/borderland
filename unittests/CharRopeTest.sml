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

fun testSplay () = let
    val str = "Copyright (c) 2018 Kai Luo <gluokai@gmail.com>. All rights reserved."
    val s =
        CharRope.toString (
            CharRope.splay
                (CharRope.init (Substring.full str))
                0
        )
in
    assertTrue (s = str)
end

fun testSplay1 () = let
    val str = "Copyright (c) 2018 Kai Luo <gluokai@gmail.com>. All rights reserved."
    val s =
        CharRope.toString (
            CharRope.splay
                (CharRope.init (Substring.full str))
                ((String.size str) - 1)
        )
in
    assertTrue (s = str)
end

fun testSplay2 () = let
    val str = "Copyright (c) 2018 Kai Luo <gluokai@gmail.com>. All rights reserved."
    fun splayIndex index =
        CharRope.toString (
            CharRope.splay
                (CharRope.init (Substring.full str))
                index
        )
    val k = ref 0
    val l = String.size str
in
    while (!k < l) do (
        assertTrue ((splayIndex (!k)) = str);
        k := !k +1
    )
end

fun testSplay3 () = let
    val str = "Copyright (c) 2018 Kai Luo <gluokai@gmail.com>. All rights reserved."
    val k = ref 0
    val l = String.size str
    val root = ref (CharRope.init (Substring.full str))
in
    while (!k < l) do (
        root := CharRope.splay (!root) (!k);
        assertTrue ((CharRope.toString (!root)) = str);
        k := !k +1
    )
end

fun testSlice () = let
    val str = "Copyright (c) 2018 Kai Luo <gluokai@gmail.com>. All rights reserved."
    val r = CharRope.init (Substring.full str)
    val r' = CharRope.slice (r, 6, NONE)
    val str' = CharRope.toString r'
in
    assertTrue (str' = "ght (c) 2018 Kai Luo <gluokai@gmail.com>. All rights reserved.")
end

fun testSlice1 () = let
    val str = "Copyright (c) 2018 Kai Luo <gluokai@gmail.com>. All rights reserved."
    val r = CharRope.init (Substring.full str)
    val l = String.size str
    val i = ref 0
    val j = ref 0
in
    while (!i < l) do (
        j := 0;
        while ((!j) <= (l - (!i))) do (
            let
                val r' = CharRope.slice (r, (!i), SOME (!j))
                val str' = CharRope.toString r'
            in
                assertTrue (str' = String.extract (str, (!i), SOME (!j)))
            end;
            j := (!j) + 1
        );
        i := (!i) + 1
    )
end


end

val _ = Testing.addTest "CharRopeTest.testInit" CharRopeTest.testInit
val _ = Testing.addTest "CharRopeTest.testConcat" CharRopeTest.testConcat
val _ = Testing.addTest "CharRopeTest.testSplay" CharRopeTest.testSplay
val _ = Testing.addTest "CharRopeTest.testSplay1" CharRopeTest.testSplay1
val _ = Testing.addTest "CharRopeTest.testSplay2" CharRopeTest.testSplay2
val _ = Testing.addTest "CharRopeTest.testSplay3" CharRopeTest.testSplay3
val _ = Testing.addTest "CharRopeTest.testSlice" CharRopeTest.testSlice
val _ = Testing.addTest "CharRopeTest.testSlice1" CharRopeTest.testSlice1
