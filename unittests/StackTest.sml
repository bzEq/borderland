(* Copyright (c) 2018 Kai Luo <gluokai@gmail.com>. All rights reserved. *)

structure StackTest = struct
open Testing

fun testPushAndTop () = let
    val s = ref Stack.empty
in
    s := Stack.push (!s) 1;
    s := Stack.push (!s) 2;
    assertTrue ((Stack.top (!s)) = 2)
end

fun testPushAndPop () = let
    val s = ref Stack.empty
in
    s := Stack.push (!s) 1;
    s := Stack.push (!s) 2;
    s := Stack.pop (!s);
    s := Stack.pop (!s);
    assertTrue (Stack.isEmpty (!s))
end


end

val _ = Testing.addTest "StackTest.testPushAndTop" StackTest.testPushAndTop
val _ = Testing.addTest "StackTest.testPushAndPop" StackTest.testPushAndPop
