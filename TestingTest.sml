(* Copyright (c) 2018 Kai Luo <gluokai@gmail.com>. All rights reserved. *)

structure TestingTest = struct
fun sayHi () = print "Hi, Testing!\n"

end

val _ = Testing.addTest TestingTest.sayHi
