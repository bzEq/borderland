(* Copyright (c) 2018 Kai Luo <gluokai@gmail.com>. All rights reserved. *)

structure Testing = struct

fun assertTrue exp =
    if exp then
      ()
    else (
      raise Fail "Assertion failed"
    )

fun assertFalse exp =
    if exp then (
      raise Fail "Assertion failed"
    )
    else
      ()

val tests : (unit -> unit) list ref = ref []

fun addTest test = tests := !tests @ [test]

fun runAll () = let
  fun loop [] = ()
    | loop (test::rest) = (
      test ();
      loop rest
    )
in
  print ("Number of tests: " ^ (Int.toString (List.length (!tests))) ^ "\n");
  loop (!tests)
end

end
