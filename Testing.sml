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

val tests : (string * (unit -> unit)) list ref = ref []

fun addTest desc test = tests := (desc, test)::(!tests)

fun runAll () = let
  fun loop [] = ()
    | loop ((desc, test)::rest) = (
        Support.println ("Running " ^ desc);
        test ();
        loop rest
    )
in
  print ("Number of tests: " ^ (Int.toString (List.length (!tests))) ^ "\n");
  loop (List.rev (!tests))
end

end
