(* Copyright (c) 2018 Kai Luo <gluokai@gmail.com>. All rights reserved. *)

structure SplayTreeTest = struct
open SplayTree

fun countNode NIL = 0
  | countNode (NODE {left=left, right=right, ...}) =
    (countNode left) + (countNode right) + 1

fun testSplay () = let
  fun compare a b = Int.compare (a, b)
  fun compare' x = compare 7 x
  val tree  = NODE {
        value = 5,
        left = NODE {
          value = 3,
          left = NODE {
            value = 2,
            left = NIL,
            right = NIL
          },
          right = NIL
        },
        right = NODE {
          value = 8,
          left = NODE {
            value = 7,
            left = NIL,
            right = NIL
          },
          right = NODE {
            value = 9,
            left = NIL,
            right = NIL
          }
        }
      }
  val root = splay compare' tree
in
  case root of
      NIL => raise Fail "Can't be nil"
   |  NODE s => (
     Testing.assertFalse ((#value s) = 8);
     Testing.assertTrue ((#value s) = 7);
     Testing.assertTrue ((countNode root) = 6)
   )
end

end

val _ = Testing.addTest "SplayTreeTest.testSplay"
                        SplayTreeTest.testSplay
