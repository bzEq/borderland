(* Copyright (c) 2018 Kai Luo <gluokai@gmail.com>. All rights reserved. *)

functor SplayMapFn (K : COMPARABLE) : ORD_MAP = struct
structure Key = K

open SplayTree

datatype 'a map = EMPTY
                | MAP of {
                    root : (Key.t * 'a) tree ref
                }

val empty = EMPTY

fun isEmpty EMPTY = true
  | isEmpty (MAP {root=ref NIL}) = true
  | isEmpty _ = false

fun get EMPTY _ = NONE
  | get (MAP {root=ref NIL}) _ = NONE
  | get (MAP (rootRef as {root=ref node})) key = let
      fun compare (x:(Key.t * 'a)) = Key.compare key (#1 x)
      val newRoot = splay compare node
      val () = (#root rootRef) := newRoot
  in
      case newRoot of
          NIL => raise Fail "Splay non-nil should not return nil"
       | NODE {value, left, right} => (
           case (compare value) of
               EQUAL => SOME (#2 value)
             | _ => NONE
       )
  end

fun put EMPTY new = MAP {
        root = ref (NODE {
                         value = new,
                         left = NIL,
                         right = NIL
                   })
    }
  | put (map as (MAP (rootRef as {root=ref NIL}))) new = let
      val () = (#root rootRef) := NODE {
                   value = new,
                   left = NIL,
                   right = NIL
               }
  in
      map
  end
  | put (map as (MAP (rootRef as {root=ref (node as NODE{value=value,left=left,right=right})}))) new = let
      fun compare (x:(Key.t * 'a)) = Key.compare (#1 new) (#1 x)
      fun insertTree (tree as NODE {value, left, right}) = (
          case (compare value) of
              EQUAL => NODE {value = new, left = left, right = right}
            | LESS => NODE {value=value, left = (insertTree left), right = right}
            | GREATER => NODE {value = value, left = left, right = (insertTree right)}
      )
        | insertTree NIL = NODE {
              value = new,
              left = NIL,
              right = NIL
          }
      val newRoot = splay compare (insertTree node)
      val () = (#root rootRef) := newRoot
  in
      map
  end

fun contains map key =
    case (get map key) of
      NONE => false
     | _ => true

fun remove EMPTY _ = NONE
  | remove (MAP {root=ref NIL}) _ = NONE
  | remove (map as MAP (rootRef as {root=ref (node as NODE{value=value,left=left,right=right})})) key = let
      fun compare (x:(Key.t * 'a)) = Key.compare key (#1 x)
      val newRoot = splay compare node
      val () = (#root rootRef) := newRoot
  in
      case newRoot of
          NIL => raise Fail "Splay non-nil should not return nil"
       | NODE {value, left, right} => (
           case (compare value) of
               EQUAL => (
                (#root rootRef) := join(left, right);
                SOME (map, (#2 value))
            )
             | _ => NONE
       )
  end

end
