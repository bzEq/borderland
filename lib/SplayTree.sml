(* Copyright (c) 2018 Kai Luo <gluokai@gmail.com>. All rights reserved. *)
(* splaytree.sml
 *
 * COPYRIGHT (c) 2015 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Splay tree structure.
 *
 *)

structure SplayTree = struct

datatype 'a tree = NODE of {value : 'a, left : 'a tree, right : 'a tree}
                  | NIL

fun exhaustedLeftRotate NIL = NIL
  | exhaustedLeftRotate (node as NODE {right=NIL,...}) = node
  | exhaustedLeftRotate (NODE {
                            value=value,
                            left=left,
                            right=NODE {
                              value=value',
                              left=left',
                              right=right'
                            }
                        }) = exhaustedLeftRotate
                                 (NODE {
                                     value = value',
                                     left = NODE {
                                       value=value,
                                       left=left,
                                       right=left'},
                                     right = right'
                                 })

fun join (NIL, NIL) = NIL
  | join (NIL, t) = t
  | join (t, NIL) = t
  | join (l, r) =
    case exhaustedLeftRotate l of
        NODE {value,left,right=NIL} => NODE {value=value,left=left,right=r} (* right must be NIL *)
      | _ => raise Fail "exhaustedLeftRotate failed"

(* Top-down splay *)
fun splay compare root = let
  fun top NIL = NIL
    | top (node as NODE {value, left, right}) = (
      case (compare value) of
          EQUAL => node
        | LESS => (
          case left of
              NIL => node
            | NODE {value=value', left=left', right=right'} => (
              case (compare value') of
                  EQUAL => NODE {
                            value = value',
                            left = left',
                            right = NODE {
                              value = value,
                              left = right',
                              right = right
                            }
                          }
                (* zig-zig step *)
                | LESS => (
                  case left' of
                      NIL => NODE {
                              value = value',
                              left = left',
                              right = NODE {
                                value = value,
                                left = right',
                                right = right
                              }
                            }
                    | _ => (
                      case (top left') of
                          NIL => raise Fail "top non-nil node returning nil node"
                        | NODE {value=x, left=l, right=r} => let
                          val rchild = NODE {value=value,left=right',right=right}
                        in
                          NODE {
                            value = x,
                            left = l,
                            right = NODE {
                              value = value',
                              left = r,
                              right = rchild
                            }
                          }
                        end)
                )
                (* zig-zag step *)
                | GREATER => (
                  case right' of
                      NIL => NODE {
                              value = value',
                              left = left',
                              right = NODE {
                                value = value,
                                left = right',
                                right = right
                              }
                            }
                    | _ => (
                      case (top right') of
                          NIL => raise Fail "top non-nil node returning nil node"
                        | NODE {value=x, left=l, right=r} => let
                          val lchild = NODE {value = value', left = left',right = l}
                          val rchild = NODE {value = value, left = r, right = right}
                        in
                          NODE {value = x, left = lchild, right = rchild}
                        end
                    )
                )
            )
        )
        | GREATER => (
          case right of
              NIL => node
            | NODE {value=value', left=left', right=right'} => (
              case (compare value') of
                  EQUAL => NODE {
                            value = value',
                            left = NODE {
                              value = value,
                              left = left,
                              right = left'
                            },
                            right = right'
                          }
                (* zig-zag step *)
                | LESS => (
                  case left' of
                      NIL => NODE {
                              value = value',
                              left = NODE {
                                value = value,
                                left = left,
                                right = left'
                              },
                              right = right'
                            }
                    | _ => (
                      case (top left') of
                          NIL => raise Fail "top non-nil node returning nil node"
                        | NODE {value=x, left=l, right=r} => let
                          val lchild = NODE {value = value, left = left, right = l}
                          val rchild = NODE {value = value', left = r, right = right' }
                        in
                          NODE {value = x, left = lchild, right = rchild}
                        end

                    )
                )
                (* zig-zig step *)
                | GREATER => (
                  case right' of
                      NIL => NODE {
                              value = value',
                              left = NODE {
                                value = value,
                                left= left,
                                right = left'
                              },
                              right = right'
                            }
                    | _ => (
                      case (top right') of
                          NIL => raise Fail "top non-nil node returning nil node"
                        | NODE {value=x, left=l, right=r} => let
                          val lchild = NODE {value=value, left=left, right=left'}
                        in
                          NODE {
                            value = x,
                            left = NODE {
                              value = value',
                              left = lchild,
                              right = l
                            },
                            right = r
                          }
                        end
                    )
                )
            )
        )
    )
in
  top root
end

end
