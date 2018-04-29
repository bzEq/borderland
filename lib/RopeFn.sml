(* Copyright (c) 2018 Kai Luo <gluokai@gmail.com>. All rights reserved. *)

functor RopeFn(C : CHARSET) = struct
structure Charset = C

val maxSizeOfSlice = 4

datatype rope = NIL
              | CONCAT of {
                  slice : Charset.substring,
                  size : int,
                  left : rope,
                  right : rope
              }

fun init s =
    if Charset.isEmpty s then
        NIL
    else let
        val l = Charset.length s
    in
        if l <= maxSizeOfSlice then
            CONCAT {
                slice = s,
                size = l,
                left = NIL,
                right = NIL
            }
        else let
            val leftGap = (l - maxSizeOfSlice) div 2
        in
            CONCAT {
                slice = Charset.slice (s, leftGap, SOME maxSizeOfSlice),
                size = l,
                left = init (Charset.slice (s, 0, SOME leftGap)),
                right = init (Charset.slice (s, (leftGap + maxSizeOfSlice), NONE))
            }
        end
    end


fun isEmpty NIL = true
  | isEmpty (CONCAT {size, ...}) = size = 0

fun length NIL = 0
  | length (CONCAT {size, ...}) = size

fun getOrder i NIL = raise Subscript
  | getOrder i (CONCAT {left=left, right=right, slice=slice, ...}) = let
      val weight = length left
      val sliceLength = Charset.length slice
      val rightBound = weight + sliceLength
  in
      if i < weight then
          (i, LESS)
      else if i < rightBound then
          (i - weight, EQUAL)
      else
          (i - rightBound, GREATER)
  end


fun sub (NIL, _) = raise Subscript
  | sub (c as (CONCAT {left=left, right=right, slice=slice, ...}), i) = (
      case (getOrder i c) of
          (i', LESS) => sub (left, i')
        | (i', EQUAL) => Charset.sub (slice, i')
        | (i', _) => sub (right, i')
  )

(* Rope with splay *)
fun splay NIL _ = NIL
  | splay (c as (CONCAT {slice, size, left, right})) index =
    case (getOrder index c) of
        (_, EQUAL) => c
      | (i, LESS) => (
          case left of
              NIL => c
            | (c' as CONCAT {
                    slice=slice',
                    size=size',
                    left=left',
                    right=right'
              }) => (
                case (getOrder i c') of
                    (_, EQUAL) => CONCAT {
                                     slice = slice',
                                     size = size,
                                     left = left',
                                     right = CONCAT {
                                         slice = slice,
                                         size = size - size' +
                                                (length right'),
                                         left = right',
                                         right = right
                                     }
                                 }
                  (* zigzig *)
                  | (i', LESS) => (
                      case left' of
                          NIL => CONCAT {
                                    slice = slice',
                                    size = size,
                                    left = left',
                                    right = CONCAT {
                                        slice = slice,
                                        size = size - size' +
                                               (length right'),
                                        left = right',
                                        right = right
                                    }
                                }
                        | _ => (
                            case (splay left' i') of
                                NIL =>
                                raise Fail "splay non-nil node returning nil node"
                              | CONCAT {
                                    slice=slice'',
                                    size=size'',
                                    left=left'',
                                    right=right''
                                } => let
                                  val right''' = CONCAT {
                                          slice = slice,
                                          size = size - size' + (length right'),
                                          left = right',
                                          right = right
                                      }
                              in
                                  CONCAT {
                                      slice = slice'',
                                      size = size,
                                      left = left,
                                      right = CONCAT {
                                          slice = slice',
                                          size = (length right'') +
                                                 (length right''') +
                                                 (Charset.length slice''),
                                          left = right'',
                                          right = right'''
                                      }
                                  }
                              end
                        )
                  )
                  (* zigzag *)
                  | (i', GREATER) => (
                      case right' of
                          NIL => CONCAT {
                                    slice = slice',
                                    size = size,
                                    left = left',
                                    right = CONCAT {
                                        slice = slice,
                                        size = size - size' +
                                               (length right'),
                                        left = right',
                                        right = right
                                    }
                                }
                        | _ => (
                            case (splay right' i') of
                                NIL =>
                                raise Fail "splay non-nil node returning nil node"
                              | CONCAT {
                                    slice=slice'',
                                    size=size'',
                                    left=left'',
                                    right=right''
                                } => let
                                  val left''' = CONCAT {
                                          slice = slice',
                                          size = size' - size'' +
                                                 (length left''),
                                          left = left',
                                          right = left''
                                      }
                                  val right''' = CONCAT {
                                          slice = slice,
                                          size = size - size' +
                                                 (length right''),
                                          left = right'',
                                          right = right
                                      }
                              in
                                  CONCAT {
                                      slice = slice'',
                                      size = size,
                                      left = left''',
                                      right = right'''
                                  }
                              end
                        )
                  )
            )
      )
      | (i, GREATER) => (
          case right of
              NIL => c
            | (c' as CONCAT {
                    slice=slice',
                    size=size',
                    left=left',
                    right=right'}) => (
                case (getOrder i c') of
                    (_, EQUAL) => CONCAT {
                                     slice = slice',
                                     size = size,
                                     left = CONCAT {
                                         slice = slice,
                                         size = size - size' + (length left'),
                                         left = left,
                                         right = left'
                                     },
                                     right = right'
                                 }
                  (* zigzig *)
                  | (i', GREATER) => (
                      case right' of
                          NIL => CONCAT {
                                    slice = slice',
                                    size = size,
                                    left = CONCAT {
                                        slice = slice,
                                        size = size - size' + (length left'),
                                        left = left,
                                        right = left'
                                    },
                                    right = right'
                                }
                        | _ => (
                            case (splay right' i') of
                                NIL =>
                                raise Fail "splay non-nil node returning nil node"
                              | CONCAT {
                                    slice=slice'',
                                    size=size'',
                                    left=left'',
                                    right=right''} => let
                                  val left''' = CONCAT {
                                          slice = slice,
                                          size = size - size' + (length left'),
                                          left = left,
                                          right = left'
                                      }
                              in
                                  CONCAT {
                                      slice = slice'',
                                      size = size,
                                      left = CONCAT {
                                          slice = slice',
                                          size = (length left''') +
                                                 (length left'') +
                                                 (Charset.length slice''),
                                          left = left''',
                                          right = left''
                                      },
                                      right = right''
                                  }
                              end
                        )
                  )
                  (* zigzag *)
                  | (i', LESS) => (
                      case left' of
                          NIL => CONCAT {
                                    slice = slice',
                                    size = size,
                                    left = CONCAT {
                                        slice = slice,
                                        size = size - size' + (length left'),
                                        left = left,
                                        right = left'
                                    },
                                    right = right'
                                }
                        | _ => (
                            case (splay left' i') of
                                NIL =>
                                raise Fail "splay non-nil node returning nil node"
                              | CONCAT {
                                    slice=slice'',
                                    size=size'',
                                    left=left'',
                                    right=right''
                                } => let
                                  val left''' = CONCAT {
                                          slice = slice,
                                          size = size - size' + (length left''),
                                          left = left,
                                          right = left''
                                      }
                                  val right''' = CONCAT {
                                          slice = slice',
                                          size = size' - size'' + (length right''),
                                          left = right'',
                                          right = right'
                                      }
                              in
                                  CONCAT {
                                      slice = slice'',
                                      size = size,
                                      left = left''',
                                      right = right'''
                                  }
                              end
                        )
                  )
            )
      )

fun exhaustedLeftRotate NIL = NIL
  | exhaustedLeftRotate (c as CONCAT {right=NIL, ...}) = c
  | exhaustedLeftRotate (CONCAT {
                              slice=slice,
                              size=size,
                              left=left,
                              right=CONCAT {
                                  slice=slice',
                                  size=size',
                                  left=left',
                                  right=right'
                              }
                        }) = exhaustedLeftRotate
                                 (CONCAT {
                                       size = size,
                                       slice = slice',
                                       left = CONCAT {
                                           slice = slice,
                                           size = size - size' +
                                                  (length left'),
                                           left = left,
                                           right = left'
                                       },
                                       right = right'
                                 })

fun concat NIL NIL = NIL
  | concat NIL r = r
  | concat r NIL = r
  | concat l r =
    case exhaustedLeftRotate l of
        CONCAT {slice, size, left, right=NIL} =>
        CONCAT {
            slice = slice,
            size = size + (length r),
            left = left,
            right = r
        }
      | _ => raise Fail "exhaustedLeftRotate failed"

fun toString NIL = ""
  | toString (CONCAT {slice, left, right, ...}) =
    (toString left) ^ (Charset.toString slice) ^ (toString right)

end
