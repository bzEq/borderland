(* Copyright (c) 2018 Kai Luo <gluokai@gmail.com>. All rights reserved. *)

functor RopeFn(C : CHARSET) = struct
open Support
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
      if i < weight then (
          (i, LESS)
      )
      else if i < rightBound then (
          (i - weight, EQUAL)
      )
      else (
          (i - rightBound, GREATER)
      )
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
                                      left = left'',
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

fun slice' (NIL, _, _) = raise Subscript
  | slice' (_, _, 0) = NIL
  | slice' ((c as CONCAT {
                  slice=sl,
                  size=size,
                  left=left,
                  right=right
            }), start, len) = let
      val startOrder = getOrder start c
      val endOrder = getOrder (start + len - 1) c
  in
      case startOrder of
          (i, EQUAL) => (
           case endOrder of
               (j, EQUAL) => (
               CONCAT {
                   slice = Charset.slice (sl, i, SOME (j - i + 1)),
                   size = j - i + 1,
                   left = NIL,
                   right = NIL
               })
             | (_, LESS) =>
               raise Fail "Right bound should not be less than left bound"
             | (j, GREATER) => let
                 val right' = slice' (right, 0, j + 1)
             in
                 CONCAT {
                     slice = Charset.slice (sl, i, NONE),
                     size = (Charset.length sl) - i + (length right'),
                     left = NIL,
                     right = right'
                 }
             end
       )
        | (i, LESS) => (
            case endOrder of
                (j, EQUAL) => let
                 val left' = slice' (left, i, (length left) - i)
             in
                 CONCAT {
                     slice = Charset.slice (sl, 0, SOME (j + 1)),
                     size = (length left') + (j + 1),
                     left = left',
                     right = NIL
                 }
             end
              | (j, LESS) => slice' (left, i, j - i + 1)
              | (j, GREATER) => let
                  val left' = slice' (left, i, (length left) - i)
                  val right'= slice' (right, 0, j + 1)
              in
                  CONCAT {
                      slice = sl,
                      size = (length left') +
                             (Charset.length sl) +
                             (length right'),
                      left = left',
                      right = right'
                  }
              end
        )
        | (i, GREATER) => (
            case endOrder of
                (_, EQUAL) => raise Fail "Right bound should not be less than left bound"
              | (_, LESS) => raise Fail "Right bound should not be less than left bound"
              | (j, GREATER) => slice' (right, i, j - i + 1)
        )
  end

fun slice (NIL, _, _) = raise Subscript
  | slice ((c as CONCAT {
                 size=size,
                 ...
           }), start, len) = case len of
                                 NONE => slice' (c, start, size - start)
                               | SOME l => slice' (c, start, l)


end
