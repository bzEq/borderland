(* Copyright (c) 2018 Kai Luo <gluokai@gmail.com>. All rights reserved. *)

structure SplayMapFnTest = struct

structure IntMap = SplayMapFn(
    struct
    type t = int
    fun compare a b = Int.compare (a, b)
    end
)

fun createMap n = let
    fun putAcc m k =
        if k = n then
            m
        else
            putAcc (IntMap.put m (k, k)) (k+1)
in
    putAcc IntMap.empty 0
end

fun putAndGet n = let
    val m = createMap n
    fun check m k = Testing.assertTrue
                        ((Option.valOf (IntMap.get m k)) = k)
    val k = ref 0
in
    while (!k < n) do (
        check m (!k);
        k := !k + 1
    )
end

fun putAndRemove n = let
    val m = createMap n
    fun checkRemoved m k = Testing.assertFalse (Option.isSome (IntMap.get m k))
    val k = ref 0
in
    while (!k < n) do (
        IntMap.remove m (!k);
        checkRemoved m (!k);
        k := !k + 1
    );
    Testing.assertTrue (IntMap.isEmpty m)
end

fun Benchmark n = let
  val i = ref 0
  val m = IntMap.empty
  val rng = Random.rand (0, n)
  val x = ref 0
in
  while !i < n do (
    x := Random.randInt rng;
    IntMap.put m (!x, !x);
    i := (!i) + 1
  )
end

end

val _ = Testing.addTest "SplayMapFnTest.putAndGet"
                        (fn () => SplayMapFnTest.putAndGet 1048576)

val _ = Testing.addTest "SplayMapFnTest.putAndRemove"
                        (fn () => SplayMapFnTest.putAndRemove 1048576)

val _ = Testing.addTest "SplayMapFnTest.Benchmark"
                        (fn () => SplayMapFnTest.Benchmark 1048576)
