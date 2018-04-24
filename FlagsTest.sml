(* Copyright (c) 2018 Kai Luo <gluokai@gmail.com>. All rights reserved. *)

structure FlagsTest = struct
open Flags

infix ##
fun m ## new = StringMap.put m new

val flags = StringMap.empty
                    ## ("debug", SWITCH "Enable debug mode")
                    ## ("out", WITHARG ("<path>", "Specify output path"))
                    ## ("in", WITHARG ("<path>", "Specify input path"))
                    ## ("compiler", WITHARG ("<path>", "Specify binary path of compiler"))

fun testParse () = let
    val args = [
        "-out", "/tmp", "-in", "src",
        "-debug", "-x", "-compiler", "/usr/bin/mlton",
        "-out", "/var/log",
        "go.sml"
    ]
    val cmd = parseArgs flags args
in
    Testing.assertFalse (Option.isSome (StringMap.get (#options cmd) "x"));
    Testing.assertTrue (Option.isSome (StringMap.get (#options cmd) "in"));
    Testing.assertTrue (
        List.exists (fn s => s = "src")
                    (Option.valOf (StringMap.get (#options cmd) "in")));
    Testing.assertTrue (Option.isSome (StringMap.get (#options cmd) "compiler"));
    Testing.assertTrue (
        List.exists (fn s => s = "/usr/bin/mlton")
                    (Option.valOf (StringMap.get (#options cmd) "compiler")));
    Testing.assertTrue (Option.isSome (StringMap.get (#options cmd) "out"));
    Testing.assertTrue (
        List.exists (fn s => s = "/tmp")
                    (Option.valOf (StringMap.get (#options cmd) "out")));
    Testing.assertTrue (
        List.exists (fn s => s = "/var/log")
                    (Option.valOf (StringMap.get (#options cmd) "out")));
    Testing.assertTrue (
        List.exists (fn s => s = "go.sml") (#positionalArgs cmd));
    Testing.assertFalse (
        List.exists (fn s => s = "-x") (#positionalArgs cmd));
    Testing.assertFalse (
        List.exists (fn s => s = "x") (#positionalArgs cmd));
    Testing.assertTrue (Option.isSome (StringMap.get (#options cmd) "debug"))
end

end

val _ = Testing.addTest "FlagsTest.testParse" FlagsTest.testParse
