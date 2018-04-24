(* Copyright (c) 2018 Kai Luo <gluokai@gmail.com>. All rights reserved. *)

structure Support = struct

infix |>

fun x |> f = f x

datatype ('v, 'e) result = OK of 'v
                         | ERR of 'e

exception NotValue
exception NotError

fun getAbsolutePath path =
    if (OS.Path.isAbsolute path) then
        path
    else
        OS.Path.mkCanonical (
            OS.Path.joinDirFile {
                dir = OS.FileSys.getDir (),
                file = path
            }
        )

fun valOf res = (
  case res of
      OK v => v
    | _ => raise NotValue
)

fun errOf res = (
  case res of
      ERR e => e
    | _ => raise NotError
)


fun defer g f = let
  val res = f() handle (e as _) => (
              g ();
              raise e
            )
in
  res before g ()
end

fun println s = print (s ^ "\n")

fun die (message : string) = (
  TextIO.output (TextIO.stdErr, message ^ "\n");
  Unix.exit 0w1
)

fun dieWithExitCode message exitCode = (
  TextIO.output (TextIO.stdErr, message ^ "\n");
  Unix.exit (Word8.fromInt exitCode)
)

fun readFromFile (filename : string) : string = let
    val is = TextIO.openIn filename
in
    defer (fn () => TextIO.closeIn is)
          (fn () => TextIO.input is)
end

fun intToFD n = n |> SysWord.fromInt |> Posix.FileSys.wordToFD

end
