(* Copyright (c) 2018 Kai Luo <gluokai@gmail.com>. All rights reserved. *)

structure Flags = struct
open Support

structure StringMap = SplayMapFn(
  struct
  type t = string
  fun compare a b = String.compare (a, b)
  end
)

datatype option = SWITCH of string
                | WITHARG of string * string

type flags = option StringMap.map

type commandline = {
  options : (string list) StringMap.map,
  positionalArgs : string list
}

fun storeOption (cmd as {options, positionalArgs} : commandline) (key, value) =
    case (StringMap.get options key) of
        NONE => {options = (StringMap.put options (key, [value])),
                 positionalArgs = positionalArgs}
      | SOME l => {options = (StringMap.put options (key, l @ [value])),
                   positionalArgs = positionalArgs}

fun storeSwitchOption (cmd as {options, positionalArgs} : commandline) key =
    case (StringMap.get options key) of
        NONE => {options = (StringMap.put options (key, [])),
                 positionalArgs = positionalArgs}
      | _ => cmd

fun storeArg (cmd as {options, positionalArgs} : commandline) arg =
    {options = options, positionalArgs = positionalArgs @ [arg]}

fun parseArgs (spec : flags) (args : string list) : commandline = let
    fun accParseArgs acc [] = acc
      | accParseArgs (acc as {options, positionalArgs})
                     (flag::arg::rest) = let
          val flag' = Substring.full flag
          val arg' = Substring.full arg
      in
          if (Substring.isPrefix "-" flag') then let
              val key = Substring.string (Substring.slice (flag', 1, NONE))
          in
              case (StringMap.get spec key) of
                  NONE => accParseArgs acc (arg::rest) (* Ignored unrecognized option *)
                | SOME (SWITCH _) => accParseArgs (storeSwitchOption acc key) (arg::rest)
                | SOME (WITHARG _) =>
                  if (Substring.isPrefix "-" arg') then (* Followed by another option, ignoring this one *)
                      accParseArgs acc (arg::rest)
                  else
                      accParseArgs (storeOption acc (key, arg)) rest
          end
          else
            accParseArgs (storeArg acc (Substring.string flag')) (arg::rest)
      end
      | accParseArgs (acc as {options, positionalArgs})
                     (flag::[]) = let
        val flag' = Substring.full flag
      in
        if (Substring.isPrefix "-" flag') then let
            val key = Substring.string (Substring.slice (flag', 1, NONE))
        in
            case (StringMap.get spec key) of
                SOME (SWITCH _) => storeSwitchOption acc key
              | _ => acc
        end
        else
          storeArg acc flag
      end
in
    accParseArgs {
        options = StringMap.empty,
        positionalArgs = []
    } args
end

end
