(* Copyright (c) 2018 Kai Luo <gluokai@gmail.com>. All rights reserved. *)

signature COMPARABLE = sig

type t

val compare : t -> t -> order

end
