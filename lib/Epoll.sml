(* Copyright (c) 2018 Kai Luo <gluokai@gmail.com>. All rights reserved. *)

structure Epoll = struct

type t = MLton.Pointer.t

val epoll_create = _import "epoll_create" : int -> int;

val epoll_ctl = _import "epoll_ctl" : int * int * int * t -> int;

val epoll_wait = _import "epoll_wait" : int * t * int * int -> int;

end
