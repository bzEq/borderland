(* Copyright (c) 2018 Kai Luo <gluokai@gmail.com>. All rights reserved. *)

structure TCPServer = struct
open Support
open OS

val kBufferSize = 4096

type server = {
    sock : Socket.passive INetSock.stream_sock,
    addr : INetSock.sock_addr
}

type client = {
    sock : Socket.active INetSock.stream_sock,
    addr : INetSock.sock_addr
}

fun toINetAddr addr port =
    case (NetHostDB.fromString addr) of
        NONE => ERR ("Invalid address: " ^ addr ^ ":" ^ Int.toString(port))
      | SOME addr' => OK (INetSock.toAddr (addr', port))

fun createSocketWithSockAddr sockAddr = let
    val s = INetSock.TCP.socket ()
in
    (
      Socket.bind (s, sockAddr);
      OK s
    ) handle (SysErr (msg, _)) => (Socket.close s; ERR msg)
end

fun createServerWithSockAddr sockAddr : (server, string) result =
    case (createSocketWithSockAddr sockAddr) of
      ERR msg => ERR msg
     | OK s => (
         Socket.listen (s, kBufferSize);
         OK {sock = s, addr = sockAddr}
     ) handle (SysErr (msg, _)) => (Socket.close s; ERR msg)

fun createServer (addr : string) (port : int) =
    case (toINetAddr addr port) of
        ERR msg => ERR msg
      | OK sockAddr => createServerWithSockAddr sockAddr

fun close (s : server) = Socket.close (#sock s)

fun accept (s : server) = let
    val (remoteSock, remoteAddr) = Socket.accept (#sock s);
in
    OK ({sock = remoteSock, addr = remoteAddr} : client)
end handle (SysErr (msg, _)) => ERR msg

end
