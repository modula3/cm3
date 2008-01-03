(*-------------------------------------------------------------------------
 *
 * pqcomm.h--
 *    Parameters for the communication module
 *
 *
 * Copyright (c) 1994, Regents of the University of California
 *
 * pqcomm.h,v 1.4 1995/05/26 22:24:46 jolly Exp
 *
 * NOTES
 *    Some of this should move to libpq.h
 *
 *-------------------------------------------------------------------------
 *)

INTERFACE PQcomm;
FROM Ctypes IMPORT int, char;
FROM Postgres IMPORT NAMEDATALEN;

TYPE bool = int;

(*
 * startup msg parameters: path length, argument string length
 *)

CONST PATH_SIZE =	64;
CONST ARGV_SIZE	=	64;

TYPE MsgType = {
    ACK_MSG,		(* acknowledge a message *)
    ERROR_MSG,		(* error response to client from server *)
    RESET_MSG,		(* client must reset connection *)
    PRINT_MSG,		(* tuples for client from server *)
    NET_ERROR,		(* error in net system call *)
    FUNCTION,		(* fastpath call (unused) *)
    QUERY_MSG,		(* client query to server *)
    STARTUP_MSG,		(* initialize a connection with a backend *)
    DUPLICATE_MSG,		(* duplicate msg arrived (errors msg only) *)
    INVALID_MSG,		(* for some control functions *)
    STARTUP_KRB4_MSG,	(* krb4 session follows startup packet *)
    STARTUP_KRB5_MS      	(* krb5 session follows startup packet *)
    (* insert new values here -- DO NOT REORDER OR DELETE ENTRIES *)
};

TYPE
  Addr = ADDRESS;
  PacketLen = int;

TYPE StartupInfo = RECORD
    database: ARRAY [0..PATH_SIZE-1] OF char; (* database name *)
    user: ARRAY [0..NAMEDATALEN-1] OF char;   (* user name *)
    options: ARRAY [0..ARGV_SIZE-1] OF char;  (* possible additional args *)
    execFile: ARRAY [0..ARGV_SIZE-1] OF char; (* possible backend to use *)
    tty: ARRAY [0..PATH_SIZE-1] OF char;     (* possible tty for debug output*)
  END;
     
  StartupInfo_star = UNTRACED REF StartupInfo;

(* amount of available data in a packet buffer *)
CONST  MESSAGE_SIZE = BYTESIZE(StartupInfo) + 5;

(* I/O can be blocking or non-blocking *)
CONST BLOCKING = 0;
CONST NON_BLOCKING = 1;

(* a PacketBuf gets shipped from client to server so be careful
   of differences in representation.  
   Be sure to use htonl() and ntohl() on the len and msgtype fields! *)
TYPE 
  PacketBuf = RECORD
    len: int;
    msgtype: MsgType;
    data: ARRAY [0..MESSAGE_SIZE-1] OF char;
  END;
  PacketBuf_star = UNTRACED REF PacketBuf;

(* update the conversion routines 
  StartupInfo2PacketBuf() and PacketBuf2StartupInfo() (decl. below)
  if StartupInfo or PacketBuf structs ever change *)

(*
 * socket descriptor port 
 *	we need addresses of both sides to do authentication calls
 *)
TYPE
  Port = RECORD
    sock: int;   (* file descriptor *)
    mask: int;   (* select mask *)
    nBytes: int; (* nBytes read in so far *)
    laddr: ADDRESS; (* sockaddr_in *)
    raddr: ADDRESS; (* sockaddr_in *)
(*  id: PacketBufId;   (* id of packet buf currently in use *)  *)
    buf: PacketBuf;	(* stream implementation (curr pack buf) *)
  END;
  Port_star = UNTRACED REF Port;

(* invalid socket descriptor *)
CONST INVALID_SOCK = (-1);

CONST 
  INVALID_ID = (-1);
  MAX_CONNECTIONS        = 10;
  N_PACK_BUFS     = 20;

(* no multi-packet messages yet *)
CONST MAX_PACKET_BACKLOG = 1;

CONST DEFAULT_STRING = "";

<*EXTERNAL*>
VAR
  Pfout, Pfin: FILE; 
  PQAsyncNotifyWaiting: int;
TYPE
  FILE = ADDRESS;

(*
 * prototypes for functions in pqpacket.c
 *)
<*EXTERNAL*>PROCEDURE PacketReceive(port: Port_star;
                                    buf: PacketBuf_star;
                                    nonBlocking: bool): int;

<*EXTERNAL*>PROCEDURE PacketSend(port: Port_star; 
                                 buf: PacketBuf_star;
                                 len: PacketLen; 
                                 nonBlocking: bool): int;

<*EXTERNAL*>PROCEDURE StartupInfo2PacketBuf(startup: StartupInfo_star): PacketBuf_star;

<*EXTERNAL*>PROCEDURE PacketBuf2StartupInfo(pkt: PacketBuf_star): PacketBuf_star;

END PQcomm.
