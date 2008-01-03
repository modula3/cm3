(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(*      modified on Sat Apr 16 by rrw1000@hermes.cam.ac.uk    *)
(*      modified on Fri Apr 30 14:41:53 PDT 1993 by muller    *)

UNSAFE INTERFACE Umsg;

FROM Ctypes IMPORT char, char_star, short, unsigned_short, long, int;
FROM Utypes IMPORT time_t;
FROM Uipc   IMPORT struct_ipc_perm;
FROM Uexec  IMPORT wait_queue_star;

(*** <sys/msg.h> ***)

(* IPC Message Facility. *)

CONST

  PZERO = 25;                     (* I got this value from param.h; em *)
  PMSG = (PZERO + 2); 		  (* message facility sleep priority *)

  (* Permission Definitions. *)

  MSG_R = 8_0400;       (* read permission *)
  MSG_W = 8_0200;       (* write permission *)

  (* ipc_perm Mode Definitions. *)

  MSG_RWAIT = 8_01000;          (* a reader is waiting for a message *)
  MSG_WWAIT = 8_02000;          (* a writer is waiting to send *)
  MSG_LCK   = 8_010000;         (* message queue locked *)
  MSG_WANT  = 8_020000;         (* lock wanted *)


(*
 * The following lock routines are required for  message queues because
 * in the middle of list manipulation, copyin()/copyout() are called.  
 * An implicit side effect of copyin/copyout are possible page faults on the
 * user buffers being accessed.  Page faults can cause the process to sleep, 
 * thereby causing a competing process to startup. -- depp
 *
 *)

(** actually, theses macros reference sleep, panic, wakeup, which I 
    cannot find; therefore their implementations are buggy - em **)
PROCEDURE MSG_LOCK (x: UNTRACED REF struct_msqid_ds);
PROCEDURE MSG_UNLOCK (x: UNTRACED REF struct_msqid_ds);


CONST
  (* Message Operation Flags. *)

  MSG_NOERROR = 8_010000;               (* no error if big message *)

  (* Sizing constants *)

   MSGMAP  = 100;
   MSGMAX  = 8192;
   MSGMNB  = 16384;
   MSGMNI  = 50;
   MSGSSZ  = 8;
   MSGTQL  = 40;
   MSGSEG  = 1024;

(*
**      There is one msg queue id data structure for each q in the system.
*)

TYPE
  struct_msqid_ds = RECORD
    msg_perm:   struct_ipc_perm;  (* operation permission struct *)
    msg_first:  struct_msg_star;  (* ptr to first message on q *)
    msg_last:   struct_msg_star;  (* ptr to last message on q *)
    msg_stime:  time_t;           (* last msgsnd time *)
    msg_rtime:  time_t;           (* last msgrcv time *)
    msg_ctime:  time_t;           (* last change time *)
    wwait    :  wait_queue_star;  
    rwait    :  wait_queue_star;  (* God knows *)
    msg_cbytes: unsigned_short;   (* current # bytes on q *)
    msg_qnum:   unsigned_short;   (* # of messages on q *)
    msg_qbytes: unsigned_short;   (* max # of bytes on q *)
    msg_lspid:  unsigned_short;   (* pid of last msgsnd *)
    msg_lrpid:  unsigned_short;   (* pid of last msgrcv *)
  END;

(*
**      There is one msg structure for each message that may be in the system.
*)

TYPE
  struct_msg = RECORD
    msg_next: UNTRACED REF struct_msg;    (* ptr to next message on q *)
    msg_type: long;                       (* message type *)
    msg_spot: char_star;                  (* message text address *)
    msg_ts:   short;                      (* message text size *)
  END;
  struct_msg_star = UNTRACED REF struct_msg;

(*
**      User message buffer template for msgsnd and msgrecv system calls.
*)

  struct_msgbuf = RECORD
    mtype: long;                          (* message type *)
    mtext: ARRAY [0..0] OF char;          (* message text *)
  END;
(*
**      Message information structure.
*)

  struct_msginfo = RECORD
    msgpool: int;      (* Linux-specific *)
    msgmap : int;      (* # of entries in msg map *)
    msgmax : int;      (* max message size *)
    msgmnb : int;      (* max # bytes on queue *)
    msgmni : int;      (* # of message queue identifiers *)
    msgssz : int;      (* msg segment size (should be word size multiple) *)
    msgtql : int;      (* # of system message headers *)
    msgseg : unsigned_short;      (* # of msg segments (MUST BE < 32768) *)
  END;


END Umsg.
