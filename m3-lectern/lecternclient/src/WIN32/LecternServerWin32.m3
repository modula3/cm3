(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Last modified on Thu May 23 17:24:03 PDT 1996 by mcjones    *)

MODULE LecternServerWin32 EXPORTS LecternServer;

IMPORT Thread;

(* No-op implementation; should rewrite LecternServerPosix in terms of
   named pipes or Winsock. *)

VAR
  mutex := NEW(MUTEX);
  cond := NEW(Thread.Condition);
  requestAvailable := FALSE;

PROCEDURE AwaitRequest(): REF ARRAY OF TEXT RAISES {} =
(* "Error" is only raised if something unrecoverable happens;
   otherwise "AwaitRequest" waits for another connection attempt. *)
  BEGIN
    LOCK mutex DO
      WHILE NOT requestAvailable DO Thread.Wait(mutex, cond) END
    END;
    RETURN NIL;
  END AwaitRequest;

BEGIN
END LecternServerWin32.
