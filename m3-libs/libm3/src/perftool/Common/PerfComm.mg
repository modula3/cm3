(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Aug 26 13:03:35 PDT 1993 by kalsow                   *)
(*      modified on Tue Mar  3 00:48:13 PST 1992 by muller                   *)

GENERIC MODULE PerfComm (Event);

IMPORT Rd, Wr;

CONST
  Chars = (BITSIZE (Event.T) + BITSIZE (CHAR) - 1) DIV BITSIZE (CHAR);

TYPE 
  Bug = ARRAY[0..Chars-1] OF CHAR;

PROCEDURE Send (wr: Wr.T; READONLY e: Event.T) =
  <*FATAL ANY*>
  BEGIN
    Wr.PutString (wr, LOOPHOLE (e, Bug));
  END Send;

PROCEDURE Receive (rd: Rd.T): Event.T =
  VAR e: Event.T;
  <*FATAL ANY*>
  BEGIN
    EVAL Rd.GetSub (rd, LOOPHOLE (e, Bug));
    RETURN e;
  END Receive;

BEGIN 
END PerfComm.
