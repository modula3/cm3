(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

INTERFACE ObErr;
IMPORT SynWr;

  EXCEPTION Fail;

  PROCEDURE Msg(swr: SynWr.T; msg: TEXT);

  PROCEDURE Fault(swr: SynWr.T; msg: TEXT) RAISES {Fail};

END ObErr.
