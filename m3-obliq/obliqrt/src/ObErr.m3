(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

MODULE ObErr;
IMPORT SynWr, Text;

PROCEDURE Msg(swr: SynWr.T; msg: TEXT) = 
  BEGIN 
    IF NOT Text.Empty(msg) THEN
      SynWr.Text(swr, msg, loud:=TRUE); 
      SynWr.Char(swr, '\n', loud:=TRUE);
      SynWr.Flush(swr, loud:=TRUE); 
    END; 
  END Msg;

PROCEDURE Fault(swr: SynWr.T; msg: TEXT) RAISES {Fail} = 
  BEGIN 
    Msg(swr, msg);
    RAISE Fail;
  END Fault;

BEGIN
END ObErr.
