(* Copyright (C) 2023, Peter McKinna *)
(* All rights reserved.              *)
(* Licensed under the MIT license.   *)

INTERFACE Utils;

IMPORT TextSeq;

TYPE
  Severity = {Info, Error, Warning};

  NotifyRec = RECORD
    msg : TEXT;
    severity : Severity;
    startLine,startCol,endLine,endCol : CARDINAL;
  END;

  NotifyRef = REF NotifyRec;

  Range = RECORD
    line,col : CARDINAL;
  END;

  Incr <: IncrPublic;
  IncrPublic = OBJECT
  METHODS
    delete(VAR st : TextSeq.T; sr,er : Range);
    insert(VAR st : TextSeq.T; incr : TEXT; sr : Range);
    replace(VAR st : TextSeq.T; incr : TEXT; sr,er : Range);
  END;

  PROCEDURE UnEncode(s : TEXT) : TEXT;

  (* output a msg to stderr which displays on output window of client *)
  PROCEDURE Msg(s : TEXT);

END Utils.

