(* Copyright (C) 2023, Peter McKinna *)
(* All rights reserved.              *)
(* Licensed under the MIT license.   *)

INTERFACE Utils;

TYPE
  Severity = {Info, Error, Warning};

  NotifyRec = RECORD
    msg : TEXT;
    severity : Severity;
    startLine,startCol,endLine,endCol : CARDINAL;
  END;

  NotifyRef = REF NotifyRec;

  PROCEDURE UnEncode(s : TEXT) : TEXT;

END Utils.

