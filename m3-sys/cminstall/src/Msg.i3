(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

INTERFACE Msg;

IMPORT AtomList, TextSeq;

VAR Debugging := FALSE;

PROCEDURE AskBool   (question, default: TEXT): BOOLEAN;
PROCEDURE Ask       (question, default: TEXT; suf : TEXT := NIL): TEXT;
PROCEDURE AskChoice (question: TEXT; choices : TextSeq.T): TEXT;

PROCEDURE Debug (a, b, c, d: TEXT := NIL);
PROCEDURE Out   (a, b, c, d, e, f: TEXT := NIL);
PROCEDURE OutS  (a, b, c, d, e, f: TEXT := NIL); (* no newline *)
PROCEDURE Warn  (a, b, c, d: TEXT := NIL);
PROCEDURE Error (ec: AtomList.T;  a, b, c, d: TEXT := NIL);

PROCEDURE AttachDrain (filename: TEXT);
PROCEDURE FinishLog (filename: TEXT);

END Msg.
