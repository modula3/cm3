(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Wed Nov  9 11:07:39 PST 1994 by isard      *)

UNSAFE MODULE M3LoaderDebug;

IMPORT IO;
IMPORT M3ID;

PROCEDURE Txt (out: TEXT) =
  BEGIN
    IF debug THEN IO.Put(out) END;
  END Txt;

PROCEDURE Name (out: M3ID.T) =
  BEGIN
    IF debug THEN IO.Put(M3ID.ToText(out)) END;
  END Name;

PROCEDURE Int (out: INTEGER) =
  BEGIN
    IF debug THEN IO.PutInt(out) END;
  END Int;

PROCEDURE Address (out: INTEGER) =
  VAR
    cont : REF INTEGER := LOOPHOLE(out, REF INTEGER);
  BEGIN
    IF debug THEN IO.PutInt(cont^) END;
  END Address;

PROCEDURE NL () =
  BEGIN
    IF debug THEN IO.Put("\n") END;
  END NL;

VAR
  debug := FALSE;
BEGIN
END M3LoaderDebug.
