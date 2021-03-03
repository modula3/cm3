(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Scanner.i3                                            *)
(* Last modified on Tue Jul  5 10:45:56 PDT 1994 by kalsow     *)

INTERFACE Scanner;

IMPORT Token, File, M3ID, M3String, M3WString, M3, Target;

TYPE
  Symbol = RECORD
    token  : Token.T;
    id     : M3ID.T;
    str    : M3String.T;
    wstr   : M3WString.T;
    int    : Target.Int;
    float  : Target.Float;
    offset : INTEGER;
    defn   : M3.Value;
  END;

VAR
  cur     : Symbol; (* READONLY *)
  offset  : INTEGER;
  nLines  : INTEGER := 0; (* READONLY *)
  nPushed : INTEGER := 0; (* READONLY *)
  in_main : BOOLEAN := FALSE;

PROCEDURE GetToken ();
PROCEDURE Match    (t: Token.T);
PROCEDURE MatchID  (): M3ID.T;
PROCEDURE Fail     (msg: TEXT);

PROCEDURE NoteReserved (name: M3ID.T;  value: M3.Value);

PROCEDURE Here (VAR file: TEXT;  VAR line: INTEGER);
PROCEDURE LocalHere (VAR file: TEXT;  VAR line: INTEGER);
PROCEDURE SameFile (a, b: INTEGER): BOOLEAN;

PROCEDURE Push (name: TEXT;  file: File.T;  is_main: BOOLEAN);
PROCEDURE Pop ();

PROCEDURE Initialize ();
PROCEDURE Reset ();

(* Support for retrieving actual text. *)
PROCEDURE EnableTextCapture();
PROCEDURE DisableTextCapture(VAR text: REF ARRAY OF CHAR; VAR size: INTEGER);

END Scanner.
