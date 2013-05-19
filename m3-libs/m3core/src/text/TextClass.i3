
(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

(* Modula-3 "TEXT"s are implemented as objects with the
   methods described below. *)

INTERFACE TextClass;

CONST Brand = "Text-2.0";

VAR Old: BOOLEAN := FALSE; (* Set this TRUE to use old CM3 TEXT algorithms. *) 
VAR Flatten: BOOLEAN := TRUE; (* Set FALSE to never flatten Cat trees. *) 
VAR MaxFlat8 : CARDINAL := 64;    (* CHARs, Including the terminating null. *) 
VAR MaxFlatWide : CARDINAL := 32; (* WIDECHARs, Including terminating null. *) 

VAR CollectStats : BOOLEAN := FALSE; 

REVEAL
  TEXT = BRANDED Brand OBJECT
  METHODS
    get_info       (VAR i: Info);

    get_char       (i: CARDINAL): CHAR      := GetChar;
    get_wide_char  (i: CARDINAL): WIDECHAR  := GetWideChar;

    get_chars      (VAR a: ARRAY OF CHAR;      start: CARDINAL) := GetChars;
    get_wide_chars (VAR a: ARRAY OF WIDECHAR;  start: CARDINAL) := GetWideChars;
  END;

TYPE
  Info = RECORD
    start  : ADDRESS;  (* non-NIL => string is at [start .. start+length) *)
    length : CARDINAL; (* length of string in characters *)
    wide   : BOOLEAN;  (* => string contains WIDECHARs. *)
  END;
  (* Note: the "start" address may refer to a heap variable, so
     it is only valid as long as it is kept on the stack. *)

PROCEDURE GetChar (t: TEXT;  i: CARDINAL): CHAR;
(* Returns "CHOP(t.get_wide_char (i))" *)

PROCEDURE GetWideChar (t: TEXT;  i: CARDINAL): WIDECHAR;
(* Returns "EXTEND(t.get_char(i))" *)

PROCEDURE GetChars (t: TEXT;  VAR a: ARRAY OF CHAR;  start: CARDINAL);
(* Returns the result of "CHOP"ing the characters returned by
   "t.get_wide_chars (a, start)". *)

PROCEDURE GetWideChars (t: TEXT;  VAR a: ARRAY OF WIDECHAR;  start: CARDINAL);
(* Returns the result of "EXTEND"ing the characters returned by
   "t.get_chars (a, start)". *)


END TextClass.

(* The Modula-3 language definition says that "TEXT" is predeclared and
   a subtype of "REFANY";  We pretend that "TYPE TEXT <: REFANY"
   is in the "Text" interface.

   The function "CHOP" converts a "WIDECHAR" to a "CHAR" by
   dropping the high-order eight bits of the character.

   The function "EXTEND" converts a "CHAR" to a "WIDECHAR" by
   zero-extending the character to a 16-bit value.
*)
