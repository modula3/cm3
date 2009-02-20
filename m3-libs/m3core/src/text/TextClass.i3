(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

(* Modula-3 "TEXT"s are implemented as objects with the
   methods described below. *)

INTERFACE TextClass;

IMPORT Text8Short, Text8, Text16Short, Text16, TextSub, TextCat;

CONST Brand = "Text-2.0";

VAR Old: BOOLEAN := FALSE;
VAR Flatten: BOOLEAN := TRUE; 
VAR MaxFlat8 : CARDINAL := 32;    (* CHARSs, Including the terminating null. *) 
VAR MaxFlatWide : CARDINAL := 16; (* WIDECHARs, Including terminating null. *) 

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

(* All of the stuff below is for instrumentation of new algorithms, and
   probably temporary. *) 

TYPE Op = { FromChar, FromWideChar, FromChars, FromWideChars,
            Cat, Sub, Equal, Compare, Hash, HasWideChars, 
            GetChar, GetWideChar, SetChars, SetWideChars, 
            FindChar, FindWideChar, FindCharR, FindWideCharR, 
            get_char, get_wide_char, get_chars, get_wide_chars }; 

TYPE OpInfo = RECORD
                Time1, Time2 : REAL := 0.0;
                GroundCt, RecurseCt, IterCt: CARDINAL := 0;
                MaxRecurseCt, MaxIterCt: CARDINAL := 0;
                CurRecurseCt, CurIterCt: CARDINAL := 0;
              END;   

TYPE OpsInfo = ARRAY Op OF OpInfo; 

VAR OldOps, NewOps : REF OpsInfo := NIL; 

PROCEDURE AllocOps (VAR Ops: REF OpsInfo); 
PROCEDURE InitOps (VAR i: OpsInfo); 

PROCEDURE NoteGround (o: Op);
PROCEDURE NoteFinished (o: Op);
PROCEDURE NoteRecurse (o: Op);
PROCEDURE SaveRecurseDepth (o: Op; VAR Buf: CARDINAL);
PROCEDURE RestoreRecurseDepth (o: Op; Buf: CARDINAL);  
PROCEDURE NoteIter (o: Op);
PROCEDURE CountTime1 (o: Op; t: REAL);
PROCEDURE CountTime2 (o: Op; t: REAL);

TYPE Obj = { Text8, Text8Chars, Text8Short, Text16, Text16Chars, Text16Short, 
             TextSub, TextCat};

TYPE ObjInfo = RECORD 
                 AllocCt, CollectCt: CARDINAL := 0; 
                 AllocSize, CollectSize: CARDINAL := 0;
                 AllocChars, CollectChars: CARDINAL := 0
               END; 

TYPE ObjsInfo = ARRAY Obj OF ObjInfo; 

VAR OldObjs, NewObjs : REF ObjsInfo := NIL; 

PROCEDURE AllocObjs (VAR Objs: REF ObjsInfo); 
PROCEDURE InitObjs (VAR i: ObjsInfo);

PROCEDURE NoteAllocText8Short (o :Text8Short.T); 
PROCEDURE NoteAllocText8 (o :Text8.T); 
PROCEDURE NoteAllocText8Chars (o :REF ARRAY OF CHAR); 
PROCEDURE NoteAllocText16Short (o :Text16Short.T); 
PROCEDURE NoteAllocText16 (o :Text16.T); 
PROCEDURE NoteAllocText16Chars (o :REF ARRAY OF WIDECHAR); 
PROCEDURE NoteAllocTextSub (o :TextSub.TT); 
PROCEDURE NoteAllocTextCat (o :TextCat.T); 

PROCEDURE InitInstrumentation ( ); 

END TextClass.

(* The Modula-3 language definition says that "TEXT" is predeclared and
   a subtype of "REFANY";  We pretend that "TYPE TEXT <: REFANY"
   is in the "Text" interface.

   The function "CHOP" converts a "WIDECHAR" to a "CHAR" by
   dropping the high-order eight bits of the character.

   The function "EXTEND" converts a "CHAR" to a "WIDECHAR" by
   zero-extending the character to a 16-bit value.
*)
