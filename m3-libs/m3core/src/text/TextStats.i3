(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

(* Low-level instrumentation of Text operations. *) 

INTERFACE TextStats; 

IMPORT Text8Short, Text8, Text16Short, Text16, TextSub, TextCat;

TYPE Op = { FromChar, FromWideChar, FromChars, FromWideChars,
            Cat, Sub, Equal, Compare, Hash, HasWideChars, 
            GetChar, GetWideChar, SetChars, SetWideChars, 
            FindChar, FindWideChar, FindCharR, FindWideCharR, 
            MultiCat, get_char, get_wide_char, get_chars, get_wide_chars }; 

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

PROCEDURE NoteAllocText8Short (o: Text8Short.T); 
PROCEDURE NoteAllocText8 (o: Text8.T); 
PROCEDURE NoteAllocText8Chars (o: REF ARRAY OF CHAR); 
PROCEDURE NoteAllocText16Short (o: Text16Short.T); 
PROCEDURE NoteAllocText16 (o: Text16.T); 
PROCEDURE NoteAllocText16Chars (o: REF ARRAY OF WIDECHAR); 
PROCEDURE NoteAllocTextSub (o: TextSub.TT); 
PROCEDURE NoteAllocTextCat (o: TextCat.T); 

PROCEDURE InitInstrumentation ( ); 

END TextStats.

