(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

(* Low-level instrumentation of Text operations. *) 

MODULE TextStats;

IMPORT TextClass, Text8, Text8Short, Text16, Text16Short, TextSub, TextCat;
IMPORT WeakRef; 

IMPORT RTHeap;
IMPORT Time, Tick;

VAR GStartTime : ARRAY Op OF Time . T; 
VAR GStartTick : ARRAY Op OF Tick . T; 

PROCEDURE AllocOps (VAR Ops: REF OpsInfo) = 
  BEGIN 
    Ops := NEW (REF OpsInfo);
    InitOps (Ops ^)
  END AllocOps; 

PROCEDURE InitOps (VAR i: OpsInfo) = 
  BEGIN 
    FOR RI := FIRST (i) TO LAST (i)
    DO WITH Opi = i [RI] 
      DO 
        Opi.GroundCt := 0;
        Opi.RecurseCt := 0;
        Opi.IterCt := 0;
        Opi.MaxRecurseCt := 0;
        Opi.MaxIterCt := 0;
        Opi.CurRecurseCt := 0;
        Opi.CurIterCt := 0;
        Opi.Time1 := 0.0;
        Opi.Time2 := 0.0
      END
    END
  END InitOps;  

PROCEDURE NoteGround (o: Op) = 
  VAR Ops: REF OpsInfo; 
  BEGIN
    IF TextClass.Old THEN Ops := OldOps ELSE Ops := NewOps END; 
    IF Ops # NIL AND TextClass.CollectStats  
    THEN WITH Opi = Ops ^ [o]
      DO 
        Opi.CurRecurseCt := 0;
        Opi.CurIterCt := 0;
        INC (Opi.GroundCt);
        GStartTime [ o ] := Time . Now ( ); 
        GStartTick [ o ] := Tick . Now ( ) 
      END (* WITH *) 
    END (* IF *) 
  END NoteGround;

PROCEDURE NoteFinished (o: Op) = 
  VAR Ops: REF OpsInfo; 
  VAR LElapsedSecsTick , LElapsedSecsTime  : LONGREAL;
  VAR LStopTime : Time . T; 
  VAR LStopTick : Tick . T; 
  BEGIN
    IF TextClass.Old THEN Ops := OldOps ELSE Ops := NewOps END; 
    IF Ops # NIL AND TextClass.CollectStats
    THEN WITH Opi = Ops ^ [o]
      DO
        LStopTick := Tick . Now ( ); 
        LStopTime := Time . Now ( );
        Opi.MaxRecurseCt := MAX (Opi.MaxRecurseCt, Opi.CurRecurseCt);
        Opi.MaxIterCt := MAX (Opi.MaxIterCt, Opi.CurIterCt);
        LElapsedSecsTick := Tick . ToSeconds ( LStopTick - GStartTick [ o ] ); 
        LElapsedSecsTime := LStopTime - GStartTime [ o ] ; 
        Opi.Time1 := Opi . Time1 + FLOAT (LElapsedSecsTick, REAL); 
        Opi.Time2 := Opi . Time2 + FLOAT (LElapsedSecsTime, REAL) 
      END (* WITH *) 
    END (* IF *) 
  END NoteFinished;

PROCEDURE NoteRecurse (o: Op) = 
  VAR Ops: REF OpsInfo; 
  BEGIN
    IF TextClass.Old THEN Ops := OldOps ELSE Ops := NewOps END; 
    IF Ops # NIL AND TextClass.CollectStats 
    THEN WITH Opi = Ops ^ [o]
      DO
        INC (Opi.RecurseCt);
        INC (Opi.CurRecurseCt)
      END (* WITH *) 
    END (* IF *) 
  END NoteRecurse;

PROCEDURE SaveRecurseDepth (o: Op; VAR Buf: CARDINAL) = 
  VAR Ops: REF OpsInfo; 
  BEGIN
    IF TextClass.Old THEN Ops := OldOps ELSE Ops := NewOps END; 
    IF Ops # NIL AND TextClass.CollectStats 
    THEN Buf := Ops ^ [o].CurRecurseCt
    END (* IF *) 
  END SaveRecurseDepth;

PROCEDURE RestoreRecurseDepth (o: Op; Buf: CARDINAL) = 
  VAR Ops: REF OpsInfo; 
  BEGIN
    IF TextClass.Old THEN Ops := OldOps ELSE Ops := NewOps END; 
    IF Ops # NIL AND TextClass.CollectStats 
    THEN Ops ^ [o].CurRecurseCt := Buf 
    END (* IF *) 
  END RestoreRecurseDepth;

PROCEDURE NoteIter (o: Op) = 
  VAR Ops: REF OpsInfo; 
  BEGIN
    IF TextClass.Old THEN Ops := OldOps ELSE Ops := NewOps END; 
    IF Ops # NIL AND TextClass.CollectStats 
    THEN WITH Opi = Ops ^ [o]
      DO
        INC (Opi.IterCt);
        INC (Opi.CurIterCt)
      END (* WITH *) 
    END (* IF *) 
  END NoteIter;

PROCEDURE CountTime1 (o: Op; t: REAL) = 
  VAR Ops: REF OpsInfo; 
  BEGIN
    IF TextClass.Old THEN Ops := OldOps ELSE Ops := NewOps END; 
    IF Ops # NIL AND TextClass.CollectStats 
    THEN WITH Opi = Ops ^ [o]
      DO
        Opi.Time1 := Opi.Time1 + t
      END (* WITH *) 
    END (* IF *) 
  END CountTime1;

PROCEDURE CountTime2 (o: Op; t: REAL) = 
  VAR Ops: REF OpsInfo; 
  BEGIN
    IF TextClass.Old THEN Ops := OldOps ELSE Ops := NewOps END; 
    IF Ops # NIL AND TextClass.CollectStats 
    THEN WITH Opi = Ops ^ [o]
      DO
        Opi.Time2 := Opi.Time2 + t
      END (* WITH *) 
    END (* IF *) 
  END CountTime2;

PROCEDURE AllocObjs (VAR Objs: REF ObjsInfo) = 
  BEGIN
    Objs := NEW (REF ObjsInfo);
    InitObjs (Objs^)
  END AllocObjs;

PROCEDURE InitObjs (VAR i: ObjsInfo) = 
  BEGIN 
    FOR RI := FIRST (i) TO LAST (i)
    DO WITH Obji = i[RI] 
      DO 
        Obji.AllocCt := 0; 
        Obji.CollectCt := 0; 
        Obji.AllocSize := 0; 
        Obji.CollectSize := 0; 
        Obji.AllocChars := 0; 
        Obji.CollectChars := 0; 
      END
    END
  END InitObjs;

VAR Text8ShortSize : CARDINAL; 
VAR Text8Size : CARDINAL; 
VAR Text16ShortSize : CARDINAL; 
VAR Text16Size : CARDINAL; 
VAR TextSubSize : CARDINAL; 
VAR TextCatSize : CARDINAL; 

PROCEDURE NoteAllocText8Short (o: Text8Short.T) = 
  VAR Objs: REF ObjsInfo; 
  BEGIN
    IF TextClass.Old 
    THEN 
      Objs := OldObjs; 
      IF TextClass.CollectStats 
      THEN EVAL WeakRef.FromRef(o, NoteCollectOldText8Short)
      END;
    ELSE 
      Objs := NewObjs; 
      IF TextClass.CollectStats 
      THEN EVAL WeakRef.FromRef(o, NoteCollectNewText8Short)
      END;
    END; 
    IF Objs # NIL AND TextClass.CollectStats 
    THEN WITH Obji = Objs ^ [Obj.Text8Short]
      DO 
        INC(Obji.AllocCt);
        INC(Obji.AllocSize, Text8ShortSize);
        INC(Obji.AllocChars, o.len);
      END (* WITH *) 
    END (* IF *) 
  END NoteAllocText8Short;

PROCEDURE NoteCollectOldText8Short 
  (<*UNUSED*> READONLY w: WeakRef.T; ref: REFANY) = 
  VAR o: Text8Short.T;
  BEGIN
    IF OldObjs # NIL 
    THEN WITH Obji = OldObjs ^ [Obj.Text8Short]
      DO
        o := ref; 
        INC(Obji.CollectCt);
        INC(Obji.CollectSize, Text8ShortSize);
        INC(Obji.CollectChars, o.len);
      END (* WITH *) 
    END (* IF *) 
  END NoteCollectOldText8Short;

PROCEDURE NoteCollectNewText8Short 
  (<*UNUSED*> READONLY w: WeakRef.T; ref: REFANY) = 
  VAR o: Text8Short.T;
  BEGIN
    IF NewObjs # NIL 
    THEN WITH Obji = NewObjs ^ [Obj.Text8Short]
      DO
        o := ref; 
        INC(Obji.CollectCt);
        INC(Obji.CollectSize, Text8ShortSize);
        INC(Obji.CollectChars, o.len);
      END (* WITH *) 
    END (* IF *) 
  END NoteCollectNewText8Short;

PROCEDURE NoteAllocText8 (o: Text8.T) = 
  VAR Objs: REF ObjsInfo; 
  BEGIN
    IF TextClass.Old 
    THEN 
      Objs := OldObjs; 
      IF TextClass.CollectStats 
      THEN EVAL WeakRef.FromRef(o, NoteCollectOldText8)
      END;
    ELSE 
      Objs := NewObjs; 
      IF TextClass.CollectStats 
      THEN EVAL WeakRef.FromRef(o, NoteCollectNewText8)
      END;
    END; 
    IF Objs # NIL AND TextClass.CollectStats 
    THEN WITH Obji = Objs ^ [Obj.Text8]
      DO 
        INC(Obji.AllocCt);
        INC(Obji.AllocSize, Text8Size);
      END (* WITH *) 
    END (* IF *) 
  END NoteAllocText8;

PROCEDURE NoteCollectOldText8 
  (<*UNUSED*> READONLY w: WeakRef.T; ref: REFANY) = 
  VAR o: Text8.T;
  BEGIN
    IF OldObjs # NIL 
    THEN WITH Obji = OldObjs ^ [Obj.Text8]
      DO
        o := ref; 
        INC(Obji.CollectCt);
        INC(Obji.CollectSize, Text8Size);
      END (* WITH *) 
    END (* IF *) 
  END NoteCollectOldText8;

PROCEDURE NoteCollectNewText8 
  (<*UNUSED*> READONLY w: WeakRef.T; ref: REFANY) = 
  VAR o: Text8.T;
  BEGIN
    IF NewObjs # NIL 
    THEN WITH Obji = NewObjs ^ [Obj.Text8]
      DO
        o := ref; 
        INC(Obji.CollectCt);
        INC(Obji.CollectSize, Text8Size);
      END (* WITH *) 
    END (* IF *) 
  END NoteCollectNewText8;

PROCEDURE NoteAllocText8Chars (o: REF ARRAY OF CHAR) = 
  VAR Objs: REF ObjsInfo; 
  VAR size: CARDINAL;
  BEGIN
    IF TextClass.Old 
    THEN 
      Objs := OldObjs; 
      IF TextClass.CollectStats 
      THEN EVAL WeakRef.FromRef(o, NoteCollectOldText8Chars)
      END;
    ELSE 
      Objs := NewObjs; 
      IF TextClass.CollectStats 
      THEN EVAL WeakRef.FromRef(o, NoteCollectNewText8Chars)
      END;
    END; 
    IF Objs # NIL AND TextClass.CollectStats 
    THEN WITH Obji = Objs ^ [Obj.Text8Chars]
      DO 
        INC(Obji.AllocCt);
        size := OpenArraySize(RTHeap.GetDataSize(o));
        INC(Obji.AllocSize, size);
        INC(Obji.AllocChars, NUMBER(o^));
      END (* WITH *) 
    END (* IF *) 
  END NoteAllocText8Chars;

PROCEDURE NoteCollectOldText8Chars 
  (<*UNUSED*> READONLY w: WeakRef.T; ref: REFANY) = 
  VAR o: REF ARRAY OF CHAR;
  VAR size: CARDINAL;
  BEGIN
    IF OldObjs # NIL 
    THEN WITH Obji = OldObjs ^ [Obj.Text8Chars]
      DO
        o := ref; 
        INC(Obji.CollectCt);
        size := OpenArraySize(RTHeap.GetDataSize(o));
        INC(Obji.CollectSize, size);
        INC(Obji.CollectChars, NUMBER(o^));
      END (* WITH *) 
    END (* IF *) 
  END NoteCollectOldText8Chars;

PROCEDURE NoteCollectNewText8Chars 
  (<*UNUSED*> READONLY w: WeakRef.T; ref: REFANY) = 
  VAR o: REF ARRAY OF CHAR;
  VAR size: CARDINAL;
  BEGIN
    IF NewObjs # NIL 
    THEN WITH Obji = NewObjs ^ [Obj.Text8Chars]
      DO
        o := ref; 
        INC(Obji.CollectCt);
        size := OpenArraySize(RTHeap.GetDataSize(o));
        INC(Obji.CollectSize, size);
        INC(Obji.CollectChars, NUMBER(o^));
      END (* WITH *) 
    END (* IF *) 
  END NoteCollectNewText8Chars;

PROCEDURE NoteAllocText16Short (o: Text16Short.T) = 
  VAR Objs: REF ObjsInfo; 
  BEGIN
    IF TextClass.Old 
    THEN 
      Objs := OldObjs; 
      IF TextClass.CollectStats 
      THEN EVAL WeakRef.FromRef(o, NoteCollectOldText16Short)
      END;
    ELSE 
      Objs := NewObjs; 
      IF TextClass.CollectStats 
      THEN EVAL WeakRef.FromRef(o, NoteCollectNewText16Short)
      END;
    END; 
    IF Objs # NIL AND TextClass.CollectStats 
    THEN WITH Obji = Objs ^ [Obj.Text16Short]
      DO 
        INC(Obji.AllocCt);
        INC(Obji.AllocSize, Text16ShortSize);
        INC(Obji.AllocChars, o.len);
      END (* WITH *) 
    END (* IF *) 
  END NoteAllocText16Short;

PROCEDURE NoteCollectOldText16Short 
  (<*UNUSED*> READONLY w: WeakRef.T; ref: REFANY) = 
  VAR o: Text16Short.T;
  BEGIN
    IF OldObjs # NIL 
    THEN WITH Obji = OldObjs ^ [Obj.Text16Short]
      DO
        o := ref; 
        INC(Obji.CollectCt);
        INC(Obji.CollectSize, Text16ShortSize);
        INC(Obji.CollectChars, o.len);
      END (* WITH *) 
    END (* IF *) 
  END NoteCollectOldText16Short;

PROCEDURE NoteCollectNewText16Short 
  (<*UNUSED*> READONLY w: WeakRef.T; ref: REFANY) = 
  VAR o: Text16Short.T;
  BEGIN
    IF NewObjs # NIL 
    THEN WITH Obji = NewObjs ^ [Obj.Text16Short]
      DO
        o := ref; 
        INC(Obji.CollectCt);
        INC(Obji.CollectSize, Text16ShortSize);
        INC(Obji.CollectChars, o.len);
      END (* WITH *) 
    END (* IF *) 
  END NoteCollectNewText16Short;

PROCEDURE NoteAllocText16 (o: Text16.T) = 
  VAR Objs: REF ObjsInfo; 
  BEGIN
    IF TextClass.Old 
    THEN 
      Objs := OldObjs; 
      IF TextClass.CollectStats 
      THEN EVAL WeakRef.FromRef(o, NoteCollectOldText16)
      END;
    ELSE 
      Objs := NewObjs; 
      IF TextClass.CollectStats 
      THEN EVAL WeakRef.FromRef(o, NoteCollectNewText16)
      END;
    END; 
    IF Objs # NIL AND TextClass.CollectStats 
    THEN WITH Obji = Objs ^ [Obj.Text16]
      DO 
        INC(Obji.AllocCt);
        INC(Obji.AllocSize, Text16Size);
      END (* WITH *) 
    END (* IF *) 
  END NoteAllocText16;

PROCEDURE NoteCollectOldText16 
  (<*UNUSED*> READONLY w: WeakRef.T; ref: REFANY) = 
  VAR o: Text16.T;
  BEGIN
    IF OldObjs # NIL 
    THEN WITH Obji = OldObjs ^ [Obj.Text16]
      DO
        o := ref; 
        INC(Obji.CollectCt);
        INC(Obji.CollectSize, Text16Size);
      END (* WITH *) 
    END (* IF *) 
  END NoteCollectOldText16;

PROCEDURE NoteCollectNewText16 
  (<*UNUSED*> READONLY w: WeakRef.T; ref: REFANY) = 
  VAR o: Text16.T;
  BEGIN
    IF NewObjs # NIL 
    THEN WITH Obji = NewObjs ^ [Obj.Text16]
      DO
        o := ref; 
        INC(Obji.CollectCt);
        INC(Obji.CollectSize, Text16Size);
      END (* WITH *) 
    END (* IF *) 
  END NoteCollectNewText16;

PROCEDURE NoteAllocText16Chars (o: REF ARRAY OF WIDECHAR) = 
  VAR Objs: REF ObjsInfo; 
  VAR size: CARDINAL;
  BEGIN
    IF TextClass.Old 
    THEN 
      Objs := OldObjs; 
      IF TextClass.CollectStats 
      THEN EVAL WeakRef.FromRef(o, NoteCollectOldText16Chars)
      END;
    ELSE 
      Objs := NewObjs; 
      IF TextClass.CollectStats 
      THEN EVAL WeakRef.FromRef(o, NoteCollectNewText16Chars)
      END;
    END; 
    IF Objs # NIL AND TextClass.CollectStats 
    THEN WITH Obji = Objs ^ [Obj.Text16Chars]
      DO 
        INC(Obji.AllocCt);
        size := OpenArraySize(RTHeap.GetDataSize(o));
        INC(Obji.AllocSize, size);
        INC(Obji.AllocChars, NUMBER(o^));
      END (* WITH *) 
    END (* IF *) 
  END NoteAllocText16Chars;

PROCEDURE NoteCollectOldText16Chars 
  (<*UNUSED*> READONLY w: WeakRef.T; ref: REFANY) = 
  VAR o: REF ARRAY OF WIDECHAR;
  VAR size: CARDINAL;
  BEGIN
    IF OldObjs # NIL 
    THEN WITH Obji = OldObjs ^ [Obj.Text16Chars]
      DO
        o := ref; 
        INC(Obji.CollectCt);
        size := OpenArraySize(RTHeap.GetDataSize(o));
        INC(Obji.CollectSize, size);
        INC(Obji.CollectChars, NUMBER(o^));
      END (* WITH *) 
    END (* IF *) 
  END NoteCollectOldText16Chars;

PROCEDURE NoteCollectNewText16Chars 
  (<*UNUSED*> READONLY w: WeakRef.T; ref: REFANY) = 
  VAR o: REF ARRAY OF WIDECHAR;
  VAR size: CARDINAL;
  BEGIN
    IF NewObjs # NIL 
    THEN WITH Obji = NewObjs ^ [Obj.Text16Chars]
      DO
        o := ref; 
        INC(Obji.CollectCt);
        size := OpenArraySize(RTHeap.GetDataSize(o));
        INC(Obji.CollectSize, size);
        INC(Obji.CollectChars, NUMBER(o^));
      END (* WITH *) 
    END (* IF *) 
  END NoteCollectNewText16Chars;

PROCEDURE NoteAllocTextSub (o: TextSub.TT) = 
  VAR Objs: REF ObjsInfo; 
  BEGIN
    IF TextClass.Old 
    THEN 
      Objs := OldObjs; 
      IF TextClass.CollectStats 
      THEN EVAL WeakRef.FromRef(o, NoteCollectOldTextSub)
      END;
    ELSE 
      Objs := NewObjs; 
      IF TextClass.CollectStats 
      THEN EVAL WeakRef.FromRef(o, NoteCollectNewTextSub)
      END;
    END; 
    IF Objs # NIL AND TextClass.CollectStats 
    THEN WITH Obji = Objs ^ [Obj.TextSub]
      DO 
        INC(Obji.AllocCt);
        INC(Obji.AllocSize, TextSubSize);
      END (* WITH *) 
    END (* IF *) 
  END NoteAllocTextSub;

PROCEDURE NoteCollectOldTextSub 
  (<*UNUSED*> READONLY w: WeakRef.T; ref: REFANY) = 
  VAR o :TextSub.TT; 
  BEGIN
    IF OldObjs # NIL 
    THEN WITH Obji = OldObjs ^ [Obj.TextSub]
      DO
        o := ref;
        INC(Obji.CollectCt);
        INC(Obji.CollectSize, TextSubSize);
      END (* WITH *) 
    END (* IF *) 
  END NoteCollectOldTextSub;

PROCEDURE NoteCollectNewTextSub 
  (<*UNUSED*> READONLY w: WeakRef.T; ref: REFANY) = 
  VAR o :TextSub.TT; 
  BEGIN
    IF NewObjs # NIL 
    THEN WITH Obji = NewObjs ^ [Obj.TextSub]
      DO
        o := ref;
        INC(Obji.CollectCt);
        INC(Obji.CollectSize, TextSubSize);
      END (* WITH *) 
    END (* IF *) 
  END NoteCollectNewTextSub;

PROCEDURE NoteAllocTextCat (o: TextCat.T) = 
  VAR Objs: REF ObjsInfo; 
  BEGIN
    IF TextClass.Old 
    THEN 
      Objs := OldObjs; 
      IF TextClass.CollectStats 
      THEN EVAL WeakRef.FromRef(o, NoteCollectOldTextCat)
      END;
    ELSE 
      Objs := NewObjs; 
      IF TextClass.CollectStats 
      THEN EVAL WeakRef.FromRef(o, NoteCollectNewTextCat)
      END;
    END; 
    IF Objs # NIL AND TextClass.CollectStats 
    THEN WITH Obji = Objs ^ [Obj.TextCat]
      DO 
        INC(Obji.AllocCt);
        INC(Obji.AllocSize, TextCatSize);
      END (* WITH *) 
    END (* IF *) 
  END NoteAllocTextCat;

PROCEDURE NoteCollectOldTextCat 
  (<*UNUSED*> READONLY w: WeakRef.T; ref: REFANY) = 
  VAR o :TextCat.T; 
  BEGIN
    IF OldObjs # NIL 
    THEN WITH Obji = OldObjs ^ [Obj.TextCat]
      DO 
        o := ref;
        INC(Obji.CollectCt);
        INC(Obji.CollectSize, TextCatSize);
      END (* WITH *) 
    END (* IF *) 
  END NoteCollectOldTextCat;

PROCEDURE NoteCollectNewTextCat 
  (<*UNUSED*> READONLY w: WeakRef.T; ref: REFANY) = 
  VAR o :TextCat.T; 
  BEGIN
    IF NewObjs # NIL 
    THEN WITH Obji = NewObjs ^ [Obj.TextCat]
      DO
        o := ref;
        INC(Obji.CollectCt);
        INC(Obji.CollectSize, TextCatSize);
      END (* WITH *) 
    END (* IF *) 
  END NoteCollectNewTextCat;

CONST WordSize = ADRSIZE(ADDRESS);
CONST WordSizeM1 = WordSize-1; 

PROCEDURE ObjSize ( DataSize : CARDINAL ) : CARDINAL = 
(* Memory used by a traced object with DataSize. *) 

  
  VAR LResult : CARDINAL;  

  BEGIN 
    LResult 
      := ADRSIZE ( INTEGER ) (* Heap allocator overhead word. *) 
         + ADRSIZE ( ADDRESS ) (* Method suite pointer. *) 
         + DataSize;
    LResult := ((LResult+WordSizeM1) DIV WordSize) * WordSize; 
    RETURN LResult 
  END ObjSize; 

PROCEDURE OpenArraySize 
  ( DataSize : CARDINAL ; Dimensions : CARDINAL := 1 ) : CARDINAL = 
(* Memory used by a heap-allocated open array with DataSize and Dimensions. *) 

  VAR LResult : CARDINAL;  

  BEGIN 
    LResult
      := ADRSIZE ( INTEGER ) (* Heap allocator overhead word. *) 
         + ADRSIZE ( ADDRESS ) (* Imbedded pointer to array cells. *) 
         + Dimensions * ADRSIZE ( ADDRESS ) (* Shape. *) 
         + DataSize;  
    LResult := ((LResult+WordSizeM1) DIV WordSize) * WordSize; 
    RETURN LResult 
  END OpenArraySize; 

(* WARNING: This can't be done at module initialization time.  Not enough
            runtime initialization has been done, and RTHeap.GetDataSize
            will segfault.
*) 
PROCEDURE InitInstrumentation ( ) =
  BEGIN 
    Text8ShortSize := ObjSize(RTHeap.GetDataSize (NEW (Text8Short.T))); 
    Text8Size := ObjSize(RTHeap.GetDataSize (NEW (Text8.T))); 
    Text16ShortSize := ObjSize(RTHeap.GetDataSize (NEW (Text16Short.T))); 
    Text16Size := ObjSize(RTHeap.GetDataSize (NEW (Text16.T))); 
    TextSubSize := ObjSize(RTHeap.GetDataSize (NEW (TextSub.TT))); 
    TextCatSize := ObjSize(RTHeap.GetDataSize (NEW (TextCat.T))); 
  END InitInstrumentation; 

BEGIN
END TextStats.
