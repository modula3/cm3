(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: TipeDesc.m3                                           *)
(* Last Modified On Tue Jul  5 16:45:55 PDT 1994 by kalsow     *)

MODULE TipeDesc;

IMPORT Word, Target, TInt, TWord, Type, Module, CG;

TYPE
  ByteList = REF ARRAY OF [0..255];
  TypeList = REF ARRAY OF Type.T;

VAR
  busy    : BOOLEAN  := FALSE;
  bytes   : ByteList := NIL;
  n_bytes : INTEGER  := 0;
  types   : TypeList := NIL;
  n_types : INTEGER  := 0;

PROCEDURE Start () =
  BEGIN
    <*ASSERT NOT busy*>
    busy := TRUE;
    IF (bytes = NIL) THEN
      bytes := NEW (ByteList, 200);
      types := NEW (TypeList, 100);
    END;
    n_bytes := 0;
    n_types := 0;
  END Start;
    
PROCEDURE Finish (a, b, c, d: TEXT := NIL): INTEGER =
  VAR base, offset, n_data_bytes: INTEGER;
  BEGIN
    IF (n_bytes = 0) THEN busy := FALSE; RETURN -1 END;

    (* add the op count *)
    n_data_bytes := n_bytes;
    AddI (n_types);

    (* allocate space *)
    base := Module.Allocate (n_bytes * Target.Int8.size,
                             Target.Int8.align, TRUE, "type_desc");
    CG.Comment (base, TRUE, a, b, c, d);

    (* emit the op count *)
    offset := base;
    FOR i := n_data_bytes TO n_bytes-1 DO
      CG.Init_intt (offset, Target.Int8.size, bytes[i], is_const := TRUE);
      INC (offset, Target.Int8.size);
    END;

    (* generate the bytes *)
    FOR i := 0 TO n_data_bytes-1 DO
      CG.Init_intt (offset, Target.Int8.size, bytes[i], is_const := TRUE);
      INC (offset, Target.Int8.size);
    END;

    busy := FALSE;
    RETURN base;
  END Finish;

PROCEDURE AddO (o: Op;  type: Type.T): BOOLEAN =
  CONST MaxSmall = 255 - ORD (Op.Old0);
  VAR tt: Type.T;
  BEGIN
    FOR i := 0 TO n_types-1 DO
      tt := types[i];
      IF (tt # NIL) AND Type.IsEqual (tt, type, NIL) THEN
        IF (i <= MaxSmall)
          THEN Stuff (ORD (Op.Old0) + i);
          ELSE Stuff (ORD (Op.OldN));  AddI (i);
        END;
        IF (n_types >= NUMBER (types^)) THEN ExpandTypes () END;
        types[n_types] := NIL;  INC (n_types);
        RETURN FALSE;
      END;
    END;
    IF (n_types >= NUMBER (types^)) THEN ExpandTypes () END;
    types[n_types] := type;  INC (n_types);
    Stuff (ORD (o));
    RETURN TRUE;
  END AddO;

PROCEDURE AddU (i: INTEGER) =
  BEGIN
    Stuff (Word.And (i, 16_ff));  i := Word.RightShift (i, 8);
    Stuff (Word.And (i, 16_ff));  i := Word.RightShift (i, 8);
    Stuff (Word.And (i, 16_ff));  i := Word.RightShift (i, 8);
    Stuff (Word.And (i, 16_ff));
  END AddU;

PROCEDURE AddI (i: INTEGER) =
  BEGIN
    IF (0 <= i) THEN
      IF    (i <= 16_3f)         THEN Stuff (i);
      ELSIF (i <= 16_ff)         THEN Stuff (16_41); Stuff (i);
      ELSIF (i = 16_7fffffff)    THEN Stuff (16_7e);
      ELSIF (i = LAST(INTEGER))  THEN Stuff (16_7f);
      ELSE  AddBigInt (i);
      END;
    ELSE (* i < 0 *)
      IF    (i >= -16_3f)        THEN Stuff (16_80-i);
      ELSIF (i >= -16_ff)        THEN Stuff (16_c1); Stuff (i);
      ELSIF (i = -16_7fffffff-1) THEN Stuff (16_fe);
      ELSIF (i = FIRST(INTEGER)) THEN Stuff (16_ff);
      ELSE  AddBigInt (i);
      END;
    END;
  END AddI;

PROCEDURE AddBigInt (i: INTEGER) =
  CONST Sign = ARRAY BOOLEAN OF INTEGER { 16_40, 16_c0 };
  VAR key, n_bytes: INTEGER;   x: ARRAY [0..BYTESIZE(INTEGER)-1] OF INTEGER;
  BEGIN
    key := Sign [i < 0];
    IF (i < 0) THEN i := Word.Minus (0, i); END;

    (* extract the bytes *)
    n_bytes := 0;
    WHILE (i # 0) DO
      x[n_bytes] := Word.And (i, 16_ff);  INC (n_bytes);
      i := Word.RightShift (i, 8);
    END;

    (* stuff'm *)
    Stuff (key + n_bytes);
    FOR i := 0 TO n_bytes-1 DO Stuff (x[i]); END;
  END AddBigInt;

PROCEDURE AddX (READONLY i: Target.Int) =
  VAR x: INTEGER;
  BEGIN
    IF    TInt.ToInt (i, x)             THEN AddI (x);
    ELSIF TInt.EQ (i, Target.Int32.max) THEN Stuff (16_7e);
    ELSIF TInt.EQ (i, Target.Int32.min) THEN Stuff (16_fe);
    ELSIF TInt.EQ (i, Target.Int64.max) THEN Stuff (16_7f);
    ELSIF TInt.EQ (i, Target.Int64.min) THEN Stuff (16_ff);
    ELSE  AddBigX (i);
    END;
  END AddX;

PROCEDURE AddBigX (READONLY ii: Target.Int) =
  CONST Sign = ARRAY BOOLEAN OF INTEGER { 16_40, 16_c0 };
  VAR x: ARRAY [0..LAST (Target.Int)] OF [0..255];
      key, n_bytes: INTEGER;  i := ii;
  BEGIN
    key := Sign [TInt.LT (i, TInt.Zero)];
    IF (key # 16_40) THEN
      TWord.Subtract (TInt.Zero, ii, i);
      EVAL TInt.Extend (i, Target.Integer.bytes, i);
    END;

    (* extract the bytes *)
    n_bytes := TInt.ToUnsignedBytes (i, x);

    (* stuff'm *)
    Stuff (key + n_bytes);
    FOR i := 0 TO n_bytes-1 DO Stuff (x[i]); END;
  END AddBigX;

PROCEDURE Stuff (i: INTEGER) =
  BEGIN
    <*ASSERT busy*>
    IF (n_bytes >= NUMBER (bytes^)) THEN ExpandBytes () END;
    bytes[n_bytes] := Word.And (i, 16_ff);  INC (n_bytes);
  END Stuff;

PROCEDURE ExpandBytes () =
  VAR new := NEW (ByteList, 2 * NUMBER (bytes^));
  BEGIN
    SUBARRAY (new^, 0, NUMBER(bytes^)) := bytes^;
    bytes := new;
  END ExpandBytes;

PROCEDURE ExpandTypes () =
  VAR new := NEW (TypeList, 2 * NUMBER (types^));
  BEGIN
    SUBARRAY (new^, 0, NUMBER(types^)) := types^;
    types := new;
  END ExpandTypes;

PROCEDURE Reset () =
  BEGIN
    busy    := FALSE;
    n_bytes := 0;
    n_types := 0;
    IF (types # NIL) THEN
      FOR i := FIRST (types^) TO LAST (types^) DO types[i] := NIL END;
    END;
  END Reset;

BEGIN
END TipeDesc.
