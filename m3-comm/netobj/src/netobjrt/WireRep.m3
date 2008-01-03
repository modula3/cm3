(* Copyright (C) 1992, Digital Equipment Corporation.          *)
(* Distributed only by permission.                             *)
(* WireRep.m3 *)
(* Last modified on Thu Apr 13 13:47:41 PDT 1995 by kalsow *)
(*      modified on Fri Feb 11 14:38:21 PST 1994 by wobber *)
(*      modified on Wed Jun 10 17:14:36 PDT 1992 by owicki *)

UNSAFE MODULE WireRep;

IMPORT NetObjEpoch, SpaceID, Time, Word;  (* IO, Fmt *)

TYPE
  Int32 = BITS 32 FOR [ -16_7fffffff-1 .. 16_7fffffff ];
  TRep  = RECORD ts: Int32; objNum: Int32; space: SpaceID.T; END;

VAR myTs: Int32 := GetTime();
    myObjNum := 0;
    mu := NEW(MUTEX);

PROCEDURE GetTime (): Int32 =
  BEGIN
    RETURN ROUND (Time.Now () - NetObjEpoch.T);
  END GetTime;

PROCEDURE New() : T =
  VAR wt: TRep;
  BEGIN
    LOCK mu DO
      IF myObjNum = LAST(Int32) THEN myTs := GetTime(); END;
      INC(myObjNum);
      wt.ts := myTs;
      wt.objNum := myObjNum;
    END;
    wt.space := SpaceID.Mine();
    RETURN LOOPHOLE(wt, T);
  END New;
  
PROCEDURE Equal(t1, t2: T) : BOOLEAN =
  BEGIN
    RETURN (t1 = t2);
  END Equal;

(*
CONST Multiplier = -1640531527; (* good only for 32-bit words *)

PROCEDURE Hash(t: T) : Word.T =
  VAR x: Word.T;
  BEGIN
    x := Word.Xor(LOOPHOLE(t, TRep).objNum, LOOPHOLE(t, TRep).ts);
    IO.Put(Fmt.Unsigned(x, 16) & " " &
        Fmt.Unsigned(Word.Times(x, Multiplier), 16)
        & "\n");
    RETURN x;
  END Hash;
*)

PROCEDURE Hash(t: T) : Word.T =
  BEGIN
    RETURN Word.Xor(LOOPHOLE(t, TRep).objNum, LOOPHOLE(t, TRep).ts);
  END Hash;

PROCEDURE GetSpaceID(t: T) : SpaceID.T =
  BEGIN
    RETURN LOOPHOLE(t, TRep).space;
  END GetSpaceID;

BEGIN
END WireRep.

