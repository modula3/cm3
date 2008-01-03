(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Fri Feb 10 11:29:37 PST 1995 by kalsow     *)
(*      modified on Thu Jul 15 17:33:05 PDT 1993 by swart      *)
(*      modified on Wed Apr 21 13:15:46 PDT 1993 by mcjones    *)
(*      modified on Tue Mar  9 15:02:05 PST 1993 by mjordan    *)
(*      Modified On Mon Jun 29 22:11:02 PDT 1992 by muller     *)
(*      Modified on Thu Mar 30 18:30:10 1989 by hisgen         *)
(*      Modified On Tue Jan 19 11:18:19 PST 1988 by denning    *)
(*      Modified On Mon Oct 13 17:01:19 1986 by levin          *)
(*      Modified On Wed Dec  5 13:10:00 1984 by Andrew Birrell *)

UNSAFE MODULE TimeStamp;

IMPORT Date, Thread, Time, Cstring, Word, TimeStampRep,
       Swap, LongFloat, FloatMode, Process, MachineID;

TYPE
  Counter = MUTEX OBJECT
    time        : [0 .. LAST(Swap.Int32)] := 0;
    fineTime    : [0..255]                := 0;
    fineCounter : [0..255]                := 0;
  END;

VAR
  counter     := NEW(Counter);
  init_done   := FALSE;
  myPIDHigh   : Swap.UInt16;
  myPIDLow    : Swap.UInt16;
  myMachineID : MachineID.T;
  epoch       : Time.T;

PROCEDURE Init () =
  <*FATAL Date.Error*>
  CONST
    epochDate = Date.T{1970, Date.Month.Jan, 2, 0,0,0,0, "", Date.WeekDay.Sun};
    oneDay    = 24.0d0 * 3600.0d0;
  VAR
    pid := Process.GetMyID ();
  BEGIN
    EVAL MachineID.CanGet (myMachineID); (* if we can't get it, too bad. *)
    myPIDHigh   := Word.Extract(pid, 0, 16);
    myPIDLow    := Word.Extract(pid, 16, 16);
    epoch       := Date.ToTime(epochDate) - oneDay;
    init_done   := TRUE;
  END Init;

PROCEDURE New (): T =
  VAR
    fineTime   : INTEGER;
    fineCounter: [0 .. 255];
    time       : [0 .. LAST(Swap.Int32)];
    ts         : TimeStampRep.T;
    now        : Time.T;
  <*FATAL FloatMode.Trap*>
  BEGIN
    LOCK counter DO
      IF NOT init_done THEN Init () END;
      LOOP
        now := Time.Now() - epoch;
        time := TRUNC(now);
        fineTime := ROUND(LongFloat.Scalb(now - FLOAT(time, LONGREAL), 8));
        IF fineTime > 255 THEN fineTime := 0; INC(time); END;
        IF counter.time # time OR counter.fineTime # fineTime THEN
          fineCounter := 0;
          counter.fineCounter := 0;
          counter.time := time;
          counter.fineTime := fineTime;
          EXIT
        ELSIF counter.fineCounter < 255 THEN
          INC(counter.fineCounter);
          fineCounter := counter.fineCounter;
          EXIT
        ELSE
          Thread.Release(counter);
          TRY
            Thread.Pause(1.0D0 / 256.0D0);
          FINALLY
            Thread.Acquire(counter);
          END;
        END;
      END;
    END;
    IF Swap.endian = Swap.Endian.Big
      THEN ts.time := time;
      ELSE ts.time := Swap.Swap4(time);
    END;
    ts.fineTime       := fineTime;
    ts.fineCounter    := fineCounter;
    ts.machineHigh[0] := myMachineID.r[0];
    ts.machineHigh[1] := myMachineID.r[1];
    ts.machineLow[0]  := myMachineID.r[2];
    ts.machineLow[1]  := myMachineID.r[3];
    ts.machineLow[2]  := myMachineID.r[4];
    ts.machineLow[3]  := myMachineID.r[5];
    ts.pidHigh        := myPIDHigh;
    ts.pidLow         := myPIDLow;
    RETURN LOOPHOLE(ts, T);
  END New;

PROCEDURE Compare (READONLY t1, t2: T): [-1 .. 1] =
  VAR res := Cstring.memcmp(ADR(t1), ADR(t2), BYTESIZE(T));
  BEGIN
    IF    res = 0 THEN  RETURN 0;
    ELSIF res < 0 THEN  RETURN -1;
    ELSE                RETURN 1;
    END;
  END Compare;

PROCEDURE Equal(READONLY t1, t2: T): BOOLEAN =
  BEGIN
    RETURN t1 = t2;
  END Equal;

PROCEDURE Max(READONLY t1, t2: T): T =
  BEGIN
    IF Compare(t1, t2) = 1 THEN RETURN t2 ELSE RETURN t1; END;
  END Max;

PROCEDURE Min(READONLY t1, t2: T): T =
  BEGIN
    IF Compare(t1, t2) = 1 THEN RETURN t1 ELSE RETURN t2; END;
  END Min;

PROCEDURE Hash (READONLY t: T): INTEGER =
  VAR i: INTEGER;
  BEGIN
    WITH a = LOOPHOLE(t, ARRAY [0 .. 3] OF Swap.Int32) DO
      i := Word.Xor(Word.Xor(a[0], a[1]), Word.Xor(a[2], a[3]));
    END;
    IF Swap.endian = Swap.Endian.Big THEN i := Swap.Swap4 (i) END;
    RETURN i;
  END Hash;

PROCEDURE ToTime (READONLY t: T): Time.T =
  VAR
    frac := FLOAT(LOOPHOLE(t, TimeStampRep.T).fineTime, LONGREAL) / 256.0D0;
    time := LOOPHOLE(t, TimeStampRep.T).time;
  BEGIN
    IF NOT init_done THEN Init () END;
    IF Swap.endian = Swap.Endian.Little THEN time := Swap.Swap4 (time); END;
    RETURN FLOAT(time, LONGREAL) + epoch + frac;
  END ToTime;

BEGIN
END TimeStamp.
