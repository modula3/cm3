(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(*                                                                           *)
(* Parts Copyright (C) 1997, Columbia University                             *)
(* All rights reserved.                                                      *)
(*
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Mon Aug  4 14:55:25 1997
 *)

UNSAFE MODULE ObLib;
IMPORT SynLocation, ObCommand, Text, Pickle2 AS Pickle, Wr, Thread,
       TimeStamp, Fingerprint, ObLibTbl, Rd, WeakerRef, WeakRef,
       PickleStubs, RTAllocator;

  PROCEDURE Setup()  =
  BEGIN
    libraries := NIL;
    helpCommandSet := ObCommand.NewSet();
  END Setup;

  PROCEDURE Extend(library: T; env: Env): Env =
    BEGIN
      IF Lookup(library.name, env)#NIL THEN
        RETURN NEW(Env, library:=library, rest:=env);
      ELSE
        RETURN NEW(Env, library:=library, rest:=env);
      END;
    END Extend;

  PROCEDURE Register(library: T) =
    BEGIN
      libraries := Extend(library, libraries);
    END Register;

  PROCEDURE RegisterHelp(name: TEXT; helpProc: HelpProc) =
    BEGIN
      ObCommand.Register(helpCommandSet,
          NEW(ObCommand.T, name:=name, 
              sortingName:="mod " & name,
              Exec:=helpProc));
    END RegisterHelp;

  PROCEDURE Lookup(name: TEXT; env: Env): Env =
    BEGIN
      LOOP
        IF env=NIL THEN RETURN NIL END;
        IF Text.Equal(name, env.library.name) THEN RETURN env END;
        env := env.rest;
      END;
    END Lookup;

  PROCEDURE LookupFixity(opName: TEXT; env: Env; VAR (*out*)libName: TEXT)
      : OpFixity =
    VAR opCodes: REF ARRAY OF OpCode;
    BEGIN
      LOOP
        IF env=NIL THEN RETURN OpFixity.Undefined END;
        opCodes := env.library.opCodes;
        FOR i:=0 TO NUMBER(opCodes^)-1 DO
          IF Text.Equal(opName, opCodes^[i].name) THEN
            libName := env.library.name;
            RETURN opCodes^[i].fixity;
          END;
        END;
        env := env.rest;
      END;
    END LookupFixity;

  PROCEDURE EncodeTermOp(self: T; opName: TEXT; 
      VAR(*out*)code: OpCode; <*UNUSED*>location: SynLocation.T): BOOLEAN =
    BEGIN
      FOR i:=FIRST(self.opCodes^) TO LAST(self.opCodes^) DO
        IF Text.Equal(opName, self.opCodes^[i].name) THEN
          code := self.opCodes^[i];
          RETURN TRUE;
        END;
      END;
      RETURN FALSE;
    END EncodeTermOp;

REVEAL
  ObLibSpecial = Pickle.Special BRANDED "ObLib.ObLibSpecial" OBJECT
                       OVERRIDES
                         write := WriteLib;
                         read := ReadLib;
                       END;
  
PROCEDURE LookupFP (fp: Fingerprint.T; newLib: T): T =
  VAR lib: T := NIL;
      wref, wrefOld: WeakerRef.T;
  BEGIN
    LOCK mu DO
      IF libTbl.get(fp, wref) THEN
        lib := WeakRef.ToRef(wref.weakRef);
      END;
      IF lib = NIL AND newLib # NIL THEN
        wref := NEW(WeakerRef.T, weakRef:=WeakRef.FromRef(newLib, CleanupLib));
        IF NOT libTbl.put(fp, wref) THEN
          EVAL libTbl.delete(fp, wrefOld);
          EVAL libTbl.put(fp, wref);
        END;
        lib := newLib;
      END;
      RETURN lib;
    END;
  END LookupFP;

PROCEDURE CheckFP (lib: T) = 
  VAR wref: WeakerRef.T;
  BEGIN
    LOCK mu DO
      IF Fingerprint.Equal(lib.ts, Fingerprint.Zero) THEN
        lib.ts := ComputeFP();
        wref := NEW(WeakerRef.T, weakRef:=WeakRef.FromRef(lib, CleanupLib));
        EVAL libTbl.put(lib.ts, wref);
      END;
    END;
  END CheckFP;

PROCEDURE ComputeFP() : Fingerprint.T =
  VAR ts := TimeStamp.New();
  BEGIN
    RETURN Fingerprint.FromChars(
             LOOPHOLE(ts, ARRAY [0..15] OF CHAR), Fingerprint.OfEmpty);
  END ComputeFP;

PROCEDURE CleanupLib(<*UNUSED*>READONLY self: WeakRef.T; ref: REFANY) =
  VAR lib: T := ref; 
      val: WeakerRef.T;
  BEGIN
    LOCK mu DO
      EVAL libTbl.delete(lib.ts, val);
    END;
  END CleanupLib;

VAR 
  libTbl: ObLibTbl.T := NEW(ObLibTbl.Default).init();

VAR
  mu := NEW(MUTEX);

PROCEDURE WriteLib (<*UNUSED*>ts: ObLibSpecial; 
                    ref: REFANY; out: Pickle.Writer)
  RAISES {Pickle.Error, Wr.Failure, Thread.Alerted} =
  VAR o := NARROW(ref, T);
  BEGIN
    CheckFP(o);
    out.writeType(TYPECODE(ref));
    PickleStubs.OutBytes(out, o.ts.byte);
    PickleStubs.OutText(out, o.name);
    PickleStubs.OutRef(out, o.opCodes);
  END WriteLib; 

PROCEDURE ReadLib (<*UNUSED*>ts: ObLibSpecial;
                   in: Pickle.Reader;
                   id: Pickle.RefID):REFANY
  RAISES {Pickle.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  VAR ac := in.readType();
      lib := NARROW(RTAllocator.NewTraced(ac),T);
  BEGIN
    PickleStubs.InBytes(in, lib.ts.byte);
    lib := LookupFP(lib.ts, lib);
    in.noteRef(lib, id);

    lib.name := PickleStubs.InText(in);
    lib.opCodes := PickleStubs.InRef(in);
    RETURN lib;
  END ReadLib;

BEGIN
  Pickle.RegisterSpecial(NEW(ObLibSpecial, sc := TYPECODE(T)));
END ObLib.
