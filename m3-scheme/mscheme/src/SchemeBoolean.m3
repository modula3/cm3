(* $Id$ *)

(*
  Copyright (c) 2008, Generation Capital Ltd.  All rights reserved.

  Author: Mika Nystrom <mika@alum.mit.edu>
*)

MODULE SchemeBoolean EXPORTS SchemeBoolean;
FROM Scheme IMPORT Object;
IMPORT Pickle;
IMPORT Rd, Thread, Wr;

REVEAL T = BRANDED Brand REF BOOLEAN;

VAR (* CONST *) LTrue, LFalse := NEW(T);

PROCEDURE Truth(x : BOOLEAN) : T =
  BEGIN IF x THEN RETURN LTrue ELSE RETURN LFalse END END Truth;

PROCEDURE TruthO(x : Object) : BOOLEAN =
  BEGIN RETURN x # LFalse END TruthO;

PROCEDURE True() : T = BEGIN RETURN LTrue END True;

PROCEDURE False() : T = BEGIN RETURN LFalse END False;

PROCEDURE BoolPklWrite (
    <*UNUSED*> sp: Pickle.Special;
    r: REFANY; writer: Pickle.Writer)
    RAISES { Wr.Failure, Thread.Alerted } =
  BEGIN
    IF    r = LTrue THEN
      writer.writeInt(1)
    ELSIF r = LFalse THEN
      writer.writeInt(0)
    END
  END BoolPklWrite;

PROCEDURE BoolPklRead (
    <*UNUSED*> sp: Pickle.Special;
    reader: Pickle.Reader;
    <*UNUSED*> id: Pickle.RefID) : REFANY
    RAISES { Pickle.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted } =
  BEGIN
    WITH res = reader.readInt() DO
      CASE res OF
        1 => RETURN LTrue
      |
        0 => RETURN LFalse
      ELSE
        RAISE Pickle.Error("bad value")
      END
    END
  END BoolPklRead;

BEGIN 

  LTrue^ := TRUE; LFalse^ := FALSE;

  Pickle.RegisterSpecial (NEW (Pickle.Special, sc := TYPECODE (T),
    write := BoolPklWrite,
    read  := BoolPklRead 
));


END SchemeBoolean.
