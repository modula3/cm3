(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* ListPkl.m3 *)
(* Last modified on Thu Apr  1 17:29:02 PST 1993 by wobber *)
(*      modified on Tue Feb 16 23:54:36 PST 1993 by owicki *)
(*      modified on Tue Sep  1 15:37:17 PDT 1992 by evers  *)


(* Atom Pickle.Special *)

MODULE AtomPkl;

IMPORT Atom, Thread, Rd, Wr, Pickle, Pickle2;

PROCEDURE AtomPklWrite (
    <*UNUSED*> sp: Pickle.Special;
    r: REFANY; writer: Pickle.Writer)
    RAISES { Pickle.Error, Wr.Failure, Thread.Alerted } =
  BEGIN
    writer.write(Atom.ToText(r));
  END AtomPklWrite;

PROCEDURE AtomPklRead (
    <*UNUSED*> sp: Pickle.Special;
    reader: Pickle.Reader;
    <*UNUSED*> id: Pickle.RefID) : REFANY
    RAISES { Pickle.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted } =
  BEGIN
    RETURN Atom.FromText(reader.read());
  END AtomPklRead;

PROCEDURE AtomPklWrite2 (
    <*UNUSED*> sp: Pickle2.Special;
    r: REFANY; writer: Pickle2.Writer)
    RAISES { Pickle2.Error, Wr.Failure, Thread.Alerted } =
  BEGIN
    writer.write(Atom.ToText(r));
  END AtomPklWrite2;

PROCEDURE AtomPklRead2 (
    <*UNUSED*> sp: Pickle2.Special;
    reader: Pickle2.Reader;
    <*UNUSED*> id: Pickle2.RefID) : REFANY
    RAISES { Pickle2.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted } =
  BEGIN
    RETURN Atom.FromText(reader.read());
  END AtomPklRead2;

BEGIN
  Pickle.RegisterSpecial (NEW (Pickle.Special, sc := TYPECODE (Atom.T),
    write := AtomPklWrite,
    read  := AtomPklRead));
  Pickle2.RegisterSpecial (NEW (Pickle2.Special, sc := TYPECODE (Atom.T),
    write := AtomPklWrite2,
    read  := AtomPklRead2));
END AtomPkl.
