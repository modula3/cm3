(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* ListPkl.m3 *)
(* Last modified on Tue Mar  1 16:24:38 PST 1994 by wobber *)
(*      modified on Tue Sep  1 15:37:17 PDT 1992 by evers  *)


(* Iterative List Pickle.Special to prevent small stacks from 
   overflowing on RefList.Ts of length >~ 25. *)

UNSAFE MODULE ListPkl;

IMPORT RefList, Thread, Rd, Wr, Pickle, Pickle2;

PROCEDURE ListPklWrite (
    sp: Pickle.Special;
    r: REFANY; writer: Pickle.Writer)
    RAISES { Pickle.Error, Wr.Failure, Thread.Alerted } =
  VAR l: RefList.T := r; len := RefList.Length (l);
      isSubtype := TYPECODE(r) # TYPECODE(RefList.T);
  BEGIN
    Wr.PutChar (writer.wr, LOOPHOLE(isSubtype, CHAR));
    IF isSubtype THEN
      (* we don't know how to marshal subtypes of RefList.T *)
      Pickle.Special.write(sp, r, writer);
    ELSE
      writer.writeInt(len);
      FOR i := 1 TO len DO
        writer.write (l.head);
        l := l.tail;
      END;
      <* ASSERT l = NIL *>
    END;
  END ListPklWrite;

PROCEDURE ListPklRead (
    sp: Pickle.Special;
    reader: Pickle.Reader;
    id: Pickle.RefID) : REFANY
    RAISES { Pickle.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted } = 
  VAR
    len: CARDINAL;
    res, tail: RefList.T;
    isSubtype := LOOPHOLE(Rd.GetChar(reader.rd), BOOLEAN);
  BEGIN
    IF isSubtype THEN
      (* the sender encountered a subtype of RefList.T *)
      res := Pickle.Special.read(sp, reader, id);
    ELSE
      len := reader.readInt();
      IF len < 0 THEN RAISE Pickle.Error("Pickle.Error: negative int"); END;
      res := NEW (RefList.T);
      tail := res;
      FOR i := 1 TO len - 1 DO
        tail.head := reader.read ();
        tail.tail := NEW (RefList.T);
        tail := tail.tail;
      END;
      tail.head := reader.read ();
      tail.tail := NIL;
    END;
    RETURN res;
  END ListPklRead;

PROCEDURE ListPklWrite2 (
    sp: Pickle2.Special;
    r: REFANY; writer: Pickle2.Writer)
    RAISES { Pickle2.Error, Wr.Failure, Thread.Alerted } =
  VAR l: RefList.T := r; len := RefList.Length (l);
      isSubtype := TYPECODE(r) # TYPECODE(RefList.T);
  BEGIN
    Wr.PutChar (writer.wr, LOOPHOLE(isSubtype, CHAR));
    IF isSubtype THEN
      (* we don't know how to marshal subtypes of RefList.T *)
      Pickle2.Special.write(sp, r, writer);
    ELSE
      writer.writeInt(len);
      FOR i := 1 TO len DO
        writer.write (l.head);
        l := l.tail;
      END;
      <* ASSERT l = NIL *>
    END;
  END ListPklWrite2;

PROCEDURE ListPklRead2 (
    sp: Pickle2.Special;
    reader: Pickle2.Reader;
    id: Pickle2.RefID) : REFANY
    RAISES { Pickle2.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted } = 
  VAR
    len: CARDINAL;
    res, tail: RefList.T;
    isSubtype := LOOPHOLE(Rd.GetChar(reader.rd), BOOLEAN);
  BEGIN
    IF isSubtype THEN
      (* the sender encountered a subtype of RefList.T *)
      res := Pickle2.Special.read(sp, reader, id);
    ELSE
      len := reader.readInt();
      IF len < 0 THEN RAISE Pickle2.Error("Pickle.Error: negative int"); END;
      res := NEW (RefList.T);
      tail := res;
      FOR i := 1 TO len - 1 DO
        tail.head := reader.read ();
        tail.tail := NEW (RefList.T);
        tail := tail.tail;
      END;
      tail.head := reader.read ();
      tail.tail := NIL;
    END;
    RETURN res;
  END ListPklRead2;

BEGIN
  Pickle.RegisterSpecial (NEW (Pickle.Special, sc := TYPECODE (RefList.T),
    write := ListPklWrite,
    read  := ListPklRead));
  Pickle2.RegisterSpecial (NEW (Pickle2.Special, sc := TYPECODE (RefList.T),
    write := ListPklWrite2,
    read  := ListPklRead2));
END ListPkl.
