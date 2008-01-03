(* Copyright 1993 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* StablePkl.m3 - pickle special for lock database            *)
(* Last modified on Thu Feb  2 09:00:56 PST 1995 by kalsow *)
(*      modified on Fri Apr 22 11:55:19 PDT 1994 by wobber *)

MODULE StablePkl;

IMPORT Fingerprint, RefList, Rd, Wr, Sx, Thread, Fmt, Convert,
       LockOps, NetPath, SortedTextRefTbl, SNetPathRefTbl,
       Text, TextWr, Atom, AtomList, SmallDB;

TYPE
  Dir = LockOps.Dir;

REVEAL
  Closure = ClosurePublic BRANDED OBJECT
  OVERRIDES
    recover := Recover;
    snapshot := Snapshot;
    readUpdate := ReadUpdate;
    logUpdate := LogUpdate;
  END;

  <* FATAL Thread.Alerted *>

VAR
  DirAtom := Atom.FromText("Dir");
  PkgAtom := Atom.FromText("Pkg");


PROCEDURE Recover(<*UNUSED*> cl: Closure; rd: Rd.T): REFANY
    RAISES {SmallDB.Failed} =
  VAR db: DB;
      topL, dirEnumL: RefList.T;
      dt := NEW(SNetPathRefTbl.Default).init();
  BEGIN
    TRY
      TYPECASE Sx.Read(rd) OF
      | RefList.T(x) => topL := x;
      ELSE RaiseFailed();
      END;
    EXCEPT
    | Sx.ReadError, Rd.EndOfFile => RaiseFailed();
    END;
    db := NEW(DB, localSite := Next(topL, TYPECODE(TEXT)), dirs := dt);
    dirEnumL := NextEnum(topL);
    WHILE dirEnumL # NIL DO
      TRY
        VAR
          dirElemL: RefList.T := Next(dirEnumL, TYPECODE(RefList.T));
          dir := NetPath.FromText(Next(dirElemL, TYPECODE(TEXT)));
          pt := NEW(SortedTextRefTbl.Default).init();
          pn: TEXT;
          pkgEnumL, pkgElemL, entryL: RefList.T;
        BEGIN
          EVAL dt.put(dir, pt);
          pkgEnumL := NextEnum(dirElemL);
          WHILE pkgEnumL # NIL DO
            pkgElemL := Next(pkgEnumL, TYPECODE(RefList.T));
            pn := Next(pkgElemL, TYPECODE(TEXT));
            entryL := Next(pkgElemL, TYPECODE(RefList.T));
            EVAL pt.put(pn, ListToEntry(entryL));
          END;
        END;
      EXCEPT
      | NetPath.Invalid => RaiseFailed();
      END;
    END;
    RETURN db;
  END Recover;

PROCEDURE Snapshot(<*UNUSED*> cl: Closure; wr: Wr.T; r: REFANY)
    RAISES {Wr.Failure} =
  <* FATAL Sx.PrintError *>
  VAR db: DB := r;
      dt := db.dirs;
      it := dt.iterate();
      dir: Dir; rr: REFANY;
      dirs: RefList.T := NIL;
  BEGIN
    WHILE it.next(dir, rr) DO
      VAR pkgs: RefList.T := NIL; pn: TEXT; rrr: REFANY;
          pt: SortedTextRefTbl.T := rr;
          itt := pt.iterate();
      BEGIN
        WHILE itt.next(pn, rrr) DO
          pkgs := RefList.Cons(RefList.List2(pn, EntryToList(rrr)), pkgs);
        END;
        dirs := RefList.Cons(RefList.List2(NetPath.ToText(dir), pkgs), dirs);
      END;
    END;
    Sx.Print(wr, RefList.List2(db.localSite, dirs));
  END Snapshot;

PROCEDURE ReadUpdate(cl: Closure; rd: Rd.T; st: REFANY): REFANY
     RAISES {SmallDB.Failed} =
  VAR update: REFANY;
      atom: Atom.T;
      topL: RefList.T;
  BEGIN
    TRY
      TYPECASE Sx.Read(rd) OF
      | RefList.T(x) => topL := x;
      ELSE RaiseFailed();
      END;

      atom := Next(topL, TYPECODE(Atom.T));
      IF atom = DirAtom THEN
        update := NEW(LogElem,
           pn  := LockOps.PN {
                    dir := NetPath.FromText(Next(topL, TYPECODE(TEXT))),
                    arc := NIL},
           add := Next(topL, TYPECODE(Atom.T)) = Sx.True);
      ELSIF atom = PkgAtom THEN
        update := NEW(LogElem,
           pn  := NetPath.PNFromText(Next(topL, TYPECODE(TEXT))),
           e   := ListToEntry(Next(topL, TYPECODE(RefList.T))));
      ELSE RaiseFailed();
      END;
    EXCEPT
    | NetPath.Invalid, Sx.ReadError, Rd.EndOfFile => RaiseFailed();
    END;
    RETURN cl.apply(update, st);
  END ReadUpdate;

PROCEDURE LogUpdate(<*UNUSED*> cl: Closure; wr: Wr.T; r: REFANY)
    RAISES {Wr.Failure} =
  VAR sx: RefList.T;
      le := NARROW(r, LogElem);
    <* FATAL Sx.PrintError *>
  BEGIN
    IF le.pn.arc = NIL THEN
      sx := RefList.List3(
         DirAtom, NetPath.ToText(le.pn.dir), Sx.FromBool(le.add));
    ELSE
      sx := RefList.List3(
         PkgAtom, NetPath.PNToText(le.pn), EntryToList(le.e));
    END;
    Sx.Print(wr, sx);
  END LogUpdate;

PROCEDURE EntryToList(e: PkgEntry): RefList.T =
  VAR l: RefList.T := NIL;
  BEGIN
    IF e.owner.key # NIL THEN
      l := RefList.List2(e.owner.key, e.owner.site);
    END;
    l := RefList.Cons(Sx.FromInt(e.pendVN), l);
    l := RefList.Cons(Sx.FromInt(e.lastVN), l);
    l := RefList.Cons(Sx.FromInt(e.curVN), l);
    l := RefList.Cons(Sx.FromInt(e.instance), l);
    l := RefList.Cons(FromFp(e.fp), l);
    l := RefList.Cons(e.managedBy, l);
    l := RefList.Cons(Sx.FromLongReal(e.lastModified), l);
    RETURN l;
  END EntryToList;

PROCEDURE ListToEntry(l: RefList.T): PkgEntry RAISES {SmallDB.Failed} =
  VAR e := NEW(PkgEntry);
  BEGIN
    e.lastModified := NextLongReal(l);
    e.managedBy := NextCommonText(l);
    e.fp := NextFp(l);
    e.instance := NextCard32(l);
    e.curVN := NextCard32(l);
    e.lastVN := NextCard32(l);
    e.pendVN := NextCard32(l);
    IF l # NIL THEN
      e.owner.key := Next(l, TYPECODE(TEXT));
      e.owner.site := NextCommonText(l);
    ELSE
      e.owner.key := NIL;
    END;
    RETURN e;
  END ListToEntry;

PROCEDURE FromFp(fp: Fingerprint.T): TEXT =
  VAR wr := TextWr.New();
    <* FATAL Wr.Failure *>
  BEGIN
    FOR x := 0 TO LAST(fp.byte) DO
      Wr.PutText(wr, Fmt.Pad(Fmt.Unsigned(fp.byte[x], 16), 2, '0'));
    END;
    RETURN TextWr.ToText(wr);
  END FromFp;

PROCEDURE ToFp(t: TEXT): Fingerprint.T RAISES {SmallDB.Failed} =
  VAR
    fp: Fingerprint.T;
    buf: ARRAY [0..(NUMBER(fp.byte)*2)-1] OF CHAR;
    used: INTEGER;
  BEGIN
    IF Text.Length(t) # NUMBER(fp.byte)*2 THEN
      RaiseFailed();
    END;
    Text.SetChars(buf, t);
    FOR x := 0 TO LAST(fp.byte) DO
      fp.byte[x] := Convert.ToUnsigned(SUBARRAY(buf, 2*x, 2), used, 16);
      IF used # 2 THEN RaiseFailed(); END;
    END;
    RETURN fp;
  END ToFp;

PROCEDURE NextCard32(VAR l: RefList.T): CARDINAL RAISES {SmallDB.Failed} =
  VAR r: REF INTEGER := Next(l, TYPECODE(REF INTEGER));
  BEGIN
    IF r^ < 0 THEN RaiseFailed(); END;
    RETURN r^;
  END NextCard32;

PROCEDURE NextCommonText(VAR l: RefList.T): TEXT RAISES {SmallDB.Failed} =
  VAR t: TEXT := Next(l, TYPECODE(TEXT));
  BEGIN
    RETURN Atom.ToText(Atom.FromText(t));
  END NextCommonText;

PROCEDURE NextLongReal(VAR l: RefList.T): LONGREAL RAISES {SmallDB.Failed} =
  VAR res: LONGREAL;
  BEGIN
    IF l = NIL OR l.head = NIL THEN
      RaiseFailed();
    END;
    TYPECASE l.head OF
    | REF INTEGER(x) => res := FLOAT(x^, LONGREAL);
    | REF REAL(x) => res := FLOAT(TRUNC(x^), LONGREAL);
    | REF LONGREAL(x) => res := x^;
    ELSE RaiseFailed();
    END;
    l := l.tail;
    RETURN res;
  END NextLongReal;

PROCEDURE NextFp(VAR l: RefList.T): Fingerprint.T RAISES {SmallDB.Failed} =
  BEGIN
    IF l = NIL OR l.head = NIL THEN RaiseFailed(); END;
    IF TYPECODE(l.head) = TYPECODE(Atom.T) THEN
      VAR h := l.head; BEGIN
        l := l.tail;
        IF h = Sx.False THEN RETURN Fingerprint.Zero; END;
      END;
    END;
    RETURN ToFp(Next(l, TYPECODE(TEXT)));
  END NextFp;

PROCEDURE Next(VAR l: RefList.T; tc: CARDINAL): REFANY
    RAISES {SmallDB.Failed} =
  VAR res: REFANY;
  BEGIN
    IF l = NIL OR l.head = NIL OR tc # TYPECODE(l.head) THEN
      RaiseFailed();
    END;
    res := l.head;
    l := l.tail;
    RETURN res;
  END Next;

PROCEDURE NextEnum(VAR l: RefList.T): RefList.T
    RAISES {SmallDB.Failed} =
  VAR res: REFANY;
  BEGIN
    IF l = NIL THEN RETURN NIL; END;
    res := l.head;
    IF res # NIL AND TYPECODE(res) # TYPECODE(RefList.T) THEN
      RaiseFailed();
    END;
    l := l.tail;
    RETURN res;
  END NextEnum;

PROCEDURE RaiseFailed() RAISES {SmallDB.Failed} =
  BEGIN
    RAISE SmallDB.Failed(AtomList.List1(Atom.FromText("SmallDB.MalformedSx")));
  END RaiseFailed;


BEGIN
END StablePkl.


