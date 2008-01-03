(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Aug 19 13:47:51 PDT 1994 by heydon                   *)
(*      modified on Mon Jun 13 15:05:51 PDT 1994 by shillner                 *)

MODULE UFInput;

IMPORT FormsVBT;
IMPORT VBT;
IMPORT Atom, FileRd, Fmt, OSError, Random, Rd, RefList, Sx, Text, Thread;

REVEAL
  T = TPublic BRANDED OBJECT METHODS
    init(fv: FormsVBT.T): T RAISES {Error}
  END;

TYPE
  TRand = T OBJECT
    setCnt, unionCnt: CARDINAL;   (* READONLY after initialization *)
    finishedSets: BOOLEAN;
    currSet, currUnion: CARDINAL;
    rand: Random.T;
  OVERRIDES
    init := InitRand;
    next := NextRand
  END;

  TFile = T OBJECT
    currSet: CARDINAL;
    lastSet: INTEGER;
    finishedSets: BOOLEAN;
    sx: Sx.T
  OVERRIDES
    init := InitFile;
    next := NextFile
  END;

PROCEDURE New(fv: FormsVBT.T): T RAISES {Error} =
  <* FATAL FormsVBT.Error, FormsVBT.Unimplemented *>
  VAR res: T; kind: TEXT; BEGIN
    LOCK VBT.mu DO kind := FormsVBT.GetChoice(fv, "inputFrom") END;
    IF Text.Equal(kind, "randomInput") THEN
      res := NEW(TRand).init(fv)
    ELSIF Text.Equal(kind, "fileInput") THEN
      res := NEW(TFile).init(fv)
    ELSE <* ASSERT FALSE *>
    END;
    RETURN res
  END New;

PROCEDURE InitRand(t: TRand; fv: FormsVBT.T): T =
  <* FATAL FormsVBT.Error, FormsVBT.Unimplemented *>
  VAR fixedSeed: BOOLEAN; BEGIN
    LOCK VBT.mu DO
      t.setCnt := FormsVBT.GetInteger(fv, "setCnt");
      t.unionCnt := FormsVBT.GetInteger(fv, "unionCnt");
      fixedSeed := FormsVBT.GetBoolean(fv, "fixedSeed")
    END;
    t.finishedSets := FALSE;
    t.currSet := 0; t.currUnion := 0;
    t.rand := NEW(Random.Default).init(fixed := fixedSeed);
    RETURN t
  END InitRand;

PROCEDURE NextRand(t: TRand; VAR (*OUT*) cmd: Cmd): BOOLEAN =
  BEGIN
    IF t.currSet < t.setCnt THEN
      cmd := NEW(NewSetCmd, arg1 := Fmt.Int(t.currSet));
      INC(t.currSet)
    ELSIF NOT t.finishedSets THEN
      cmd := NEW(FinishedSetsCmd);
      t.finishedSets := TRUE
    ELSIF t.currUnion < t.unionCnt THEN
      VAR id1, id2: INTEGER;  BEGIN
        id1 := t.rand.integer(min := 0, max := t.setCnt - 1);
        REPEAT
          id2 := t.rand.integer(min := 0, max := t.setCnt - 1)
        UNTIL id2 # id1;
        cmd := NEW(UnionCmd, arg1 := id1, arg2 := id2, bothRoots := FALSE);
        INC(t.currUnion)
      END
    ELSE
      RETURN FALSE
    END;
    RETURN TRUE
  END NextRand;

PROCEDURE InitFile(t: TFile; fv: FormsVBT.T): T RAISES {Error} =
  <* FATAL FormsVBT.Error, FormsVBT.Unimplemented, Thread.Alerted *>
  VAR fileName: TEXT; file: FileRd.T; BEGIN
    LOCK VBT.mu DO fileName := FormsVBT.GetText(fv, "browser") END;
    IF fileName = NIL OR Text.Equal(fileName, "") THEN
      RAISE Error("you must specify an input file")
    END;
    TRY
      file := FileRd.Open(fileName);
      t.sx := Sx.Read(file);
      Rd.Close(file)
    EXCEPT
      OSError.E => RAISE Error("unable to open file \"" & fileName & "\"")
    | Sx.ReadError (msg) => RAISE Error(msg)
    | Rd.EndOfFile => RAISE Error("premature end-of-file")
    | Rd.Failure => RAISE Error("failed to close file reader")
    END;
    t.currSet := 0;
    t.lastSet := -1;
    t.finishedSets := FALSE;
    RETURN t
  END InitFile;

PROCEDURE NextFile(t: TFile; VAR (*OUT*) cmd: Cmd): BOOLEAN RAISES {Error}=
  BEGIN
    IF t.lastSet # -1 THEN
      IF t.currSet < t.lastSet THEN
        cmd := NEW(NewSetCmd, arg1 := Fmt.Int(t.currSet));
        INC(t.currSet);
        RETURN TRUE
      ELSE
        t.lastSet := -1
      END
    END;
    IF t.sx = NIL THEN RETURN FALSE END;
    TYPECASE t.sx OF RefList.T (rl) =>
      cmd := ProcessElt(t, rl.head);
      t.sx := rl.tail
    ELSE RAISE Error("top-level input element is not a list")
    END;
    RETURN TRUE
  END NextFile;

VAR (* CONST *)
  NewSetSym := Atom.FromText("NewSet");
  NewSetsSym := Atom.FromText("NewSets");
  FinishedSetsSym := Atom.FromText("FinishedSets");
  FindSym := Atom.FromText("Find");
  UnionSym := Atom.FromText("Union");

PROCEDURE ProcessElt(t: TFile; sx: Sx.T): Cmd RAISES {Error} =
  VAR cmd: Cmd; BEGIN
    TYPECASE sx OF
      NULL => RAISE Error("empty list")
    | RefList.T (rl) =>
        IF rl.head = NewSetSym THEN
          IF t.finishedSets THEN
            RAISE Error("no new sets allowed after FinishedSets")
          ELSIF RefList.Length(rl) # 2 THEN
            RAISE Error("NewSet expects 1 argument")
          END;
          rl := rl.tail;
          VAR label := ProcessTextArg(rl); BEGIN
            <* ASSERT rl.tail = NIL *>
            cmd := NEW(NewSetCmd, arg1 := label);
            INC(t.currSet)
          END
        ELSIF rl.head = NewSetsSym THEN
          IF t.finishedSets THEN
            RAISE Error("no new sets allowed after FinishedSets")
          ELSIF RefList.Length(rl) # 2 THEN
            RAISE Error("NewSets expects 1 argument")
          END;
          rl := rl.tail;
          VAR num := ProcessIntArg(rl); BEGIN
            <* ASSERT rl.tail = NIL *>
            IF num = 0 THEN RAISE Error("NumSet arg must be positive") END;
            cmd := NEW(NewSetCmd, arg1 := Fmt.Int(t.currSet));
            t.lastSet := t.currSet + num;
            INC(t.currSet)
          END 
        ELSIF rl.head = FinishedSetsSym THEN
          IF rl.tail # NIL THEN
            RAISE Error("FinishedSets expects 0 arguments")
          ELSIF t.finishedSets THEN
            RAISE Error("Too many FinishedSets events")
          END;
          cmd := NEW(FinishedSetsCmd);
          t.finishedSets := TRUE
        ELSIF rl.head = FindSym THEN
          IF NOT t.finishedSets THEN
            RAISE Error("Find not allowed before FinishedSets")
          ELSIF RefList.Length(rl) # 2 THEN
            RAISE Error("Find expects 1 argument")
          END;
          rl := rl.tail;
          VAR num := ProcessIntArg(rl); BEGIN
            <* ASSERT rl.tail = NIL *>
            cmd := NEW(FindCmd, arg1 := num)
          END
        ELSIF rl.head = UnionSym THEN
          IF NOT t.finishedSets THEN
            RAISE Error("Union not allowed before FinishedSets")
          ELSIF RefList.Length(rl) # 4 THEN
            RAISE Error("Union expects 3 arguments")
          END;
          rl := rl.tail;
          VAR num1, num2: CARDINAL; bothRoots: BOOLEAN; BEGIN
            num1 := ProcessIntArg(rl); rl := rl.tail;
            num2 := ProcessIntArg(rl); rl := rl.tail;
            bothRoots := ProcessBoolArg(rl);
            <* ASSERT rl.tail = NIL *>
            cmd := NEW(UnionCmd, arg1 := num1, arg2 := num2,
              bothRoots := bothRoots)
          END
        END
    ELSE RAISE Error("input element is not a list")
    END;
    RETURN cmd
  END ProcessElt;

PROCEDURE ProcessTextArg(args: RefList.T): TEXT RAISES {Error} =
  VAR res: TEXT; BEGIN
    TYPECASE args.head OF
      NULL => RAISE Error("expecting text argument")
    | TEXT (t) => res := t
    ELSE RAISE Error("argument should be a text")
    END;
    RETURN res
  END ProcessTextArg;

PROCEDURE ProcessIntArg(args: RefList.T): CARDINAL RAISES {Error} =
  VAR res: INTEGER; BEGIN
    TYPECASE args.head OF
      NULL => RAISE Error("expecting integer argument")
    | REF INTEGER (i) => res := i^
    ELSE RAISE Error("argument should be an integer")
    END;
    IF res < 0 THEN RAISE Error("argument must be non-negative") END;
    RETURN res
  END ProcessIntArg;

VAR (* CONST *)
  TrueSym, FalseSym: Atom.T;

PROCEDURE ProcessBoolArg(args: RefList.T): BOOLEAN RAISES {Error} =
  VAR res: BOOLEAN; BEGIN
    TYPECASE args.head OF
      NULL => RAISE Error("expecting Boolean argument")
    | Atom.T (a) =>
        IF a = TrueSym THEN res := TRUE
        ELSIF a = FalseSym THEN res := FALSE
        ELSE RAISE Error("expecting \"TRUE\" or \"FALSE\"")
        END
    ELSE RAISE Error("argument should be a Boolean")
    END;
    RETURN res
  END ProcessBoolArg;

BEGIN
  TrueSym := Atom.FromText("TRUE");
  FalseSym := Atom.FromText("FALSE")
END UFInput.
