(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: StarterScan.m3,v 1.1.1.1 2001-09-19 11:40:10 wagner Exp $ *)

MODULE StarterScan;
IMPORT Starter, StarterList;
IMPORT PaneFrame;
IMPORT Pathname;
IMPORT VBT;
IMPORT Text;
IMPORT Fmt;
IMPORT Rd;
IMPORT Debug;
CONST
  DebugLevel = 90;
TYPE
  Kind = {FromKey, FromPath, FromStarter};
VAR
  UntNo: INTEGER := 0;
  UntMu := NEW(MUTEX);
PROCEDURE Untitled(): TEXT =
  BEGIN
    LOCK UntMu DO
      INC(UntNo);
      RETURN "Untitled-" & Fmt.Int(UntNo);
    END;
  END Untitled;

REVEAL
  T = BRANDED REF RECORD
    kind: Kind;
    key: CHAR := '\000';
    path: TEXT := NIL;
    starter: Starter.T := NIL;
    title: TEXT := NIL;
    rd: Rd.T := NIL;
  END;
PROCEDURE FromKey(key: CHAR; title: TEXT := NIL): T = BEGIN
  RETURN NEW(T, kind := Kind.FromKey, key := key, title := title);END FromKey;
PROCEDURE FromPath(path: TEXT; title: TEXT := NIL): T = BEGIN
  RETURN NEW(T, kind := Kind.FromPath, path:=path, title:=title);END FromPath;
PROCEDURE FromStarter(s: Starter.T; title: TEXT := NIL): T = BEGIN
RETURN NEW(T, kind:=Kind.FromStarter,starter:=s,title:=title);END FromStarter;
PROCEDURE Default(): T = BEGIN RETURN FromStarter(NIL); END Default;
PROCEDURE FromRd(rd: Rd.T; s: Starter.T; path, title: TEXT := NIL): T = BEGIN
  RETURN NEW(T, kind := Kind.FromStarter,starter:=s,
             path := path, title := title, rd := rd); END FromRd;

PROCEDURE GetPath(from: T): TEXT = BEGIN RETURN from.path END GetPath;

PROCEDURE NewFromStarter(override: Starter.T;
                         p, t: TEXT;
                         paneMan: VBT.T;
                         rd: Rd.T := NIL;
                         s: StarterList.T := NIL): PaneFrame.T =
  VAR
    starter := override;
    title := t;
    path := p;
  BEGIN
    IF override = NIL THEN
      starter := s.head;
    END;
    IF title = NIL THEN
      title := Untitled();
    END;
    IF path = NIL THEN
      path := "./";
    END;
    Debug.S("Start frame: Path = " & path, DebugLevel);
    RETURN starter.new().init(path, title, starter.name, paneMan, starter, rd);
  END NewFromStarter;

PROCEDURE NewFromPath(s: StarterList.T;
                      path: TEXT;
                      t: TEXT;
                      paneMan: VBT.T): PaneFrame.T =
  VAR
    title := t;
    cur := s;
    ext := Pathname.LastExt(path);
  BEGIN
    IF title = NIL THEN
      title := Pathname.Last(path);
    END;
    WHILE cur # NIL DO
      IF cur.tail = NIL OR Text.Equal(cur.head.ext, ext) THEN
        RETURN NewFromStarter(cur.head, path, title, paneMan);
      END;
      cur := cur.tail;
    END;
    RETURN NIL;
  END NewFromPath;

PROCEDURE NewFromKey(s: StarterList.T;
                     key: CHAR;
                     title: TEXT;
                     paneMan: VBT.T): PaneFrame.T =
  VAR
    cur := s;
  BEGIN
    WHILE cur # NIL DO
      IF cur.head.key = key THEN
        RETURN NewFromStarter(cur.head, NIL, title, paneMan);
      END;
      cur := cur.tail;
    END;
    RETURN NIL;
  END NewFromKey;

PROCEDURE NewPaneFrame(s: StarterList.T; paneMan: VBT.T;
                       from: T): PaneFrame.T =
  BEGIN
    CASE from.kind OF
    | Kind.FromKey => RETURN NewFromKey(s, from.key, from.title, paneMan);
    | Kind.FromPath => RETURN NewFromPath(s, from.path, from.title, paneMan);
    | Kind.FromStarter => RETURN NewFromStarter(from.starter,
                                                from.path,
                                                from.title,
                                                paneMan,
                                                from.rd, s);
    END;
  END NewPaneFrame;


PROCEDURE Equal(a, b: T): BOOLEAN = BEGIN RETURN a=b; END Equal;
BEGIN
END StarterScan.
