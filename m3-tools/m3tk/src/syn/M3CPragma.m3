(***************************************************************************)
(*                      Copyright (C) Olivetti 1989                        *)
(*                          All Rights reserved                            *)
(*                                                                         *)
(* Use and copy of this software and preparation of derivative works based *)
(* upon this software are permitted to any person, provided this same      *)
(* copyright notice and the following Olivetti warranty disclaimer are     *) 
(* included in any copy of the software or any modification thereof or     *)
(* derivative work therefrom made by any person.                           *)
(*                                                                         *)
(* This software is made available AS IS and Olivetti disclaims all        *)
(* warranties with respect to this software, whether expressed or implied  *)
(* under any law, including all implied warranties of merchantibility and  *)
(* fitness for any purpose. In no event shall Olivetti be liable for any   *)
(* damages whatsoever resulting from loss of use, data or profits or       *)
(* otherwise arising out of or in connection with the use or performance   *)
(* of this software.                                                       *)
(***************************************************************************)
(**)
(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE M3CPragma EXPORTS M3CPragma, M3CPragmaF;

IMPORT Text, TextExtras, ASCII;
IMPORT M3AST_AS;
IMPORT M3CSrcPos;

(**IMPORT M3AST_AS_F;**)

PROCEDURE NewStore(): Store RAISES {}=
  BEGIN
    RETURN NEW(Store);
  END NewStore;

 
PROCEDURE AddToStore(
    body: Text.T;
    pos: M3CSrcPos.T;
    precedingNode: M3AST_AS.SRC_NODE;
    VAR store: Store)
    : T
    RAISES {}=
  VAR
    new := NEW(Iter, pos := pos, body := body,
        precedingNode := precedingNode);
  BEGIN
    WITH last = store.last DO
      IF last = NIL THEN
        store.first := new;
      ELSE
        last.next := new;
        new.prev := last;
      END;
      last := new;
    END;
    RETURN new;
  END AddToStore;


PROCEDURE AddFollowingNode(
    followingNode: M3AST_AS.SRC_NODE;
    store: Store)
    RAISES {}=
  VAR
    t := store.last;
    precedingNode := t.precedingNode;
  BEGIN
    REPEAT
      t.followingNode := followingNode;
      t := t.prev;
    UNTIL t = NIL OR t.precedingNode # precedingNode;
  END AddFollowingNode;


PROCEDURE AddPrecedingStmOrDecl(
    stmOrDecl: M3AST_AS.SRC_NODE;
    store: Store)
    RAISES {}=
  VAR
    t := store.last;
    precedingNode := t.precedingNode;
  BEGIN
    WHILE t # NIL AND t.precedingNode = precedingNode AND
        t.precedingStmOrDecl = NIL DO
      t.precedingStmOrDecl := stmOrDecl;
      t := t.prev;
    END;
  END AddPrecedingStmOrDecl;


PROCEDURE NewIter(ps: Store; after := M3CSrcPos.Null): Iter RAISES {}=
  BEGIN
    IF ps = NIL THEN RETURN NIL END;
    IF after = M3CSrcPos.Null OR ps.first = NIL OR
        M3CSrcPos.Compare(ps.first.pos, after) > 0 THEN
      RETURN ps.first;
    ELSE
      VAR
        t := ps.last;
      BEGIN
        LOOP
          IF M3CSrcPos.Compare(t.pos, after) <= 0 THEN
            RETURN t.next;
          ELSE
            t := t.prev;
          END;
        END;
      END;
    END;
  END NewIter;

 
PROCEDURE Next(VAR iter: Iter; VAR t: T): BOOLEAN RAISES {}=
  BEGIN
    IF iter = NIL THEN RETURN FALSE END;
    t := iter;
    iter := iter.next;
    RETURN TRUE;
  END Next;


PROCEDURE Position(t: T): M3CSrcPos.T RAISES {}=
  BEGIN
    RETURN t.pos;
  END Position;

 
PROCEDURE Body(t: T): Text.T RAISES {}=
  BEGIN
    RETURN t.body;
  END Body;

 
PROCEDURE Match(t: T; keyword: Text.T; VAR args: Text.T): BOOLEAN RAISES {}=
  VAR
    body := t.body;
    pos: CARDINAL := 2;
    length := Text.Length(keyword);
    startOfClosingBracket := Text.Length(body) - 2;
  BEGIN
    IF TextExtras.FindCharSet(body, ASCII.All - ASCII.Spaces, pos) THEN
      IF pos + length > startOfClosingBracket THEN RETURN FALSE END;
      FOR i := 0 TO length - 1 DO
        IF Text.GetChar(keyword, i) # Text.GetChar(body, pos + i) THEN
          RETURN FALSE;
        END;
      END;
      INC(pos, length);
      IF pos = startOfClosingBracket THEN
        args := NIL;
        RETURN TRUE;
      ELSIF NOT Text.GetChar(body, pos) IN ASCII.Spaces THEN
        RETURN FALSE
      ELSE
        INC(pos);
        IF TextExtras.FindCharSet(body, ASCII.All - ASCII.Spaces, pos) THEN
          IF pos >= startOfClosingBracket THEN
            args := NIL;
          ELSE
            VAR
              end := startOfClosingBracket - 1;
            BEGIN
              WHILE Text.GetChar(body, end) IN ASCII.Spaces DO
                DEC(end);
              END;
              args := TextExtras.Extract(body, pos, end + 1);
            END;
          END; (* if *)
        END;
        RETURN TRUE;
      END; (* if *)
    ELSE
      RETURN FALSE;
    END; (* if *)
  END Match;


PROCEDURE Hook(t: T): REFANY RAISES {}=
  BEGIN
    RETURN t.hook;
  END Hook;


PROCEDURE SetHook(t: T; hook: REFANY) RAISES {}=
  BEGIN
    t.hook := hook;
  END SetHook;


PROCEDURE AfterNode(
    store: Store;
    node: M3AST_AS.SRC_NODE)
    : Iter
    RAISES {}=
  BEGIN
    IF store = NIL THEN RETURN NIL END;
    VAR
      search := store.first;
    BEGIN
      WHILE search # NIL DO
        IF search.precedingNode = node THEN RETURN search END;
        search := search.next;
      END;
      RETURN NIL;
    END;
  END AfterNode;

 
PROCEDURE BeforeNode(
    store: Store;
    node: M3AST_AS.SRC_NODE)
    : Iter
    RAISES {}=
  BEGIN
    IF store = NIL THEN RETURN NIL END;
    VAR
      search := store.first;
    BEGIN
      WHILE search # NIL DO
        IF search.followingNode = node THEN RETURN search END;
        search := search.next;
      END;
      RETURN NIL;
    END;
  END BeforeNode;

 
PROCEDURE AfterStmOrDecl(
    store: Store;
    stmOrDecl: M3AST_AS.SRC_NODE)
    : Iter
    RAISES {}=
  BEGIN
    IF store = NIL THEN RETURN NIL END;
    VAR
      search := store.first;
    BEGIN
      WHILE search # NIL DO
        IF search.precedingStmOrDecl = stmOrDecl THEN RETURN search END;
        search := search.next;
      END;
      RETURN NIL;
    END;
  END AfterStmOrDecl;

 
PROCEDURE PrecedingNode(t: T): M3AST_AS.SRC_NODE RAISES {}=
  BEGIN
    RETURN t.precedingNode;
  END PrecedingNode;

 
PROCEDURE FollowingNode(t: T): M3AST_AS.SRC_NODE RAISES {}=
  BEGIN
    RETURN t.followingNode;
  END FollowingNode;

 
PROCEDURE PrecedingStmOrDecl(t: T): M3AST_AS.SRC_NODE RAISES {}=
  BEGIN
    RETURN t.precedingStmOrDecl;
  END PrecedingStmOrDecl;


BEGIN

END M3CPragma.
