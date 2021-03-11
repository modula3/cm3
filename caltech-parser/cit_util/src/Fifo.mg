(* $Id$ *)

GENERIC MODULE Fifo(Elem);

(* Copyright (c) 2005-2006, Generation Capital Ltd.  All rights reserved. *)
(* Author: Mika Nystrom <mika@gcapltd.com> *)

REVEAL
  T = Public BRANDED Brand OBJECT
    data : Rec;
  OVERRIDES
    init := Init;
    put := Put;
    get := Get;
    peek := Peek;
    empty := Empty;
    member := Member;
    size := Size;
    iterate := Iterate;
  END;

TYPE
  Rec = OBJECT
    txt : Elem.T;
    nxt, prv : Rec;
  END;

PROCEDURE Init(t : T) : T =
  BEGIN
    (* allocate sentinel *)
    t.data := NEW(Rec (*, txt := Elem.NilValue *));
    t.data.nxt := t.data;
    t.data.prv := t.data;
    RETURN t
  END Init;

PROCEDURE Put(t : T; txt : Elem.T) =
  VAR
    new := NEW(Rec, txt := txt);
  BEGIN

    (* put at tail *)
    new.nxt := t.data;
    new.prv := t.data.prv;
    t.data.prv.nxt := new;
    t.data.prv := new
  END Put;

PROCEDURE Get(t : T) : Elem.T =
  VAR
    rec := t.data.nxt;
  BEGIN
    (* get from head *)
    <* ASSERT rec # t.data *>
    
    rec.prv.nxt := rec.nxt;
    rec.nxt.prv := rec.prv;

    (* not really necessary, but... *)
    rec.nxt := NIL;
    rec.prv := NIL;
    
    RETURN rec.txt
  END Get;

PROCEDURE Peek(t : T) : Elem.T =
  VAR
    rec := t.data.nxt;
  BEGIN
    (* peek at head *)
    <* ASSERT rec # t.data *>
    
    RETURN rec.txt
  END Peek;

PROCEDURE Empty(t : T) : BOOLEAN =
  BEGIN RETURN t.data.nxt = t.data END Empty;

PROCEDURE Member(t : T; elem : Elem.T) : BOOLEAN =
  VAR
    p := t.data.nxt;
  BEGIN
    WHILE p # t.data DO
      IF Elem.Equal(elem,p.txt) THEN RETURN TRUE END;
      p := p.nxt
    END;
    RETURN FALSE
  END Member;

PROCEDURE Size(t : T) : CARDINAL =
  VAR
    p := t.data.nxt;
    i : CARDINAL := 0;
  BEGIN
    WHILE p # t.data DO
      INC(i);
      p := p.nxt
    END;
    RETURN i
  END Size;

(**********************************************************************)

REVEAL
  Iterator = PubIterator BRANDED Brand & " Iterator" OBJECT
    t : T;
    p : Rec;
  OVERRIDES
    next := INext;
  END;

PROCEDURE INext(i : Iterator; VAR e : Elem.T) : BOOLEAN =
  BEGIN
    IF i.p = i.t.data THEN 
      RETURN FALSE
    ELSE
      TRY 
        e := i.p.txt;
        RETURN TRUE
      FINALLY
        i.p := i.p.nxt
      END
    END
  END INext;

PROCEDURE Iterate(t : T) : Iterator =
  BEGIN RETURN NEW(Iterator, t := t, p := t.data.nxt) END Iterate;

BEGIN END Fifo.
  


