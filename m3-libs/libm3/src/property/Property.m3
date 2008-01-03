(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
 
(* Last modified on Thu Sep 19 18:56:30 1991 by kalsow     *)
(*      modified on Fri Jan 18  9:35:26 PST 1991 by mjordan    *)

MODULE Property EXPORTS Property, PropertyF;

IMPORT RTType;

TYPE
  SetElements = REF ARRAY OF REFANY;
REVEAL
  Set = Set_public BRANDED OBJECT
    s: SetElements := NIL;
  OVERRIDES
    put := Put; get := Get; remove := Remove;
    getSub := GetSub; removeSub := RemoveSub;
  END;

CONST
  InitSize = 4; (* must exceed 0 *)

EXCEPTION FatalError;
<*FATAL FatalError*>

PROCEDURE New(): Set=
  BEGIN
    RETURN NEW(Set);
  END New;

PROCEDURE Put(ps: Set; r: REFANY) =
  BEGIN
    IF r = NIL THEN RAISE FatalError END;
    IF ps.s = NIL THEN
      ps.s := NEW(SetElements, InitSize);
      ps.s[0] := r;
      RETURN
    END;
    WITH tc = TYPECODE(r) DO
      FOR i := 0 TO InitSize - 1 DO
        WITH ref = ps.s[i] DO
          IF (ref = NIL) OR (TYPECODE(ref) = tc) THEN
            ref := r;
            RETURN
          END
        END
      END;
      IF MoveToFront(ps, tc) THEN
        (* The entry for tc is now at the front of the list *)
        ps.s[0] := r;
        RETURN
      END
    END;
    (* need to create a new entry *)
    VAR ps_sP: SetElements; i := NUMBER(ps.s^);
    BEGIN
      WHILE ps.s[i - 1] = NIL DO DEC(i) END;
      IF i = NUMBER(ps.s^) THEN
        ps_sP := NEW(SetElements, 2 * NUMBER(ps.s^));
        FOR i := 0 TO NUMBER(ps.s^) -1  DO ps_sP[i] := ps.s[i] END;
        FOR i := NUMBER(ps.s^) TO NUMBER(ps_sP^) -1 DO ps_sP[i] := NIL END;
        i := NUMBER(ps.s^);
        ps.s := ps_sP
      END;
      ps.s[i] := r
    END
  END Put;

PROCEDURE Remove(ps: Set; tc: CARDINAL) =
  BEGIN RemoveP(ps, tc); END Remove;

PROCEDURE RemoveSub(ps: Set; tc: CARDINAL) =
  BEGIN RemoveP(ps, tc, sub := TRUE); END RemoveSub;

PROCEDURE RemoveP(ps: Set; tc: CARDINAL; sub := FALSE) =
  VAR i: INTEGER; t, u: REFANY; 
  BEGIN
    IF ps.s # NIL AND
       (RTType.IsSubtype(TYPECODE(ps.s[0]), tc) OR 
        MoveToFront(ps, tc, sub)) THEN
      i := NUMBER(ps.s^) - 1;
      WHILE ps.s[i] = NIL DO DEC(i) END;
      IF i = 0 THEN ps.s := NIL; RETURN END;
      t := ps.s[i];
      ps.s[i] := NIL;
      WHILE i # 0 DO
        i := (i - 1) DIV 2;
        u := ps.s[i];
        ps.s[i] := t; 
        t := u
      END
    END
  END RemoveP;

PROCEDURE Get(ps: Set; tc: CARDINAL): REFANY =
  BEGIN
    IF ps.s = NIL THEN RETURN NIL END;
    FOR i := 0 TO InitSize - 1 DO
      WITH ref = ps.s[i] DO
        IF (ref = NIL) OR (TYPECODE(ref) = tc) THEN RETURN ref END
      END
    END;
    IF MoveToFront(ps, tc) THEN
      RETURN ps.s[0]
    ELSE
      RETURN NIL
    END
  END Get;

PROCEDURE GetSub(ps: Set; tc: CARDINAL): REFANY =
  BEGIN
    IF ps.s = NIL THEN RETURN NIL END;
    FOR i := 0 TO InitSize - 1 DO
      WITH ref = ps.s[i] DO
        IF (ref = NIL) OR RTType.IsSubtype(TYPECODE(ref),  tc) THEN 
          RETURN ref 
        END
      END
    END;
    IF MoveToFront(ps, tc, sub := TRUE) THEN
      RETURN ps.s[0]
    ELSE
      RETURN NIL
    END
  END GetSub;

PROCEDURE MoveToFront(ps: Set; tc: CARDINAL; sub := FALSE): BOOLEAN=
  (* Move the tc entry of the set ps.s to the front and return TRUE, if it
     exists. Return FALSE otherwise. ps.s # NIL. *)
  VAR i, j: INTEGER; ref: REFANY;  
  BEGIN
    IF ps.s = NIL THEN RETURN FALSE END;
    i := 0;
    LOOP
      IF i = NUMBER(ps.s^) THEN RETURN FALSE END;
      ref := ps.s[i];
      IF ref = NIL THEN RETURN FALSE END;
      IF sub THEN
        IF RTType.IsSubtype(TYPECODE(ref), tc) THEN EXIT END;
      ELSE
        (* covered by IsSubtype but faster path *)
        IF TYPECODE(ref) = tc THEN EXIT END;
      END;
      INC(i)
    END;
    WHILE i # 0 DO j := (i - 1) DIV 2; ps.s[i] := ps.s[j]; i := j END;
    ps.s[0] := ref;
    RETURN TRUE
  END MoveToFront;

BEGIN
END Property.
