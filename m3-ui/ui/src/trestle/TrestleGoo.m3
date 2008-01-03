(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Oct 15 16:14:04 PDT 1992 by msm     *)
<*PRAGMA LL*>

(* A mechanism for attaching arbitrary sets of properties to an installed
   window that can be set and retrieved by a TrestleClass.  The interface
   is like VBT's property set, but supports enumeration. *)

MODULE TrestleGoo;

IMPORT VBT;

VAR mu := NEW(MUTEX);

TYPE AliasRef = BRANDED REF RECORD alias: VBT.T END;

PROCEDURE Alias(v, ch: VBT.T) =
  VAR al := NEW(AliasRef, alias := ch); BEGIN
    VBT.PutProp(v, al)
  END Alias;

PROCEDURE TrueChild (v: VBT.T): VBT.T =
  BEGIN
    LOOP
      VAR al: AliasRef := VBT.GetProp(v, TYPECODE(AliasRef));
      BEGIN
        IF al = NIL THEN RETURN v END;
        v := al.alias
      END
    END
  END TrueChild;

PROCEDURE PutProp (vv: VBT.T; ref: REFANY) = <* LL.sup < v *>
  BEGIN
    LOCK mu DO
      VAR
        v        := TrueChild(vv);
        tc       := TYPECODE(ref);
        p : Enum := VBT.GetProp(v, TYPECODE(Enum));
        e        := NEW(Enum, prop := ref, next := p);
      BEGIN
        VBT.PutProp(v, e);
        WHILE e.next # NIL DO
          IF TYPECODE(e.next.prop) = tc THEN
            e.next := e.next.next
          ELSE
            e := e.next
          END
        END
      END
    END
  END PutProp;

PROCEDURE GetProp (vv: VBT.T; tc: INTEGER): REFANY =
  BEGIN
    LOCK mu DO
      VAR
        v       := TrueChild(vv);
        e: Enum := VBT.GetProp(v, TYPECODE(Enum));
      BEGIN
        WHILE e # NIL DO
          IF TYPECODE(e.prop) = tc THEN RETURN e.prop ELSE e := e.next END
        END;
        RETURN NIL
      END
    END
  END GetProp;

PROCEDURE RemProp (vv: VBT.T; tc: INTEGER) = <* LL.sup < v *>
  BEGIN
    LOCK mu DO
      VAR
        v       := TrueChild(vv);
        e: Enum := VBT.GetProp(v, TYPECODE(Enum));
      BEGIN
        IF e = NIL THEN RETURN END;
        IF TYPECODE(e.prop) = tc THEN
          IF e.next = NIL THEN
            VBT.RemProp(v, TYPECODE(Enum))
          ELSE
            VBT.PutProp(v, e.next)
          END
        ELSE
          LOOP
            IF e.next = NIL THEN EXIT END;
            IF TYPECODE(e.next.prop) = tc THEN
              e.next := e.next.next;
              EXIT
            END;
            e := e.next
          END
        END
      END
    END
  END RemProp;

REVEAL Enum = BRANDED REF RECORD next: Enum; prop: REFANY END;

PROCEDURE Next (v: VBT.T; VAR enum: Enum): REFANY =
  BEGIN
    LOCK mu DO
      IF enum = NIL THEN
        enum := VBT.GetProp(TrueChild(v), TYPECODE(Enum))
      ELSE
        enum := enum.next
      END;
      IF enum = NIL THEN RETURN NIL ELSE RETURN enum.prop END
    END
  END Next;

BEGIN
END TrestleGoo.


