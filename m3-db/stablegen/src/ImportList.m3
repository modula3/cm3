(* Copyright (C) 1989, Digital Equipment Corporation               *)
(* All rights reserved.                                            *)
(* See the file COPYRIGHT for a full description.                  *)
(* Created by Susan Owicki, rewritten by Carsten Weich             *)
(* Last modified on Fri Jan  6 12:48:02 PST 1995 by chaiken        *)
(*      modified on Fri Sep 23 14:48:47 PDT 1994 by weich          *)

(* The module contains procedures to gain necessary imports to compile a
   type. *)

MODULE ImportList;

IMPORT Atom, AtomRefTbl, Type;

(* \subsection{Producing import lists} The proceuderes "Import..." maintain
   a list modules necessary to represent a type.  This list is a
   "AtomRefTbl.T" which is only used in a set like fashion.  There are no
   entries in the table ("NIL" is inserted together with the interface).

   The main module called from outside is "FromType()".
*)
REVEAL T = AtomRefTbl.Default BRANDED OBJECT END;

PROCEDURE FromType (type: Type.Object; methods: MethodList): T =
  VAR
    obj      : Type.Reference := type;
    importTbl: T              := NEW(T).init();
  BEGIN
    IF type.name # NIL AND type.name.intf # nullAtm THEN
      EVAL importTbl.put(type.name.intf, NIL);
    END;
    ImportRevelations(type, importTbl);
    FOR i := 0 TO LAST(methods^) DO
      WITH sig = methods[i].sig DO
        ImportFromSig(sig, importTbl);
        ImportRefsFromSig(sig, importTbl);
        (* Hmm.  StableData.m3 will not actually handle the exceptions.
        FOR j := 0 TO LAST(sig.raises^) DO
          ImportFromType(sig.raises[j].arg, importTbl);
        END;
        *)
      END                        (*WITH*)
    END;                         (*FOR*)
    WHILE obj # NIL AND ISTYPE(obj, Type.Object) DO
      IF obj.revIntf # NIL THEN EVAL importTbl.put(obj.revIntf, NIL); END;
      obj := NARROW(obj, Type.Object).super;
    END;
    RETURN importTbl;
  END FromType;


PROCEDURE ImportRevelations (t: Type.Reference; importTbl: T) =
  (* Add to the table "importTbl" the names of interfaces that provide
     revelations of "t" or one of its supertypes. *)
  BEGIN
    IF t.revIntf # NIL THEN EVAL importTbl.put(t.revIntf, NIL); END;
    TYPECASE t OF
    | Type.Object (obj) =>
        VAR o := obj;
        BEGIN
          WHILE TRUE DO
            TYPECASE o.super OF
            | Type.Opaque => EXIT
            | Type.Object =>
                IF o.revIntf # NIL THEN
                  EVAL importTbl.put(o.revIntf, NIL);
                END;
                o := o.super;
            ELSE
              <*ASSERT FALSE*>
            END;
          END;
        END;
    ELSE
    END;
  END ImportRevelations;


PROCEDURE ImportFromType (t: Type.T; importTbl: T; byName: BOOLEAN := TRUE) =
  BEGIN
    IF t = NIL THEN RETURN END;
    IF t.name # NIL AND t.name.intf # nullAtm THEN
      EVAL importTbl.put(t.name.intf, NIL);
    END;
    TYPECASE t OF
    | Type.Reference (ref) => ImportRevelations(ref, importTbl);
    ELSE
    END;
    IF byName AND t.name # NIL THEN RETURN; END;
    TYPECASE t OF
    | Type.Enumeration, Type.UserDefined, Type.Real, Type.LongReal,
        Type.Extended =>
    | Type.Subrange (sr) => ImportFromType(sr.base, importTbl);
    | Type.Object (ob) =>
        ImportFromFields(ob.fields, importTbl);
        FOR i := 0 TO LAST(ob.methods^) DO
          ImportFromSig(ob.methods[i].sig, importTbl);
        END;
        IF ob.revIntf # NIL THEN EVAL importTbl.put(ob.revIntf, NIL); END;
    | Type.Ref (r) => ImportFromType(r.target, importTbl);
    | Type.Opaque (op) => ImportFromType(op.revealedSuperType, importTbl);
    | Type.Array (a) =>
        ImportFromType(a.index, importTbl);
        ImportFromType(a.element, importTbl);
    | Type.Packed (p) => ImportFromType(p.base, importTbl);
    | Type.Record (rec) => ImportFromFields(rec.fields, importTbl);
    | Type.Set (s) => ImportFromType(s.range, importTbl);
    | Type.Procedure (p) => ImportFromSig(p.sig, importTbl);
    ELSE
      <*ASSERT FALSE*>
    END;
  END ImportFromType;

PROCEDURE ImportFromFields (fields: REF ARRAY OF Type.Field; importTbl: T) =
  BEGIN
    IF fields = NIL THEN RETURN END;
    FOR i := 0 TO LAST(fields^) DO
      ImportFromType(fields[i].type, importTbl);
    END;
  END ImportFromFields;

PROCEDURE ImportFromSig (sig: Type.Signature; importTbl: T) =
  BEGIN
    FOR i := 0 TO LAST(sig.formals^) DO
      ImportFromType(sig.formals[i].type, importTbl);
    END;
    ImportFromType(sig.result, importTbl);
    IF sig.raises # NIL THEN
      FOR i := 0 TO LAST(sig.raises^) DO
        EVAL importTbl.put(sig.raises[i].qid.intf, NIL);
      END;
    END;
  END ImportFromSig;

PROCEDURE ImportRefsFromSig (sig: Type.Signature; importTbl: T) =
  BEGIN
    FOR i := 0 TO LAST(sig.formals^) DO
      ImportRefsFromType(sig.formals[i].type, importTbl);
    END;
    ImportRefsFromType(sig.result, importTbl);
  END ImportRefsFromSig;

PROCEDURE ImportRefsFromType (t: Type.T; importTbl: T) =
  BEGIN
    IF t = NIL THEN RETURN END;
    TYPECASE t OF
    | Type.Enumeration, Type.UserDefined, Type.Real, Type.LongReal,
        Type.Extended, Type.Subrange, Type.Set, Type.Procedure =>
    | Type.Reference =>
        IF NOT Type.MayBeRefAny(t) AND Type.NamedType(t) THEN
          ImportFromType(t, importTbl)
        END;
    | Type.Array (a) =>
        ImportFromType(a.index, importTbl);
        ImportRefsFromType(a.element, importTbl);
    | Type.Packed (p) => ImportRefsFromType(p.base, importTbl);
    | Type.Record (rec) =>
        IF rec.fields = NIL THEN RETURN END;
        FOR i := 0 TO LAST(rec.fields^) DO
          ImportRefsFromType(rec.fields[i].type, importTbl);
        END;
    ELSE
      <*ASSERT FALSE*>
    END;
  END ImportRefsFromType;


(* \subsection{Procedure Add} Add an element "intf" to a import list. *)
PROCEDURE Add (importList: T; intf: Atom.T) =
  BEGIN
    EVAL importList.put(intf, NIL)
  END Add;


(* \subsection{Procedure ToText} Generate a comma separated list of
   interface names out of "imports".  The output is suitable for an
   "IMPORT" statement.

   Iterate over the table "imports" and append to "result".
*)
PROCEDURE ToText (imports: T): TEXT =
  VAR
    intf  : Atom.T;
    dummy : REFANY;
    iter           := imports.iterate();
    result         := "";
  BEGIN
    IF iter.next(intf, dummy) THEN
      result := Atom.ToText(intf);
      WHILE iter.next(intf, dummy) DO
        result := result & ", " & Atom.ToText(intf);
      END;
    END;
    RETURN result;
  END ToText;


VAR nullAtm := Atom.FromText("");

BEGIN
END ImportList.
