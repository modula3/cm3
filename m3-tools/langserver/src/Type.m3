(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Type.m3                                             *)
(* Last Modified On Mon Feb 28 16:52:17 PST 1994 by wobber     *)
(*      Modified On Fri Jun 11 16:48:09 PDT 1993 by owicki     *)
(*      Modified On Mon May 17 14:03:42 PDT 1993 by mjordan    *)
(*      Modified On Fri Feb  2 10:35:09 PST 1990 by gnelson    *)
(*      Modified On Wed Dec 20 18:07:50 1989 by kalsow         *)

MODULE Type;

IMPORT Atom, Fmt, Text, Value, ValueProc;

TYPE
  Foo = OBJECT
          name      : Qid;
          visited   := FALSE;
          brandsOK  := TRUE;
        END;

REVEAL T =  Foo BRANDED OBJECT END;

PROCEDURE ToText (t: T; byName: BOOLEAN := TRUE): Text.T =
  VAR text: Text.T;
  BEGIN
    IF t = NIL THEN RETURN "" END;
    IF t.name # NIL AND byName THEN RETURN QidToText(t.name); END;
    t.visited := TRUE;
    TYPECASE t OF
    | Char => RETURN "CHAR"
    | WideChar => RETURN "WIDECHAR"
    | UserDefined (ud) =>
      IF NUMBER(ud.elts^) = 0 THEN text := "";
      ELSE 
        text := Atom.ToText(ud.elts[0]);
        FOR i := 1 TO LAST(ud.elts^) DO
          text := text & ", " & Atom.ToText(ud.elts[i]);
        END;
      END;
      RETURN "{" & text & "}";
    | Enumeration (enum) =>
      IF enum = boolean THEN RETURN "BOOLEAN"; END;
      RETURN "Type.ToText: unsupported enumeration type";
    | Subrange (sub) =>
      VAR
        min, max: INTEGER;
        ud      : UserDefined;
      BEGIN
        IF sub = integer THEN RETURN "INTEGER" END;
        IF sub = longint THEN RETURN "LONGINT" END;
        IF sub.base = longint THEN
          WITH min = NARROW(sub.min, Value.Longint).val,
               max = NARROW(sub.max, Value.Longint).val DO
            RETURN "[" & Fmt.LongInt(min) & "L.." & Fmt.LongInt(max) & "L]";
          END;
        END;
        min := NARROW(sub.min, Value.Ordinal).ord;
        max := NARROW(sub.max, Value.Ordinal).ord;
        IF sub.base = integer THEN
          RETURN "[" & Fmt.Int(min) & ".." & Fmt.Int(max) & "]"
        END;
        IF sub.base = char THEN RETURN
          "[VAL(" & Fmt.Int(min) &  ", CHAR) .. VAL(" &
            Fmt.Int(max) & ", CHAR)]"
        END;
        IF sub.base = widechar THEN RETURN
          "[VAL(" & Fmt.Int(min) &  ", WIDECHAR) .. VAL(" & 
            Fmt.Int(max) & ", WIDECHAR)]"
        END;
        ud := NARROW(sub.base, UserDefined);
        RETURN "[" & ToText(sub.base) & "." & Atom.ToText(ud.elts[min]) & 
               ".." & ToText(sub.base) & "." & Atom.ToText(ud.elts[max]) & 
               "]";
      END;
    | Real => RETURN "REAL";
    | LongReal => RETURN "LONGREAL";
    | Extended => RETURN "EXTENDED";
    | Reference (ref) =>
      TYPECASE ref OF
      | Opaque (o) =>
        (* Type can only be displayed by name *)
        RETURN "***Error*** Opaque type only printed by name: "
        & "supertype " & ToText(o.revealedSuperType);
        (* RETURN Atom.ToText(t.name.intf) & "." & Atom.ToText(t.name.item)*)
      | Object, Ref =>
        IF ref.brand # NIL THEN
          text := "BRANDED \"" & Atom.ToText(ref.brand) & "\" ";
        ELSE
          text := ""
        END;
        TYPECASE ref OF
        | Object(o) => 
(* 2 newlines "\\n\\n" for new paragraph and 2 spaces and a newline "  \\n"
   for a newline *)
          RETURN ToText(o.super) & " " & text & "OBJECT  \\n" &
                 FieldsToText(o, o.fields) &
                 "  \\nMETHODS  \\n" & MethodsToText(t, o.methods) & "  \\nEND";
        | Ref (r) => 
          IF NOT r.traced THEN text := "UNTRACED " & text END;
          RETURN text & "REF " & ToText(r.target, TRUE);
        ELSE
          RETURN "Type.ToText: unsupported reference type";
        END;
      ELSE
        RETURN "Type.ToText: unsupported reference type";
      END;
    | Array (arr) =>
      IF arr.index = NIL THEN
        text := "";
      ELSE
        text := ToText(arr.index);
      END;
      RETURN "ARRAY " & text & " OF " & ToText(arr.element);
    | Packed (p) => RETURN "BITS " & Fmt.Int(p.size) & " FOR " & ToText(p.base);
    | Record (rec) => RETURN "RECORD " & FieldsToText(rec, rec.fields) & " END";
    | Set (set) => RETURN "SET OF " & ToText(set.range);
    | Procedure (proc) => RETURN "PROCEDURE" & SigToText(t, proc.sig);
    ELSE
      RETURN "Type.ToText: unsupported type";
    END;
  END ToText;

PROCEDURE QidToText (qid: Qid): TEXT =
  BEGIN
    IF qid.intf = nullAtm THEN RETURN Atom.ToText(qid.item)
    ELSE RETURN Atom.ToText(qid.intf) & "." & Atom.ToText(qid.item) 
    END;
  END QidToText;

PROCEDURE SigToText (t : T; sig: Signature): TEXT =
  VAR
    result: TEXT;
    raises: TEXT;
  BEGIN
    IF sig.result # NIL THEN
      IF NOT sig.result.visited THEN
        result := ": " & ToText(sig.result);
      ELSE
        result := ":::";
      END;
    ELSE
      result := "";
    END;
    IF sig.raises = NIL THEN
      raises := " RAISES ANY"
    ELSIF NUMBER(sig.raises^) = 0 THEN
      raises := "";
    ELSE
      raises := " RAISES {" & QidToText(sig.raises[0].qid);
      FOR i := 1 TO LAST(sig.raises^) DO
        raises := raises & ", " & QidToText(sig.raises[i].qid);
      END;
      raises := raises & "}";
    END;
    RETURN "(" & FormalsToText(sig.formals) & ")" & result & raises;
  END SigToText;

PROCEDURE FieldsToText (p : T; f: REF ARRAY OF Field): TEXT =
  VAR
    notFirst := FALSE;
    text     := "";
  BEGIN
    FOR i := 0 TO LAST(f^) DO
      IF notFirst THEN text := text & "; "; END;
      notFirst := TRUE;
      IF NOT f[i].type.visited THEN
        text := text & Atom.ToText(f[i].name)  & ": " & ToText(f[i].type);
      ELSE
        text := text & Atom.ToText(f[i].name);
 (* recursive type just display name & QidToText(f[i].type.name) *)
      END;
      IF f[i].default # NIL THEN
        text := text & ":= " & ValueProc.ToText(f[i].default, f[i].type);
      END;
      text := text & "  \\n";
    END;
    RETURN text;
  END FieldsToText;

PROCEDURE MethodsToText (t : T; m: REF ARRAY OF Method): TEXT =
  VAR
    notFirst := FALSE;
    text     := "";
  BEGIN
    FOR i := 0 TO LAST(m^) DO
      IF notFirst THEN text := text & ";\\n"; END;
      notFirst := TRUE;
      text := text & Atom.ToText(m[i].name) & SigToText(t, m[i].sig);
      IF m[i].default # NIL THEN
        text := text & ":= ";
        TYPECASE m[i].default OF
        | MethodDefault1 (md1) =>
          text := text & QidToText(md1.qid);
        | MethodDefault2 (md2) =>
          text := text & ToText(md2.obType) & "." & Atom.ToText(md2.method);
        ELSE
          RETURN "Type.MethodsToText: unrecognized method value";
        END;
      END;
    END;
    RETURN text;
  END MethodsToText;

PROCEDURE FormalsToText (f: REF ARRAY OF Formal): TEXT =
  VAR
    notFirst       := FALSE;
    text           := "";
    modeName       := ARRAY Mode OF TEXT{"", "VAR ", "READONLY "};
    outPrag : TEXT;
  BEGIN
    FOR i := 0 TO LAST(f^) DO
      IF notFirst THEN text := text & "; "; END;
      notFirst := TRUE;
      IF f[i].outOnly THEN outPrag := "<*OUT*> " ELSE outPrag := "" END;
      (* check it formals can be recursive types *)
      text := text & modeName[f[i].mode] & " " & outPrag &
                Atom.ToText(f[i].name) & ": " & ToText(f[i].type);
      IF f[i].default # NIL THEN
        text := text & ":= " & ValueProc.ToText(f[i].default, f[i].type);
      END;
    END;
    RETURN text;
  END FormalsToText;

PROCEDURE MayBeRefAny (t: T): BOOLEAN =
  BEGIN
    IF t = refany THEN RETURN TRUE; END;
    TYPECASE t OF
    | Ref, Object => RETURN FALSE;
    | Opaque (o) => RETURN MayBeRefAny(o.revealedSuperType);
    ELSE RETURN FALSE (* e.g. type TEXT *)
    END;
  END MayBeRefAny;

PROCEDURE NamedType (t: T): BOOLEAN =
  BEGIN
    RETURN t.name # NIL;
  END NamedType;

(*
PROCEDURE Size (t: T): INTEGER =
  BEGIN
  END Size;

PROCEDURE MinSize (t: T): INTEGER =
  BEGIN
  END MinSize;

PROCEDURE Alignment (t: T): INTEGER =
  BEGIN
  END Alignment;

PROCEDURE Bounds (t: T): Interval.T =
  BEGIN
  END Bounds;

PROCEDURE Base (t: T): T =
  BEGIN
  END Base;

PROCEDURE IsTraced (t: T): BOOLEAN =
  BEGIN
  END IsTraced;

PROCEDURE IsEmpty (t: T): BOOLEAN =
  BEGIN
  END IsEmpty;

*)

VAR nullAtm: Atom.T;

BEGIN
  nullAtm := Atom.FromText("");
  integer := NEW(Subrange, name := NEW(Qid, intf := nullAtm,
                                       item := Atom.FromText("INTEGER")),
                 min := NEW(Value.Ordinal, ord := FIRST(INTEGER)),
                 max := NEW(Value.Ordinal, ord := LAST(INTEGER)));
  integer.base := integer;
  longint := NEW(Subrange, name := NEW(Qid, intf := nullAtm,
                                       item := Atom.FromText("LONGINT")),
                 min := NEW(Value.Longint, val := FIRST(LONGINT)),
                 max := NEW(Value.Longint, val := LAST(LONGINT)));
  longint.base := longint;

  cardinal := NEW(Subrange, name := NEW(Qid, intf := nullAtm,
                                        item := Atom.FromText("CARDINAL")),
                  base := integer, min := NEW(Value.Ordinal, ord := 0),
                  max := integer.max);
  longcard := NEW(Subrange, name := NEW(Qid, intf := nullAtm,
                                        item := Atom.FromText("LONGCARD")),
                  base := longint, min := NEW(Value.Longint, val := 0L),
                  max := longint.max);

  boolean := NEW(
               UserDefined, name := NEW(Qid, intf := nullAtm,
                                        item := Atom.FromText("BOOLEAN")));
  boolean.elts := NEW(REF ARRAY OF Atom.T, 2);
  boolean.elts[0] := Atom.FromText("FALSE");
  boolean.elts[1] := Atom.FromText("TRUE");

  char := NEW(Char, name := NEW(Qid, intf := nullAtm,
                                item := Atom.FromText("CHAR")));
  widechar := NEW(WideChar, name := NEW(Qid, intf := nullAtm,
                                item := Atom.FromText("WIDECHAR")));
  real := NEW(Real, name := NEW(Qid, intf := nullAtm,
                                item := Atom.FromText("REAL")));
  longreal :=
    NEW(LongReal, name := NEW(Qid, intf := nullAtm,
                              item := Atom.FromText("LONGREAL")));
  extended :=
    NEW(Extended, name := NEW(Qid, intf := nullAtm,
                              item := Atom.FromText("EXTENDED")));
  refany := NEW(Reference, name := NEW(Qid, intf := nullAtm,
                                       item := Atom.FromText("REFANY")),
                traced := TRUE);
  address := NEW(Reference, name := NEW(Qid, intf := nullAtm,
                                        item := Atom.FromText("ADDRESS")),
                 traced := FALSE);
  root := NEW(Object, name := NEW(Qid, intf := nullAtm,
                                  item := Atom.FromText("ROOT")),
              traced := TRUE, fields := NEW(REF ARRAY OF Field, 0),
              methods := NEW(REF ARRAY OF Method, 0));
  untracedRoot :=
    NEW(Object, name := NEW(Qid, intf := nullAtm,
                            item := Atom.FromText("UNTRACED ROOT")),
        traced := FALSE, fields := NEW(REF ARRAY OF Field, 0),
        methods := NEW(REF ARRAY OF Method, 0));
  null := NEW(Reference, name := NEW(Qid, intf := nullAtm,
                                     item := Atom.FromText("NULL")));
  text := NEW(Opaque, name := NEW(Qid, intf := nullAtm,
                                  item := Atom.FromText("TEXT")),
              revealedSuperType := refany);
  mutex := NEW(Opaque, name := NEW(Qid, intf := nullAtm,
                                   item := Atom.FromText("MUTEX")),
               revealedSuperType := root);
END Type.
