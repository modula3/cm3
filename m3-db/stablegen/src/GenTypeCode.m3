(* Copyright (C) 1989, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(* Created by Susan Owicki                                   *)
(* Last modified on Wed Sep 28 12:03:12 PDT 1994 by weich    *)

(* The module provides code producing procedures for the stub
   generator. *)

MODULE GenTypeCode;

IMPORT Atom, Fmt, Wr, Formatter, Text, Type,
       Value, ValueProc;

<*FATAL Wr.Failure*>

(* \subsection{Produce type specification text}
   The procedures "ToText()", "FieldsToText()" and "MethodsToText()"
   recursivly scan "Type.T" structures to produce equivalent
   "TEXT" that contains the type specification in Modula-3 code.
*)
PROCEDURE ToText (t: Type.T; byName: BOOLEAN := TRUE): Text.T =
  VAR text: Text.T;
  BEGIN
    IF t = NIL THEN RETURN "" END;
    IF t.name # NIL AND byName THEN RETURN QidToText(t.name); END;
    TYPECASE t OF
    | Type.Char => RETURN "CHAR"
    | Type.UserDefined (ud) =>
        IF NUMBER(ud.elts^) = 0 THEN
          text := "";
        ELSE
          text := Atom.ToText(ud.elts[0]);
          FOR i := 1 TO LAST(ud.elts^) DO
            text := text & ", " & Atom.ToText(ud.elts[i]);
          END;
        END;
        RETURN "{" & text & "}";
    | Type.Enumeration (enum) =>
        IF enum = Type.boolean THEN RETURN "BOOLEAN"; END;
        <*ASSERT FALSE*>
    | Type.Subrange (sub) =>
        VAR
          min, max: INTEGER;
          ud      : Type.UserDefined;
        BEGIN
          IF sub = Type.integer THEN RETURN "INTEGER" END;
          min := NARROW(sub.min, Value.Ordinal).ord;
          max := NARROW(sub.max, Value.Ordinal).ord;
          IF sub.base = Type.integer OR sub.base = Type.cardinal THEN
            RETURN "[" & Fmt.Int(min) & ".." & Fmt.Int(max) & "]"
          END;
          IF sub.base = Type.char THEN
            RETURN "[VAL(" & Fmt.Int(min) & ", CHAR) .. VAL("
                     & Fmt.Int(max) & ", CHAR)]"
          END;
          ud := NARROW(sub.base, Type.UserDefined);
          RETURN
            "[" & ToText(sub.base) & "." & Atom.ToText(ud.elts[min]) & ".."
              & ToText(sub.base) & "." & Atom.ToText(ud.elts[max]) & "]";
        END;
    | Type.Real => RETURN "REAL";
    | Type.LongReal => RETURN "LONGREAL";
    | Type.Extended => RETURN "EXTENDED";
    | Type.Reference (ref) =>
        TYPECASE ref OF
        | Type.Opaque (o) =>
            (* Type can only be displayed by name *)
            RETURN "***Error*** Opaque type only printed by name: "
                     & "supertype " & ToText(o.revealedSuperType);
          (* RETURN Atom.ToText(t.name.intf) & "." &
             Atom.ToText(t.name.item)*)
        | Type.Object, Type.Ref =>
            IF ref.brand # NIL THEN
              text := "BRANDED \"" & Atom.ToText(ref.brand) & "\" ";
            ELSE
              text := ""
            END;
            TYPECASE ref OF
            | Type.Object (o) =>
                RETURN ToText(o.super) & " " & text & "OBJECT\n"
                         & FieldsToText(o.fields) & "\nMETHODS\n"
                         & MethodsToText(o.methods) & "\nEND";
            | Type.Ref (r) =>
                IF NOT r.traced THEN text := "UNTRACED " & text END;
                RETURN text & "REF " & ToText(r.target, TRUE);
            ELSE
              <*ASSERT FALSE*>
            END;
        ELSE
          <*ASSERT FALSE*>
        END;
    | Type.Array (arr) =>
        IF arr.index = NIL THEN
          text := "";
        ELSE
          text := ToText(arr.index);
        END;
        RETURN "ARRAY " & text & " OF " & ToText(arr.element);
    | Type.Packed (p) =>
        RETURN "BITS " & Fmt.Int(p.size) & "FORF " & ToText(p.base);
    | Type.Record (rec) =>
        RETURN "RECORD " & FieldsToText(rec.fields) & " END";
    | Type.Set (set) => RETURN "SET OF " & ToText(set.range);
    | Type.Procedure => RETURN "PROCEDURE" (* & SigToText(proc.sig);*)
    ELSE
      <*ASSERT FALSE*>
    END;
  END ToText;

PROCEDURE FieldsToText (f: REF ARRAY OF Type.Field): TEXT =
  VAR
    notFirst := FALSE;
    text     := "";
  BEGIN
    FOR i := 0 TO LAST(f^) DO
      IF notFirst THEN text := text & "; "; END;
      notFirst := TRUE;
      text := text & Atom.ToText(f[i].name) & ": " & ToText(f[i].type);
      IF f[i].default # NIL THEN
        text := text & ":= " & ValueProc.ToText(f[i].default, f[i].type);
      END;
    END;
    RETURN text;
  END FieldsToText;

PROCEDURE MethodsToText (m: REF ARRAY OF Type.Method): TEXT =
  VAR
    notFirst := FALSE;
    text     := "";
  BEGIN
    FOR i := 0 TO LAST(m^) DO
      IF notFirst THEN text := text & ";\n"; END;
      notFirst := TRUE;
      text := text & Atom.ToText(m[i].name) (*& SigToText(m[i].sig);*);
      IF m[i].default # NIL THEN
        text := text & ":= ";
        (* TYPECASE m[i].default OF MethodDefault1 (md1) => text := text &
           QidToText(md1.qid); | MethodDefault2 (md2) => text := text &
           ToText(md2.obType) & "." & Atom.ToText(md2.method); END; *)
      END;
    END;
    RETURN text;
  END MethodsToText;


(* \subsection{Procedure ProcHeader}
   Produce a procedure header suitable for method procedures. The
   first parameter is always set to "self: T". The 

   Put output to "f", name procedure "procName", use "sig" for
   parameter, result type and raises declarations. Use "argPragmas"
   for pragmas connected to arguments of the procedure.
   "Formals()" is called to produce deklarations for each formal
   parameter in "sig".
*)
PROCEDURE ProcHeader (f         : Formatter.T;
                      procName  : TEXT;
                      sig       : Type.Signature;
                      suffix                        := "";
                      argPragmas: REF ARRAY OF TEXT := NIL ) =
  BEGIN
    Formatter.Begin(f, 4);
    Formatter.Begin(f, 4);
    Formatter.PutText(f, "PROCEDURE " & procName & "(");
    Formatter.UnitedBreak(f);
    Formatter.PutText(f, "self: T");
    Formals(f, sig.formals, argPragmas, suffix);
    Formatter.PutText(f, ")");
    Formatter.End(f);
    IF sig.result # NIL THEN
      Formatter.Break(f);
      Formatter.PutText(f, ": " & ToText(sig.result))
    END;
    IF sig.raises = NIL THEN
      Formatter.Break(f);
      Formatter.PutText(f, " RAISES ANY")
    ELSE
      Formatter.Break(f);
      Formatter.PutText(f, " RAISES {");
      IF NUMBER(sig.raises^) > 0 THEN
        Formatter.PutText(f, QidToText(sig.raises[0].qid));
        FOR i := 1 TO LAST(sig.raises^) DO
          Formatter.PutText(f, ", ");
          Formatter.Break(f);
          Formatter.PutText(f, QidToText(sig.raises[i].qid))
        END (*FOR*)
      END; (*IF*)
      Formatter.PutText(f, "}")
    END; (*IF*)
    Formatter.End(f);
  END ProcHeader;

PROCEDURE Formals (fmtr      : Formatter.T;
                   f         : REF ARRAY OF Type.Formal;
                   argPragmas: REF ARRAY OF TEXT;
                   suffix                                 := "") =
  VAR
    notFirst := FALSE;
    modeName := ARRAY Type.Mode OF TEXT{"", "VAR ", "READONLY "};
  BEGIN
    FOR i := 0 TO LAST(f^) DO
      Formatter.PutText(fmtr, "; ");
      Formatter.UnitedBreak(fmtr);
      notFirst := TRUE;
      IF argPragmas # NIL THEN Formatter.PutText(fmtr, argPragmas[i]); END;
      Formatter.PutText(fmtr, modeName[f[i].mode] & Atom.ToText(f[i].name)
                                & suffix & ": " & ToText(f[i].type));
      (* IF f[i].default # NIL THEN text := text & ":= " &
         ValueProc.ToText(f[i].default, f[i].type); END; *)
    END;
  END Formals;


(* \subsection{Procedure QidToText}
   Utility procedure used to convert a "Type.Qid" to a "TEXT".
*)
PROCEDURE QidToText (qid: Type.Qid): TEXT =
  BEGIN
    IF qid.intf = Atom.FromText("") THEN
      RETURN Atom.ToText(qid.item)
    ELSE
      RETURN Atom.ToText(qid.intf) & "." & Atom.ToText(qid.item)
    END;
  END QidToText;

BEGIN
END GenTypeCode.
