(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: CodeForType.m3                                             *)
(* Last Modified On Thu Mar  3 11:18:22 PST 1994 by wobber     *)
(*      Modified On Tue May 18 09:07:26 PDT 1993 by mjordan    *)
(*      Modified On Thu Apr 22 11:50:27 PDT 1993 by owicki     *)

MODULE CodeForType;

IMPORT Atom, Fmt, Formatter, AtomRefTbl, StubCode, StubUtils,
       Text, Type, Value, ValueProc, Wr;

<* FATAL Wr.Failure  *>

PROCEDURE ToText(t: Type.T; byName: BOOLEAN := TRUE): Text.T =
  VAR text: Text.T;
  BEGIN
    IF t = NIL THEN RETURN "" END;
    IF t.name # NIL AND byName THEN RETURN QidToText(t.name); END;
    TYPECASE t OF 
      | Type.Char => RETURN "CHAR"
      | Type.WideChar => RETURN "WIDECHAR"
      | Type.UserDefined (ud) => 
          IF NUMBER(ud.elts^) = 0 THEN text := "";
          ELSE 
            text := Atom.ToText(ud.elts[0]);
            FOR i := 1 TO LAST(ud.elts^) DO
              text := text & ", " & Atom.ToText(ud.elts[i]);
            END;
          END;
          RETURN "{" & text & "}";
      | Type.Enumeration (enum) =>
          IF enum = Type.boolean THEN RETURN "BOOLEAN"; END;
          StubUtils.Die("CodeForType.ToText: unsupported enumeration type");
      | Type.Subrange (sub) => 
          VAR min, max: INTEGER;
              ud: Type.UserDefined;
          BEGIN
            IF sub = Type.integer THEN RETURN "INTEGER" END;
            min := NARROW(sub.min, Value.Ordinal).ord;
            max := NARROW(sub.max, Value.Ordinal).ord;
            IF sub.base = Type.integer OR sub.base = Type.cardinal THEN RETURN
               "[" & Fmt.Int(min) &  ".." & Fmt.Int(max) &"]"
            END;
            IF sub.base = Type.char THEN RETURN
               "[VAL(" & Fmt.Int(min) &  ", CHAR) .. VAL(" & 
                     Fmt.Int(max) & ", CHAR)]"
            END;
            ud := NARROW(sub.base, Type.UserDefined);
            RETURN "[" & ToText(sub.base) & "." & Atom.ToText(ud.elts[min]) & 
                    ".." & ToText(sub.base) & "." & Atom.ToText(ud.elts[max]) & 
                     "]";
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
            (* RETURN Atom.ToText(t.name.intf) & "." & Atom.ToText(t.name.item)*)
          | Type.Object, Type.Ref =>
            IF ref.brand # NIL THEN 
              text := "BRANDED \"" &  Atom.ToText(ref.brand) & "\" ";
            ELSE 
              text := ""
            END;
            TYPECASE ref OF
            | Type.Object(o) => 
              RETURN ToText(o.super) & " " & text & "OBJECT" & Wr.EOL &
                     FieldsToText(o.fields) & Wr.EOL
                  & "METHODS" & Wr.EOL
                  & MethodsToText(o.methods) & Wr.EOL
                  & "END";
            | Type.Ref (r) => 
              IF NOT r.traced THEN text := "UNTRACED " & text END;
              RETURN text & "REF " & ToText(r.target, TRUE);
            ELSE StubUtils.Die("CodeForType.ToText: unsupported reference type");
            END;
          ELSE StubUtils.Die("CodeForType.ToText: unsupported reference type");
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
      | Type.Set (set) =>
          RETURN "SET OF " & ToText(set.range);
      | Type.Procedure  =>
          RETURN "PROCEDURE" (* & SigToText(proc.sig);*)
      ELSE StubUtils.Die("CodeForType.ToText: unsupported type");
    END;

    RETURN NIL;
  END ToText;

PROCEDURE QidToText(qid: Type.Qid): TEXT = 
  BEGIN
    IF qid.intf = nullAtm THEN RETURN Atom.ToText(qid.item)
    ELSE RETURN Atom.ToText(qid.intf) & "." & Atom.ToText(qid.item) 
    END;
  END QidToText;


PROCEDURE ProcHeader(f: Formatter.T; 
                       objType: Type.Object; 
                       procName: TEXT; 
                       sig: Type.Signature;
                       argPragmas: REF ARRAY OF TEXT := NIL;
                       suffix := "") =
  VAR exceptList:= NEW(AtomRefTbl.Default).init();
      ename: TEXT;
      firstException := TRUE;
  BEGIN
    Formatter.Begin(f,4);
    Formatter.Begin(f,4);
    Formatter.PutText(f, "PROCEDURE " & procName & "(");
    Formatter.UnitedBreak(f);
    Formatter.PutText(f, "self: " & QidToText(objType.name));
    Formals(f, sig.formals, argPragmas, suffix);
    Formatter.PutText(f, ")");
    Formatter.End(f);
    IF sig.result # NIL THEN
      Formatter.Break(f);
      Formatter.PutText(f, ": " & ToText(sig.result));
    END;
    IF sig.raises = NIL THEN 
      Formatter.Break(f);
      Formatter.PutText(f, " RAISES ANY");
    ELSE
      Formatter.Break(f);
      Formatter.PutText(f, " RAISES {");
      FOR i := 0 TO LAST(sig.raises^) DO
        IF NOT firstException THEN 
          Formatter.PutText(f,  ", ");
          Formatter.Break(f);
        END;
        firstException := FALSE;
        ename := QidToText(sig.raises[i].qid);
        Formatter.PutText(f, ename);
        EVAL exceptList.put(Atom.FromText(ename), NIL);
      END;
      Formatter.PutText(f, "}");
    END;
    Formatter.End(f);
  END ProcHeader;

PROCEDURE FieldsToText(f: REF ARRAY OF Type.Field): TEXT =
  VAR notFirst := FALSE;
      text := "";
  BEGIN
    FOR i := 0 TO LAST(f^) DO
      IF notFirst THEN text := text & "; "; END;
      notFirst := TRUE;
      text := text & Atom.ToText(f[i].name) & ": " & 
                ToText(f[i].type);
      IF f[i].default # NIL THEN
        text := text & ":= " & ValueProc.ToText(f[i].default, f[i].type);
      END;
    END;
    RETURN text;
  END FieldsToText;

PROCEDURE MethodsToText(m: REF ARRAY OF Type.Method): TEXT =
  VAR notFirst := FALSE;
      text := "";
  BEGIN
    FOR i := 0 TO LAST(m^) DO
      IF notFirst THEN text := text & ";" & Wr.EOL; END;
      notFirst := TRUE;
      text := text & Atom.ToText(m[i].name) (*& SigToText(m[i].sig);*);
      IF m[i].default # NIL THEN
        text := text & ":= " ;
(*        TYPECASE m[i].default OF
          MethodDefault1 (md1) => text := text & QidToText(md1.qid);
        | MethodDefault2 (md2) => text := text & ToText(md2.obType) &
                                 "." & Atom.ToText(md2.method);
        END;  *)
      END;
    END;
    RETURN text;
  END MethodsToText;

PROCEDURE Formals(fmtr: Formatter.T; 
                  f: REF ARRAY OF Type.Formal;
                  argPragmas: REF ARRAY OF TEXT;
                  suffix := "") =
  VAR notFirst := FALSE;
      modeName := ARRAY Type.Mode OF TEXT {"", "VAR ", "READONLY "};
  BEGIN
    FOR i := 0 TO LAST(f^) DO
      Formatter.PutText(fmtr, "; ");
      Formatter.UnitedBreak(fmtr);
      notFirst := TRUE;
      IF argPragmas # NIL THEN
        Formatter.PutText(fmtr, argPragmas[i]);
      END;
      Formatter.PutText(fmtr, modeName[f[i].mode] & 
        Atom.ToText(f[i].name) & suffix & ": " & ToText(f[i].type));
      (* IF f[i].default # NIL THEN
          text := text & ":= " & ValueProc.ToText(f[i].default, f[i].type); 
        END; *)
    END;
  END Formals;

PROCEDURE ImportRevelations(t: Type.Reference; importTbl: AtomRefTbl.T) =
  BEGIN
    IF t.revIntf # NIL THEN
      EVAL importTbl.put(t.revIntf, NIL);
    END;
    TYPECASE t OF
      | Type.Object (obj)  =>
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
            ELSE StubUtils.Die("CodeForType.ImportRevelations: unsupported object supertype");
            END;
          END;
        END;
      ELSE 
    END;
  END ImportRevelations;

PROCEDURE ImportList(t: Type.Object; 
                     importTbl: AtomRefTbl.T; 
                     methods: MethodList;
                     lastNewMethod: INTEGER;
                     byName: BOOLEAN:= TRUE) =
  VAR obj: Type.Reference := t;
  BEGIN
    IF t.name # NIL AND t.name.intf # nullAtm THEN
      EVAL importTbl.put(t.name.intf, NIL);
    END;
    ImportRevelations(t, importTbl);
    IF byName AND t.name # NIL THEN
      RETURN;
    END;
    FOR i := 0 TO lastNewMethod DO
      ImportFromSig(methods[i].sig, importTbl);
    END;
    WHILE obj # NIL AND ISTYPE(obj, Type.Object)  DO
      IF obj.revIntf # NIL THEN
        EVAL importTbl.put(obj.revIntf, NIL);
      END;
      obj := NARROW(obj, Type.Object).super;
    END;
  END ImportList;

PROCEDURE ImportFromType(t: Type.T; importTbl: AtomRefTbl.T; 
                     byName: BOOLEAN:= TRUE) =
  BEGIN
    IF t = NIL THEN RETURN END;
    IF t.name # NIL AND t.name.intf # nullAtm THEN
      EVAL importTbl.put(t.name.intf, NIL);
    END;
    TYPECASE t OF
    | Type.Reference (ref) => ImportRevelations(ref, importTbl);
    ELSE
    END; 
    IF byName AND t.name # NIL THEN
      RETURN;
    END;
    TYPECASE t OF
    | Type.Enumeration, Type.UserDefined, Type.Real, Type.LongReal, 
        Type.Extended =>
    | Type.Subrange (sr) => ImportFromType(sr.base, importTbl);
    | Type.Object (ob) => 
        ImportFromFields(ob.fields, importTbl);
        FOR i := 0 TO LAST(ob.methods^) DO
          ImportFromSig(ob.methods[i].sig, importTbl);
        END;
        IF ob.revIntf # NIL THEN
          EVAL importTbl.put(ob.revIntf, NIL);
        END;
    | Type.Ref (r) => ImportFromType(r.target, importTbl);
    | Type.Opaque (op) => 
        ImportFromType(op.revealedSuperType, importTbl);
    | Type.Array (a) => ImportFromType(a.index, importTbl);
                   ImportFromType(a.element, importTbl);
    | Type.Packed (p) => ImportFromType(p.base, importTbl);
    | Type.Record (rec) => ImportFromFields(rec.fields, importTbl);        
    | Type.Set (s) => ImportFromType(s.range, importTbl);
    | Type.Procedure (p) => ImportFromSig(p.sig, importTbl);
    ELSE StubUtils.Die("CodeForType.ImportFromType: unsupported type");
    END;
  END ImportFromType;

PROCEDURE ImportFromFields(fields: REF ARRAY OF Type.Field; 
                           importTbl: AtomRefTbl.T) =
  BEGIN
    IF fields = NIL THEN RETURN END;
    FOR i := 0 TO LAST(fields^) DO
      ImportFromType(fields[i].type, importTbl);
    END;
  END ImportFromFields;

PROCEDURE ImportFromSig(sig: Type.Signature; importTbl: AtomRefTbl.T) =
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

PROCEDURE ImportRefsFromSig(sig: Type.Signature; importTbl: AtomRefTbl.T) =
  BEGIN
    FOR i := 0 TO LAST(sig.formals^) DO
      ImportRefsFromType(sig.formals[i].type, importTbl);
    END;
    ImportRefsFromType(sig.result, importTbl);
  END ImportRefsFromSig;

PROCEDURE ImportRefsFromType(t: Type.T; importTbl: AtomRefTbl.T) =
  BEGIN
    IF t = NIL THEN RETURN END;
    TYPECASE t OF
    | Type.Enumeration, Type.UserDefined, Type.Real, Type.LongReal, 
        Type.Extended, Type.Subrange, Type.Set, Type.Procedure =>
    | Type.Reference => 
        IF NOT Type.MayBeRefAny(t) AND Type.NamedType(t) THEN
          ImportFromType(t, importTbl)
        END;
    | Type.Array (a) => ImportFromType(a.index, importTbl);
                   ImportRefsFromType(a.element, importTbl);
    | Type.Packed (p) => ImportRefsFromType(p.base, importTbl);
    | Type.Record (rec) => 
        IF rec.fields = NIL THEN RETURN END;
        FOR i := 0 TO LAST(rec.fields^) DO
          ImportRefsFromType(rec.fields[i].type, importTbl);
        END;
    ELSE StubUtils.Die("CodeForType.ImportRefsFromType: unsupported type");
    END;
  END ImportRefsFromType;

PROCEDURE AugmentImportList(importList: AtomRefTbl.T; 
     READONLY newImports: ARRAY OF Atom.T) =
  VAR dummy: REFANY;
  BEGIN
    FOR i := FIRST(newImports) TO LAST(newImports) DO
      EVAL importList.put(newImports[i], dummy)
    END;
  END AugmentImportList;

PROCEDURE AddModuleImports(importTbl: AtomRefTbl.T; 
                           methods: MethodList;
                           lastNewMethod: INTEGER) =
  VAR thread := Atom.FromText("Thread");
      alerted := Atom.FromText("Alerted");
      netobj := Atom.FromText("NetObj");
      error := Atom.FromText("Error");
  BEGIN
    FOR i := 0 TO lastNewMethod DO
      WITH sig = methods[i].sig DO
        ImportRefsFromSig(sig, importTbl);
        FOR j := 0 TO LAST(sig.raises^) DO
          WITH r = sig.raises[j] DO
            IF r.arg # NIL AND
              NOT (r.qid.intf = thread AND r.qid.item = alerted) AND
              NOT (r.qid.intf = netobj AND r.qid.item = error) THEN
              ImportFromType(r.arg, importTbl);
            END;
          END; 
        END;
      END;
    END;
  END AddModuleImports;

PROCEDURE ProduceImports(fmtWr: Formatter.T; 
                         <* UNUSED *>objName: Type.Qid; 
                         imports: AtomRefTbl.T) =
  VAR key: Atom.T; value: REFANY;
    iter := imports.iterate();
    firstTime := TRUE;
  BEGIN
    Formatter.Begin(fmtWr, 7);
    Formatter.PutText(fmtWr, "IMPORT ");
    WHILE iter.next(key, value) DO
      OutputIntf(key, fmtWr, firstTime); 
      firstTime := FALSE;
    END;
    Formatter.PutText(fmtWr, ";"); 
    Formatter.End(fmtWr);
    Formatter.NewLine(fmtWr, freshLine := FALSE);
  END ProduceImports;

PROCEDURE OutputIntf(intf: Atom.T; fmtWr: Formatter.T;
                     firstTime := TRUE)=
  BEGIN
    IF NOT firstTime THEN
      Formatter.PutText(fmtWr, ", ");
      Formatter.Break(fmtWr, type:=Formatter.BreakType.NonOptimal);
    END;
    Formatter.PutText(fmtWr, Atom.ToText(intf));
  END OutputIntf;

PROCEDURE ImportSuperStubs(fmtWr: Formatter.T; 
                           methods: StubCode.MethodList;
                           lastNewMethod: INTEGER;
                           <*UNUSED*>typeName: Atom.T) =
  (* VAR last := typeName;
      printedAny := FALSE; *)
  BEGIN
    IF lastNewMethod < LAST(methods^) THEN
      (* Some stubs use methods from a supertype. 
         That supertype must be imported.  *)
      Formatter.PutText(fmtWr, "IMPORT " & 
        Atom.ToText(methods[lastNewMethod+1].intf) &";");  
      Formatter.NewLine(fmtWr, freshLine := FALSE);
    END;
    (*  
    FOR i := LAST(methods^) TO 0 BY -1 DO
      IF last # methods[i].intf THEN
        last := methods[i].intf;
        IF NOT printedAny THEN
          Formatter.PutText(fmtWr, "IMPORT " & Atom.ToText(last));  
          printedAny := TRUE;
        ELSE
          Formatter.PutText(fmtWr, ", " & Atom.ToText(last));  
        END;
      END;
    END;
    IF printedAny THEN 
      Formatter.PutText(fmtWr, ";");
    END;
    Formatter.NewLine(fmtWr, freshLine := FALSE);
    *)
  END ImportSuperStubs;

VAR nullAtm := Atom.FromText("");

BEGIN

END CodeForType.
