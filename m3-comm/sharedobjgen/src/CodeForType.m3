(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: CodeForType.m3                                             *)
(* Last Modified On Thu Mar  3 11:18:22 PST 1994 by wobber     *)
(*      Modified On Tue May 18 09:07:26 PDT 1993 by mjordan    *)
(*      Modified On Thu Apr 22 11:50:27 PDT 1993 by owicki     *)

MODULE CodeForType;

IMPORT Atom, AtomList, Fmt, Formatter, ImportList(* , AtomRefTbl *),
       SOxCodeUtils, Text, Type, Value, ValueProc, Wr;

<* FATAL Wr.Failure, SOxCodeUtils.Error  *>

(**********************************************************************
 * ToText conversion Internal routines 
 **********************************************************************)
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
      IF notFirst THEN text := text & ";\n"; END;
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

(**********************************************************************
 * ToText conversion External routines 
 **********************************************************************)
PROCEDURE ToText(t: Type.T; byName: BOOLEAN := TRUE; 
                 exports: AtomList.T := NIL): Text.T =
  VAR text: Text.T;
  BEGIN
    IF t = NIL THEN RETURN "" END;
    IF t.name # NIL AND byName THEN RETURN QidToText(t.name, exports); END;
    TYPECASE t OF 
      | Type.Char => RETURN "CHAR"
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
           RAISE SOxCodeUtils.Error("Run time error -- shouldn't occur");
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
              RETURN ToText(o.super) & " " & text & "OBJECT\n" &
                   FieldsToText(o.fields) &
                  "\nMETHODS\n" & MethodsToText(o.methods) & "\nEND";
            | Type.Ref (r) => 
              IF NOT r.traced THEN text := "UNTRACED " & text END;
              RETURN text & "REF " & ToText(r.target, TRUE);
            ELSE RAISE SOxCodeUtils.Error("Run time error -- shouldn't occur");
            END;
          ELSE RAISE SOxCodeUtils.Error("Run time error -- shouldn't occur");
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
      ELSE RAISE SOxCodeUtils.Error("Run time error -- shouldn't occur");
    END;
  END ToText;

PROCEDURE QidToText(qid: Type.Qid; exports: AtomList.T := NIL): TEXT = 
  BEGIN
    IF qid.intf = nullAtm OR 
      (exports # NIL AND AtomList.Member(exports, qid.intf)) THEN 
      RETURN Atom.ToText(qid.item)
    ELSE 
      RETURN Atom.ToText(qid.intf) & "." & Atom.ToText(qid.item) 
    END;
  END QidToText;

PROCEDURE QidToIdentf(qid: Type.Qid): TEXT = 
  BEGIN
    RETURN Atom.ToText(qid.item);
  END QidToIdentf;

(**********************************************************************
 * Printing Internal routines 
 **********************************************************************)
PROCEDURE Formals(fmtr: Formatter.T; 
                  f: REF ARRAY OF Type.Formal;
                  argPragmas: REF ARRAY OF TEXT;
                  suffix := ""; 
                  exports: AtomList.T := NIL) =
  VAR modeName := ARRAY Type.Mode OF TEXT {"", "VAR ", "READONLY "};
  BEGIN
    FOR i := 0 TO LAST(f^) DO
      Formatter.PutText(fmtr, "; ");
      Formatter.UnitedBreak(fmtr);
      IF argPragmas # NIL THEN
        Formatter.PutText(fmtr, argPragmas[i]);
      END;
      Formatter.PutText(fmtr, modeName[f[i].mode] & 
        Atom.ToText(f[i].name) & suffix & ": " & ToText(f[i].type,
                                                        exports := exports));
      (* IF f[i].default # NIL THEN
          text := text & ":= " & ValueProc.ToText(f[i].default, f[i].type); 
        END; *)
    END;
  END Formals;

(**********************************************************************
 * Printing External routines 
 **********************************************************************)
PROCEDURE ProcHeader(f: Formatter.T; 
                     objType: Type.Object; 
                     procName: TEXT; 
                     sig: Type.Signature;
                     argPragmas: REF ARRAY OF TEXT := NIL;
                     suffix := "";
                     exports: AtomList.T := NIL) =
  VAR (* What for???? exceptList:= NEW(AtomRefTbl.Default).init(); *)
      ename: TEXT;
      firstException := TRUE;
      (*
      sharedobj := Atom.FromText("SharedObj");
      event := Atom.FromText("Event");
      error := Atom.FromText("Error");
      fatal := Atom.FromText("Fatal");
      rd := Atom.FromText("Rd");
      failure := Atom.FromText("Failure");
      thread := Atom.FromText("Thread");
      alerted := Atom.FromText("Alerted");
      *)
  BEGIN
    Formatter.Begin(f,4);
    Formatter.PutText(f, "PROCEDURE " & procName & "(");
    Formatter.Begin(f,0);
    Formatter.PutText(f, "self: " & QidToText(objType.name, exports));
    Formals(f, sig.formals, argPragmas, suffix, exports);
    Formatter.PutText(f, ")");
    Formatter.End(f);
    IF sig.result # NIL THEN
      Formatter.Break(f);
      Formatter.PutText(f, ": " & ToText(sig.result, exports := exports));
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
        ename := QidToText(sig.raises[i].qid, exports);
        Formatter.PutText(f, ename);
        (* ??? EVAL exceptList.put(Atom.FromText(ename), NIL); *)
      END;
      Formatter.PutText(f, "}");
    END;
    Formatter.End(f);
  END ProcHeader;

(*
PROCEDURE AddModuleImports(importTbl: AtomRefTbl.T; 
                           methods: MethodList;
                           lastNewMethod: INTEGER) =
  BEGIN
    FOR i := 0 TO lastNewMethod DO
      WITH sig = methods[i].sig DO
        ImportRefsFromSig(sig, importTbl);
        FOR j := 0 TO LAST(sig.raises^) DO
          WITH r = sig.raises[j] DO
            IF r.arg # NIL AND
'              NOT (r.qid.intf = thread AND r.qid.item = alerted) AND
              NOT (r.qid.intf = netobj AND r.qid.item = error) THEN
              ImportFromType(r.arg, importTbl);
            END;
          END; 
        END;
      END;
    END;
  END AddModuleImports;
*)

PROCEDURE PrintSig (f         : Formatter.T;
                    sig       : Type.Signature; 
                    argPragmas: REF ARRAY OF TEXT :=NIL;
                    suffix := ""; 
                    exports: AtomList.T := NIL) =
  BEGIN
    Formals(f, sig.formals, argPragmas, suffix, exports);
  END PrintSig;

PROCEDURE PrintArgs (f         : Formatter.T;
                     sig       : Type.Signature) = 
  BEGIN
    FOR i := 0 TO LAST(sig.formals^) DO
      Formatter.PutText(f, ", ");
      Formatter.Break(f);
      Formatter.PutText(f, Atom.ToText(sig.formals[i].name));
    END;
  END PrintArgs;

(**********************************************************************
 * ImportList creation Internal routines 
 **********************************************************************)
PROCEDURE ImportFromType(t: Type.T; importTbl: ImportList.T; 
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
          ImportReducedSig(ob.methods[i].sig, importTbl);
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
    | Type.Procedure (p) => ImportReducedSig(p.sig, importTbl);
    ELSE RAISE SOxCodeUtils.Error("Run time error -- shouldn't occur");
    END;
  END ImportFromType;

PROCEDURE ImportRefsFromType(t: Type.T; importTbl: ImportList.T) =
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
    ELSE RAISE SOxCodeUtils.Error("Run time error -- shouldn't occur");
    END;
  END ImportRefsFromType;

PROCEDURE ImportFromFields(fields: REF ARRAY OF Type.Field; 
                           importTbl: ImportList.T) =
  BEGIN
    IF fields = NIL THEN RETURN END;
    FOR i := 0 TO LAST(fields^) DO
      ImportFromType(fields[i].type, importTbl);
    END;
  END ImportFromFields;

PROCEDURE ImportReducedSig(sig: Type.Signature; importTbl: ImportList.T) =
  BEGIN
    FOR i := 0 TO LAST(sig.formals^) DO
      ImportFromType(sig.formals[i].type, importTbl);
    END;
    ImportFromType(sig.result, importTbl);
  END ImportReducedSig; 

PROCEDURE ImportFullSig(sig: Type.Signature; importTbl: ImportList.T) =
  BEGIN
    FOR i := 0 TO LAST(sig.formals^) DO
      ImportFromType(sig.formals[i].type, importTbl);
      ImportRefsFromType(sig.formals[i].type, importTbl);
    END;
    ImportFromType(sig.result, importTbl);
    ImportRefsFromType(sig.result, importTbl);

    IF sig.raises # NIL THEN
      FOR i := 0 TO LAST(sig.raises^) DO
        WITH r = sig.raises[i] DO
          EVAL importTbl.put(r.qid.intf, NIL);
          IF r.arg # NIL THEN
            ImportFromType(r.arg, importTbl);
          END;
        END; 
      END;
    END;
  END ImportFullSig;

PROCEDURE ImportRevelations(t: Type.Reference; importTbl: ImportList.T) =
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
            ELSE RAISE SOxCodeUtils.Error("Run time error -- shouldn't occur");
            END;
          END;
        END;
      ELSE 
    END;
  END ImportRevelations;

(**********************************************************************
 * ImportList creation External routines 
 **********************************************************************)
PROCEDURE ImportLst(t: Type.Object; 
                    importTbl: ImportList.T; 
                    <*UNUSED*>methods: MethodList;
                    <*UNUSED*>umethods: AtomList.T) =
  BEGIN
    IF t.name # NIL AND t.name.intf # nullAtm THEN
      EVAL importTbl.put(t.name.intf, NIL);
    END;
  END ImportLst;

PROCEDURE ImportCBLst(<*UNUSED*>t: Type.Object; 
                       importTbl: ImportList.T; 
                       methods: MethodList;
                       umethods: AtomList.T) =
  BEGIN
    (* Include the basic method data *)
    FOR i := 0 TO LAST(methods^) DO
      WITH meth = methods[i].name DO
        IF AtomList.Member(umethods, meth) THEN 
          ImportReducedSig(methods[i].sig, importTbl);
        END;
      END;
    END;
  END ImportCBLst;  

PROCEDURE ImportSOLst(t: Type.Object;
                       importTbl: ImportList.T; 
                       methods: MethodList;
                       <* UNUSED *> umethods: AtomList.T) =
  VAR obj: Type.Reference := t;
  BEGIN
    (* add the raises information *)
    FOR i := 0 TO LAST(methods^) DO
(*      WITH meth = methods[i].name,
           sig = methods[i].sig DO
          IF AtomList.Member(umethods, meth) THEN *)
        WITH sig = methods[i].sig DO
          ImportFullSig(sig, importTbl);
(*        END;*)
      END;
    END;

    ImportRevelations(t, importTbl);

    (* Include the full object type revelation interfaces *)
    WHILE obj # NIL AND ISTYPE(obj, Type.Object)  DO
      IF obj.revIntf # NIL THEN
        EVAL importTbl.put(obj.revIntf, NIL);
      END;
      obj := NARROW(obj, Type.Object).super;
    END;
  END ImportSOLst; 

PROCEDURE AugmentImportList(importList: ImportList.T; 
     READONLY newImports: ARRAY OF Atom.T) =
  VAR dummy: REFANY;
  BEGIN
    FOR i := FIRST(newImports) TO LAST(newImports) DO
      EVAL importList.put(newImports[i], dummy)
    END;
  END AugmentImportList;

PROCEDURE AugmentExceptionList(VAR (*in/out*) exceptionList: AtomList.T; 
                               raises  : REF ARRAY OF Type.Exception;
                               exports: AtomList.T := NIL) =
  BEGIN
    FOR i := FIRST(raises^) TO LAST(raises^) DO
      WITH elem = Atom.FromText(QidToText(raises[i].qid, exports)) DO
        IF NOT AtomList.Member(exceptionList, elem) THEN
          exceptionList := AtomList.Cons(elem, exceptionList);
        END;
      END;
    END;
  END AugmentExceptionList;

(**********************************************************************
 * Write out an import list
 **********************************************************************)
PROCEDURE OutputIntf(intf: Atom.T; fmtWr: Formatter.T;
                     firstTime := TRUE)=
  BEGIN
    IF NOT firstTime THEN
      Formatter.PutText(fmtWr, ", ");
      Formatter.Break(fmtWr, type:=Formatter.BreakType.NonOptimal);
    END;
    Formatter.PutText(fmtWr, Atom.ToText(intf));
  END OutputIntf;

PROCEDURE ProduceImports(fmtWr: Formatter.T; 
                         imports: ImportList.T) =
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

VAR nullAtm := Atom.FromText("");

BEGIN
END CodeForType.
