(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Value.m3                                             *)
(* Last Modified On Mon Feb 28 16:51:07 PST 1994 by wobber     *)
(*      Modified On Mon May 17 13:39:12 PDT 1993 by mjordan    *)
(*      Modified On Tue Nov 10 13:48:19 PST 1992 by owicki     *)
(*      Modified On Fri Feb  2 10:35:09 PST 1990 by gnelson    *)
(*      Modified On Wed Dec 20 18:07:50 1989 by kalsow         *)

MODULE Value EXPORTS Value, ValueProc;

IMPORT Atom, Fmt, Text, Type, StubUtils;

REVEAL T =  ROOT BRANDED OBJECT END;

PROCEDURE ToText(v: T; type: Type.T): TEXT =
  BEGIN
    TYPECASE v OF
    | Ordinal (ordinal) => 
        IF (type = Type.integer) OR (type = Type.cardinal) THEN
          RETURN Fmt.Int(ordinal.ord); 
        ELSIF type = Type.boolean THEN 
          RETURN Fmt.Bool(VAL(ordinal.ord, BOOLEAN));
        ELSE TYPECASE type OF
            Type.Char =>  RETURN "VAL(" & Fmt.Int(ordinal.ord) & ", CHAR)";
          | Type.UserDefined (ud) => 
             RETURN Atom.ToText(ud.elts[ordinal.ord]);
          | Type.Subrange (sub) =>
             RETURN ToText(NEW(Ordinal, 
                    ord := ordinal.ord +  NARROW(sub.min, Ordinal).ord),
                           sub.base);
          ELSE StubUtils.Die("Value.ToText: unsupported ordinal type");
          END;
        END;
    | Float (f) =>
        RETURN Fmt.Real(f.val, style := Fmt.Style.Auto, literal := TRUE);
    | LongFloat (lf) =>
        RETURN Fmt.LongReal(lf.val, style := Fmt.Style.Auto, literal := TRUE);
    | Extended (e) =>
        VAR lr:= FLOAT(e.val, LONGREAL);
            txt := Fmt.LongReal(lr, style := Fmt.Style.Auto, literal := TRUE);
            pos := Text.FindChar(txt, 'D');
        BEGIN
          RETURN Text.Sub(txt, 0, pos) & "X" & 
                 Text.Sub(txt, pos+1, Text.Length(txt));
        END;
    | ArrayOrRecord (aor) =>
      (* arrays and records use same format for initialization *)
      TYPECASE type OF
        Type.Array(arrType) =>
        VAR eltList: TEXT;
            eltType:= arrType.element;
        BEGIN 
          IF NUMBER(aor.elements^) = 0 THEN 
            eltList := "";
          ELSE
            eltList := ArrayOrRecordElemToText(aor.elements[0], eltType);
            FOR i := 1 TO LAST(aor.elements^) DO
              eltList := eltList & ", " & 
                             ArrayOrRecordElemToText(aor.elements[i], eltType);
            END;
          END;
          RETURN Type.ToText(type) & "{" & eltList & "}";
        END;
      |
        Type.Record(recType) =>
        VAR fieldList: TEXT := "";
            notFirst := FALSE;
        BEGIN
          FOR i := 0 TO LAST(aor.elements^) DO
            IF notFirst THEN fieldList := fieldList & ", "; END;
            notFirst := TRUE;
            fieldList := fieldList & 
               ArrayOrRecordElemToText(aor.elements[i], recType.fields[i].type);
          END;
          RETURN Type.ToText(type) & "{" & fieldList & "}";          
        END;
      ELSE
        StubUtils.Die("Value.ToText: Array or record constructor but type isn't Type.Array or Type.Record")
      END
    | Set (set) =>
        VAR eltList: TEXT := "";
            baseType: Type.T := NARROW(type, Type.Set).range;
            notFirst := FALSE;
        BEGIN 
          FOR i := 0 TO LAST(set.elements^) DO
            IF notFirst THEN eltList := eltList & ", "; END;
            notFirst := TRUE;
            eltList := eltList & 
                           ToText(NEW(Ordinal, 
                                      ord:=set.elements[i].ord), baseType);
          END;
          RETURN Type.ToText(type) & "{" & eltList & "}";
        END;
    | Txt (text) => RETURN "\"" & text.val & "\"";
    | Null  => RETURN "NIL";
    ELSE StubUtils.Die("Value.ToText: unsupported type");
    END;

    RETURN NIL;
  END ToText;

PROCEDURE ArrayOrRecordElemToText(elem : Element; type : Type.T) : TEXT =
  BEGIN
    TYPECASE elem OF
      Propagate => RETURN ".." (* propagate array initializer *)
    |
      Range(ran) => RETURN ToText(ran.val, type)
    |
      Actual(act) => RETURN Atom.ToText(act.field) & " := " & ToText(act.val, type)
    ELSE StubUtils.Die("Value.ArrayOrRecordElemToText: unsupported type");
      <*ASSERT FALSE *>
    END
  END ArrayOrRecordElemToText;

BEGIN
END Value.
