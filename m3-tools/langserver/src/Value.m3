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

IMPORT Atom, Fmt, Text, Type;

REVEAL T =  ROOT BRANDED OBJECT END;

PROCEDURE ToText(v: T; type: Type.T): TEXT =
  BEGIN
    TYPECASE v OF
    | Integer (i) => 
        IF (type = Type.integer) OR (type = Type.cardinal) THEN
          RETURN Fmt.Int(i.val); 
        ELSIF type = Type.boolean THEN 
          RETURN Fmt.Bool(VAL(i.val, BOOLEAN));
        ELSE TYPECASE type OF
          | Type.Char =>  RETURN "VAL(" & Fmt.Int(i.val) & ", CHAR)";
          | Type.WideChar =>  RETURN "VAL(" & Fmt.Int(i.val) & ", WIDECHAR)";
          | Type.UserDefined (ud) => 
             RETURN Atom.ToText(ud.elts[i.val]);
          | Type.Subrange (sub) =>
             RETURN ToText(NEW(Integer,
                               val := i.val +  NARROW(sub.min, Integer).val),
                           sub.base);
          ELSE RETURN "Value.ToText: unsupported ordinal type";
          END;
        END;
    | Longint (i) =>
        IF (type = Type.longint) OR (type = Type.longcard) THEN
          RETURN Fmt.LongInt(i.val); 
        ELSE TYPECASE type OF
          | Type.Subrange (sub) =>
             RETURN ToText(NEW(Longint,
                               val := i.val +  NARROW(sub.min, Longint).val),
                           sub.base);
          ELSE RETURN "Value.ToText: unsupported ordinal type";
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
    | Array (arr) =>
        VAR eltList: TEXT;
            eltType: Type.T := NARROW(type, Type.Array).element;
        BEGIN 
          IF NUMBER(arr.elements^) = 0 THEN 
            eltList := "";
          ELSE
            eltList := ToText(arr.elements[0], eltType);
            FOR i := 1 TO LAST(arr.elements^) DO
              eltList := eltList & ", " & 
                 ToText(arr.elements[i], eltType);
            END;
          END;
          RETURN Type.ToText(type) & "{" & eltList & "}";
        END;
    | Set (set) =>
        VAR eltList: TEXT := "";
            baseType: Type.T := NARROW(type, Type.Set).range;
            notFirst := FALSE;
        BEGIN 
          FOR i := 0 TO LAST(set.elements^) DO
            IF set.elements[i] THEN
              IF notFirst THEN eltList := eltList & ", "; END;
              notFirst := TRUE;
              eltList := eltList & 
                 ToText(NEW(Integer, val:=i), baseType);
            END;
          END;
          RETURN Type.ToText(type) & "{" & eltList & "}";
        END;
    | Record (rec) =>
        VAR fieldList: TEXT := "";
            recType := NARROW(type, Type.Record);
            notFirst := FALSE;
        BEGIN
          FOR i := 0 TO LAST(rec.elements^) DO
            IF notFirst THEN fieldList := fieldList & ", "; END;
            notFirst := TRUE;
            fieldList := fieldList & 
               ToText(rec.elements[i], recType.fields[i].type);
          END;
          RETURN Type.ToText(type) & "{" & fieldList & "}";          
        END;
    | Txt (text) => RETURN "\"" & text.val & "\"";
    | Null  => RETURN "NIL";
    ELSE RETURN "Value.ToText: unsupported type";
    END;

  END ToText;

BEGIN
END Value.
