(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue May 17 14:59:39 PDT 1994 by mhb        *)
(*      modified on Tue Jun 16 18:26:35 PDT 1992 by muller     *)

MODULE Calculator EXPORTS Main;

IMPORT CalculatorBundle, FloatMode, Fmt, FormsVBT, Lex, Rd, Rsrc, Scan, Text,
       Thread, Trestle, TrestleComm, VBT;

<* FATAL FormsVBT.Error, FormsVBT.Unimplemented *>
<* FATAL Rd.Failure, Rsrc.NotFound, Thread.Alerted, TrestleComm.Failure *>
<* FATAL Lex.Error, FloatMode.Trap *>

TYPE
  Closure = REF RECORD
    memory:   INTEGER;
    operator: Text.T;
    building: BOOLEAN;
  END;
  
PROCEDURE NewForm (): FormsVBT.T =
  VAR
    form := NEW(FormsVBT.T).initFromRsrc(
              "Calculator.fv",
              Rsrc.BuildPath(
                "$CALCULATORPATH", CalculatorBundle.Get()));
    cl := NEW(Closure);
  BEGIN
    FOR i := 0 TO 9 DO
      VAR ri := NEW(REF INTEGER);
      BEGIN
        ri^ := i;
        FormsVBT.AttachProc(
          form, "b" & Fmt.Int(i), DigitProc, ri);
      END
    END;
    FormsVBT.AttachProc(form, "bAdd", BinaryOpProc, "ADD");
    FormsVBT.AttachProc(form, "bSub", BinaryOpProc, "SUB");
    FormsVBT.AttachProc(form, "bMult", BinaryOpProc, "MUL");
    FormsVBT.AttachProc(form, "bDiv", BinaryOpProc, "DIV");
    FormsVBT.AttachProc(form, "bEquals", BinaryOpProc, "NOOP");
    FormsVBT.AttachProc(form, "display", BinaryOpProc, "NOOP");
    FormsVBT.AttachProc(form, "quit", QuitProc);
    cl.memory := 0;
    cl.operator := "NOOP";
    cl.building := FALSE;
    VBT.PutProp(form, cl);
    RETURN form;
  END NewForm;

PROCEDURE QuitProc (
                 fv      : FormsVBT.T;
    <* UNUSED *> event   : Text.T;
    <* UNUSED *> closure : REFANY;
    <* UNUSED *> time    : VBT.TimeStamp) =
  BEGIN
    Trestle.Delete (fv)
  END QuitProc;

PROCEDURE DigitProc (
                 fv      : FormsVBT.T;
    <* UNUSED *> event   : Text.T;
                 closure : REFANY;
    <* UNUSED *> time    : VBT.TimeStamp) =
  VAR
    cl := NARROW(VBT.GetProp(fv, TYPECODE(Closure)), Closure);
    value := NARROW(closure, REF INTEGER)^;
    acc: INTEGER;
  BEGIN
    IF cl.building THEN
      acc := Scan.Int(FormsVBT.GetText(fv, "display")) * 10 + value;
    ELSE
      acc := value;
      cl.building := TRUE;
    END;
    FormsVBT.PutText(fv, "display", Fmt.Int(acc));
  END DigitProc;

PROCEDURE BinaryOpProc (               
                 fv      : FormsVBT.T;
    <* UNUSED *> event   : Text.T;
                 closure : REFANY;
    <* UNUSED *> time    : VBT.TimeStamp) =
  VAR
    cl := NARROW(VBT.GetProp(fv, TYPECODE(Closure)), Closure);
    acc := Scan.Int(FormsVBT.GetText(fv, "display"));
  BEGIN
    IF Text.Equal(cl.operator, "ADD") THEN
      cl.memory := cl.memory + acc
    ELSIF Text.Equal(cl.operator, "SUB") THEN
      cl.memory := cl.memory - acc
    ELSIF Text.Equal(cl.operator, "MUL") THEN
      cl.memory := cl.memory * acc
    ELSIF Text.Equal(cl.operator, "DIV") THEN
      IF acc # 0 THEN cl.memory := cl.memory DIV acc END;
    ELSE
      cl.memory := acc;
    END;
    FormsVBT.PutText(fv, "display", Fmt.Int(cl.memory));
    cl.operator := NARROW(closure, Text.T);
    cl.building := FALSE;
  END BinaryOpProc;

BEGIN
  WITH z = NewForm() DO
    Trestle.Install(z);
    Trestle.AwaitDelete(z);
  END
END Calculator.
