(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Jan 30 15:42:34 PST 1995 by kalsow                   *)
(*      modified on Wed Jul 21 09:00:13 PDT 1993 by steveg                   *)
(*      modified on Fri Jun 19 20:34:19 PDT 1992 by muller                   *)

UNSAFE MODULE SLispMath EXPORTS SLisp, SLispMath;

IMPORT Atom, Fmt, Math, Random, RefList, SLispClass, Sx, Text;

VAR
  true: Atom.T;

<* FATAL Sx.PrintError *>

PROCEDURE Register (self: T) =
  <* FATAL Error *>
  BEGIN
    true := self.varEval("t");

    FOR i := FIRST(Comparisons) TO LAST(Comparisons) DO
      self.defineFun(
        NEW(BuiltinComp, name := CompNames[i], apply := Comparison,
            minArgs := 2, maxArgs := 2, comp := i));
    END;
    self.defineFun(NEW(Builtin, name := "or", apply := Or, minArgs := 1,
                       maxArgs := LAST(INTEGER)));
    self.defineFun(NEW(Builtin, name := "and", apply := And, minArgs := 1,
                       maxArgs := LAST(INTEGER)));
    self.defineFun(NEW(Builtin, name := "not", apply := Not, minArgs := 1,
                       maxArgs := 1));
    self.defineFun(NEW(Builtin, name := "+", apply := Plus, minArgs := 0,
                       maxArgs := LAST(INTEGER)));
    self.defineFun(NEW(Builtin, name := "*", apply := Times, minArgs := 0,
                       maxArgs := LAST(INTEGER)));
    self.defineFun(NEW(Builtin, name := "-", apply := Sub, minArgs := 0,
                       maxArgs := LAST(INTEGER)));
    self.defineFun(NEW(Builtin, name := "/", apply := Div, minArgs := 0,
                       maxArgs := LAST(INTEGER)));
    self.defineFun(NEW(Builtin, name := "float", apply := FloatFun,
                       minArgs := 1, maxArgs := 1));
    self.defineFun(NEW(Builtin, name := "round", apply := Round,
                       minArgs := 1, maxArgs := 1));
    self.defineFun(NEW(Builtin, name := "truncate", apply := Truncate,
                       minArgs := 1, maxArgs := 1));
    self.defineFun(NEW(Builtin, name := "sqrt", apply := Sqrt,
                       minArgs := 1, maxArgs := 1));
    self.defineFun(NEW(Builtin, name := "sin", apply := Sin,
                       minArgs := 1, maxArgs := 1));
    self.defineFun(NEW(Builtin, name := "cos", apply := Cos,
                       minArgs := 1, maxArgs := 1));
    self.defineFun(NEW(Builtin, name := "random", apply := RandomFun,
                       minArgs := 0, maxArgs := 0));
    self.defineFun(NEW(Builtin, name := "randomgen", apply := RandomGen,
                       minArgs := 1, maxArgs := 1));
    self.defineFun(NEW(Builtin, name := "randomnext", apply := RandomNext,
                       minArgs := 1, maxArgs := 1));
  END Register;

(* NumVal does 3 things:

   1) It converts its arg to a numeric value
   2) It returns whether the value is an integer or real
   3) It makes sure a related value has the same type

   So for the old value and new value, the resulting type is:
   integer and integer => integer
   real and real => real
   real and integer => real (convert new integer to real)
   integer and real => real (convert old integer to real)
*)
PROCEDURE NumVal (                 interp : T;
                                   arg    : Sexp;
                  VAR (* in/out *) real   : BOOLEAN;
                  VAR (* out *)    oldi, i: INTEGER;
                  VAR (* out *)    oldr, r: REAL ) RAISES {Error} =
  BEGIN
    TYPECASE arg OF
    | NULL =>
        EVAL
          interp.error(Fmt.F("NIL valued argument to a numeric operator"));
    | Integer (int) =>
        IF real THEN r := FLOAT(int^, REAL) ELSE i := int^ END;
    | Float (flt) =>
        r := flt^;
        IF NOT real THEN real := TRUE; oldr := FLOAT(oldi, REAL) END;
    ELSE
      EVAL interp.error(
             Fmt.F("\"%s\": non-numeric argument to a numeric operator",
                   SLispClass.SxToText(arg)));
    END;
  END NumVal;

TYPE
  BuiltinComp = Builtin OBJECT
    comp: Comparisons;
  END;

TYPE
  Comparisons = {Eq, Ne, Gt, Lt, Ge, Le};

CONST
  CompNames = ARRAY Comparisons OF TEXT{"eq", "ne", "gt", "lt", "ge", "le"};

CONST
  Less = SET OF Comparisons{Comparisons.Ne, Comparisons.Le, Comparisons.Lt};
  Equal = SET OF Comparisons{Comparisons.Eq, Comparisons.Le, Comparisons.Ge};
  More = SET OF Comparisons{Comparisons.Ne, Comparisons.Ge, Comparisons.Gt};

PROCEDURE CompMismatch (interp: T; comp: Comparisons; s1, s2: Sexp): Sexp
  RAISES {Error} =
  BEGIN
    CASE comp OF
    | Comparisons.Eq => RETURN NIL
    | Comparisons.Ne => RETURN true
    ELSE
      RETURN
        interp.error(
          Fmt.F("Cannot compare: %s, %s", SLispClass.SxToText(s1), SLispClass.SxToText(s2)));
    END;
  END CompMismatch;

PROCEDURE Comparison (self: BuiltinComp; interp: T; args: List): Sexp
  RAISES {Error} =
  VAR
    i1, i2: INTEGER;
    r1, r2: REAL;
    res   : BOOLEAN;
    real            := FALSE;
    comp            := self.comp;
    a1              := interp.eval(args.head);
    a2              := interp.eval(args.tail.head);
  BEGIN
    TYPECASE a1 OF
    | NULL =>
        TYPECASE a2  OF
        | NULL => 
           CASE comp OF
           | Comparisons.Eq, Comparisons.Le, Comparisons.Ge => RETURN true
           | Comparisons.Ne => RETURN NIL
           ELSE
             RETURN CompMismatch(interp, comp, a1, a2);
           END;
        ELSE
           CASE comp OF
           | Comparisons.Eq => RETURN NIL
           | Comparisons.Ne => RETURN true
           ELSE
             RETURN CompMismatch(interp, comp, a1, a2);
           END;
        END;
    | Symbol (s1) =>
        TYPECASE a2 OF
        | Symbol (s2) =>
            CASE Text.Compare(Atom.ToText(s1), Atom.ToText(s2)) OF
            | -1 => res := comp IN Less;
            | 0 => res := comp IN Equal;
            | 1 => res := comp IN More;
            END;
        ELSE
          RETURN CompMismatch(interp, comp, a1, a2);
        END;
    | String (s1) =>
        TYPECASE a2 OF
        | String (s2) =>
            CASE Text.Compare(s1, s2) OF
            | -1 => res := comp IN Less;
            | 0 => res := comp IN Equal;
            | 1 => res := comp IN More;
            END;
        ELSE
          RETURN CompMismatch(interp, comp, a1, a2);
        END;
    | List =>
        TYPECASE a2 OF
        | List =>
            EVAL interp.error(Fmt.F("Cannot compare lists: %s, %s",
                                    SLispClass.SxToText(a1), SLispClass.SxToText(a2)));
        ELSE
          CASE comp OF
          | Comparisons.Eq => RETURN NIL
          | Comparisons.Ne => RETURN true
          ELSE
            EVAL interp.error(Fmt.F("Cannot compare lists: %s, %s",
                                    SLispClass.SxToText(a1), SLispClass.SxToText(a2)));
          END;
        END;
    | Integer, Float =>
        TYPECASE a2 OF
        | Integer, Float =>
            NumVal(interp, a1, real, i2, i1, r2, r1);
            NumVal(interp, a2, real, i1, i2, r1, r2);
            IF real THEN
              CASE comp OF
              | Comparisons.Eq => res := r1 = r2;
              | Comparisons.Ne => res := r1 # r2;
              | Comparisons.Gt => res := r1 > r2;
              | Comparisons.Lt => res := r1 < r2;
              | Comparisons.Ge => res := r1 >= r2;
              | Comparisons.Le => res := r1 <= r2;
              END;
            ELSE
              CASE comp OF
              | Comparisons.Eq => res := i1 = i2;
              | Comparisons.Ne => res := i1 # i2;
              | Comparisons.Gt => res := i1 > i2;
              | Comparisons.Lt => res := i1 < i2;
              | Comparisons.Ge => res := i1 >= i2;
              | Comparisons.Le => res := i1 <= i2;
              END;
            END;
        ELSE
          RETURN CompMismatch(interp, comp, a1, a2);
        END;
    ELSE
      CASE comp OF
      | Comparisons.Eq => res := a1 = a2;
      | Comparisons.Ne => res := a1 # a2;
      ELSE
        RETURN CompMismatch(interp, comp, a1, a2);
      END;
    END;
    IF res THEN RETURN true ELSE RETURN NIL END;
  END Comparison;

PROCEDURE Or (<*UNUSED*> self: Builtin; interp: T; args: List): Sexp RAISES {Error} =
  VAR res: Sexp;
  BEGIN
    WHILE args # NIL DO
      res := interp.eval (args.head);
      IF res # NIL THEN
        RETURN res; END;
      args := args.tail; END;
    RETURN res;
  END Or;

PROCEDURE And (<*UNUSED*> self: Builtin; interp: T; args: List): Sexp RAISES {Error} =
  VAR res: Sexp;
  BEGIN
    WHILE args # NIL DO
      res := interp.eval (args.head);
      IF res = NIL THEN
        RETURN NIL; END;
      args := args.tail; END;
    RETURN res;
  END And;

PROCEDURE Not(<*UNUSED*> self: Builtin; interp: T; args: List): Sexp RAISES {Error} =
  VAR res: Sexp;
  BEGIN
    res := interp.eval (args.head);
    IF res = NIL THEN
      RETURN true; 
    ELSE
      RETURN NIL
    END;
  END Not;

PROCEDURE Plus (<*UNUSED*> self: Builtin; interp: T; args: List): Sexp
  RAISES {Error} =
  VAR
    real            := FALSE;
    i1, i2: INTEGER;
    r1, r2: REAL;
    ires  : Integer;
    fres  : Float;
  BEGIN
    i1 := 0;
    WHILE args # NIL DO
      NumVal(interp, interp.eval(args.head), real, i1, i2, r1, r2);
      IF real THEN r1 := r1 + r2 ELSE i1 := i1 + i2; END;
      args := args.tail;
    END;
    IF real THEN
      fres := NEW(Float);
      fres^ := r1;
      RETURN fres;
    ELSE
      ires := NEW(Integer);
      ires^ := i1;
      RETURN ires;
    END;
  END Plus;

PROCEDURE Sub (<*UNUSED*> self: Builtin; interp: T; args: List): Sexp
  RAISES {Error} =
  VAR
    real            := FALSE;
    i1, i2: INTEGER;
    r1, r2: REAL;
    ires  : Integer;
    fres  : Float;
  BEGIN
    IF args = NIL THEN
      i1 := 0;
    ELSE
      NumVal(interp, interp.eval(args.head), real, i2, i1, r2, r1);
      args := args.tail;
      IF args = NIL THEN (* unary minus *)
        IF real THEN r1 := -r1 ELSE i1 := -i1 END;
      ELSE
        WHILE args # NIL DO
          NumVal(interp, interp.eval(args.head), real, i1, i2, r1, r2);
          IF real THEN r1 := r1 - r2 ELSE i1 := i1 - i2; END;
          args := args.tail;
        END;
      END;
    END;
    IF real THEN
      fres := NEW(Float);
      fres^ := r1;
      RETURN fres;
    ELSE
      ires := NEW(Integer);
      ires^ := i1;
      RETURN ires;
    END;
  END Sub;

PROCEDURE Times (<*UNUSED*> self: Builtin; interp: T; args: List): Sexp RAISES {Error} =
  VAR
    real            := FALSE;
    i1, i2: INTEGER;
    r1, r2: REAL;
    ires  : Integer;
    fres  : Float;
  BEGIN
    i1 := 1;
    WHILE args # NIL DO
      NumVal(interp, interp.eval(args.head), real, i1, i2, r1, r2);
      IF real THEN r1 := r1 * r2 ELSE i1 := i1 * i2; END;
      args := args.tail;
    END;
    IF real THEN
      fres := NEW(Float);
      fres^ := r1;
      RETURN fres;
    ELSE
      ires := NEW(Integer);
      ires^ := i1;
      RETURN ires;
    END;
  END Times;

PROCEDURE Div (<*UNUSED*> self: Builtin; interp: T; args: List): Sexp
  RAISES {Error} =
  VAR
    real            := FALSE;
    i1, i2: INTEGER;
    r1, r2: REAL;
    ires  : Integer;
    fres  : Float;
  BEGIN
    IF args = NIL THEN
      i1 := 1;
    ELSE
      NumVal(interp, interp.eval(args.head), real, i2, i1, r2, r1);
      args := args.tail;
      IF args = NIL THEN         (* unary divide? *)
        IF real THEN
          IF r1 = 0.0 THEN EVAL interp.error("divide by 0.0"); END;
          r1 := 1.0 / r1
        ELSE
          IF i1 = 0 THEN EVAL interp.error("divide by 0"); END;
          i1 := 1 DIV i1
        END;
      ELSE
        WHILE args # NIL DO
          NumVal(interp, interp.eval(args.head), real, i1, i2, r1, r2);
          IF real THEN
            IF r2 = 0.0 THEN EVAL interp.error("divide by 0.0"); END;
            r1 := r1 / r2
          ELSE
            IF i2 = 0 THEN EVAL interp.error("divide by 0"); END;
            i1 := i1 DIV i2;
          END;
          args := args.tail;
        END;
      END;
    END;
    IF real THEN
      fres := NEW(Float);
      fres^ := r1;
      RETURN fres;
    ELSE
      ires := NEW(Integer);
      ires^ := i1;
      RETURN ires;
    END;
  END Div;

PROCEDURE FloatFun (<*UNUSED*> self: Builtin; interp: T; args: List): Sexp
  RAISES {Error} =
  VAR i := interp.eval(args.head);
  BEGIN
    TYPECASE i OF
    | NULL => RETURN interp.error("Can't \"float\" nil");
    | Integer (ri) => WITH f = NEW(Float) DO f^ := FLOAT(ri^); RETURN f END;
    | Float => RETURN i
    ELSE
      RETURN
        interp.error(Fmt.F("\"%s\" should be an integer", SLispClass.SxToText(i)));
    END;
  END FloatFun;

PROCEDURE Truncate (<*UNUSED*> self: Builtin; interp: T; args: List): Sexp
  RAISES {Error} =
  VAR r := interp.eval(args.head);
  BEGIN
    TYPECASE r OF
    | NULL => RETURN interp.error("Can't \"truncate\" nil");
    | Integer (ri) => RETURN ri
    | Float (f) => WITH i = NEW(Integer) DO i^ := TRUNC(f^); RETURN i END;
    ELSE
      RETURN
        interp.error(Fmt.F("\"%s\" should be a real", SLispClass.SxToText(r)));
    END;
  END Truncate;

PROCEDURE Round (<*UNUSED*> self: Builtin; interp: T; args: List): Sexp
  RAISES {Error} =
  VAR r := interp.eval(args.head);
  BEGIN
    TYPECASE r OF
    | NULL => RETURN interp.error("Can't \"round\" nil");
    | Integer (ri) => RETURN ri
    | Float (f) => WITH i = NEW(Integer) DO i^ := ROUND(f^); RETURN i END;
    ELSE
      RETURN
        interp.error(Fmt.F("\"%s\" should be a real", SLispClass.SxToText(r)));
    END;
  END Round;

PROCEDURE Sqrt (<*UNUSED*> self: Builtin; interp: T; args: List): Sexp
  RAISES {Error} =
  VAR
    r   := interp.eval(args.head);
    res := NEW(Float);
  BEGIN
    TYPECASE r OF
    | NULL => RETURN interp.error("Can't \"sqrt\" nil");
    | Integer (ri) => res^ := FLOAT(Math.sqrt(FLOAT(ri^, LONGREAL)));
    | Float (f) => res^ := FLOAT(Math.sqrt(FLOAT(f^, LONGREAL)));
    ELSE
      RETURN interp.error(Fmt.F("\"%s\" should be a real", SLispClass.SxToText(r)));
    END;
    RETURN res;
  END Sqrt;

PROCEDURE Sin (<*UNUSED*> self: Builtin; interp: T; args: List): Sexp
  RAISES {Error} =
  VAR
    r   := interp.eval(args.head);
    res := NEW(Float);
  BEGIN
    TYPECASE r OF
    | NULL => RETURN interp.error("Can't \"sin\" nil");
    | Integer (ri) => res^ := FLOAT(Math.sin(FLOAT(ri^, LONGREAL)));
    | Float (f) => res^ := FLOAT(Math.sin(FLOAT(f^, LONGREAL)));
    ELSE
      RETURN interp.error(Fmt.F("\"%s\" should be a real", SLispClass.SxToText(r)));
    END;
    RETURN res;
  END Sin;

PROCEDURE Cos (<*UNUSED*> self: Builtin; interp: T; args: List): Sexp
  RAISES {Error} =
  VAR
    r   := interp.eval(args.head);
    res := NEW(Float);
  BEGIN
    TYPECASE r OF
    | NULL => RETURN interp.error("Can't \"cos\" nil");
    | Integer (ri) => res^ := FLOAT(Math.cos(FLOAT(ri^, LONGREAL)));
    | Float (f) => res^ := FLOAT(Math.cos(FLOAT(f^, LONGREAL)));
    ELSE
      RETURN interp.error(
               Fmt.F("\"%s\" should be a real", SLispClass.SxToText(r)));
    END;
    RETURN res;
  END Cos;

PROCEDURE RandomFun (<*UNUSED*>   self  : Builtin;
                     <* UNUSED *> interp: T;
                     <* UNUSED *> args  : List     ): Sexp =
  BEGIN
    WITH f = NEW(Float) DO
      f^ := NEW(Random.Default).init().real();
      RETURN f
    END;
  END RandomFun;

PROCEDURE RandomGen (<*UNUSED*>   self  : Builtin;
                     <* UNUSED *> interp: T;
                                  args  : List     ): Sexp =
  VAR fixed := (RefList.Length(args) = 0) OR (args.head # NIL);
  BEGIN
    RETURN NEW(Random.Default).init(fixed);
  END RandomGen;

PROCEDURE RandomNext (<*UNUSED*> self: Builtin; interp: T; args: List):
  Sexp RAISES {Error} =
  VAR r := interp.eval(args.head);
  BEGIN
    IF NOT ISTYPE(r, Random.Default) THEN
      RETURN interp.error(Fmt.F("\"%s\" should be a Random.T",
                                SLispClass.SxToText(r)));
    END;
    VAR rg := NARROW(r, Random.T);
    BEGIN
      RETURN Sx.FromReal(rg.real())
    END
  END RandomNext;

BEGIN
END SLispMath.
