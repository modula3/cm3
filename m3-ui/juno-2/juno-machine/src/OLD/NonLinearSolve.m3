(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Oct 31 18:30:37 PST 1994 by isard                    *)
(*      modified on Mon Oct 31 09:35:27 PST 1994 by heydon                   *)
(*      modified on Sun Sep  6 20:02:41 PDT 1992 by gnelson                  *)

MODULE NonLinearSolve EXPORTS NonLinearSolve, NonLinearSolveRep;

IMPORT LinearSolve, JunoValue;
FROM JunoValue IMPORT Real;

IMPORT Wr, Fmt, Text;
FROM Stdio IMPORT stderr;
FROM Thread IMPORT Alerted;
<* FATAL Wr.Failure, Alerted *>

VAR debug := 0; oneStep := FALSE;
(* debug >= 1 => show input & solution
   debug >= 2 => show values & deltas at each iteration
   oneStep => take only one Newton step each solve; always report success
*)

TYPE T = Real;

PROCEDURE NewCon(type: ConType): Constraint =
  VAR res: Constraint; BEGIN
    IF conAvail # NIL
      THEN res := conAvail; conAvail := conAvail.availLink
      ELSE res := NEW(Constraint)
    END;
    res.availLink := conInUse;
    conInUse := res;
    res.type := type;
    RETURN res
  END NewCon;

PROCEDURE NewPlus(): Constraint =
  BEGIN RETURN NewCon(ConType.Plus) END NewPlus;

PROCEDURE NewMinus(): Constraint =
  BEGIN RETURN NewCon(ConType.Minus) END NewMinus;

PROCEDURE NewHalve(): Constraint =
  BEGIN RETURN NewCon(ConType.Halve) END NewHalve;

PROCEDURE NewTimes(): Constraint =
  BEGIN RETURN NewCon(ConType.Times) END NewTimes;

PROCEDURE NewSin(): Constraint =
  BEGIN RETURN NewCon(ConType.Sin) END NewSin;

PROCEDURE NewCos(): Constraint =
  BEGIN RETURN NewCon(ConType.Cos) END NewCos;

PROCEDURE NewAtan(): Constraint =
  BEGIN RETURN NewCon(ConType.Atan) END NewAtan;

PROCEDURE NewMultTan(): Constraint =
  BEGIN RETURN NewCon(ConType.MultTan) END NewMultTan;

PROCEDURE NewExp(): Constraint =
  BEGIN RETURN NewCon(ConType.Exp) END NewExp;

PROCEDURE Dispose() =
  VAR l := conInUse; BEGIN
    IF l # NIL THEN
      WHILE l.availLink # NIL DO l := l.availLink END;
      l.availLink := conAvail;
      conAvail := conInUse;
      conInUse := NIL
    END
  END Dispose;

CONST
  MaxIterations = 20;

VAR (* READONLY *)
  MinDelta := 1.0e-3;                    (* minimum progress to continue *)
  MaxError := 1.0e-3;			 (* maximum tolerated absolute error *)

CONST
  InitRows = 20;
  InitCols = 40;

VAR
  a := NEW(REF LinearSolve.Matrix, InitRows, InitCols + 1); (* matrix *)
  x := NEW(REF LinearSolve.Vector, InitCols);        (* solution vector *)
  errorVec := NEW(REF LinearSolve.Vector, InitRows); (* one per constraint *)

CONST
  NameWidth = 9;
  Prec = 3;
  FieldWidth = Prec + 8;

PROCEDURE ShowVector(
    name: TEXT;
    READONLY v: LinearSolve.Vector;
    size: CARDINAL) =
  BEGIN
    Wr.PutText(stderr, "  ");
    IF size > 0 THEN
      Wr.PutText(stderr, Fmt.Pad(name & ":", NameWidth - 2,
        align := Fmt.Align.Left));
      FOR i := 0 TO size - 1 DO
    	Wr.PutText(stderr,
    	  Fmt.Pad(Fmt.Real(v[i], Fmt.Style.Sci, prec := 3), FieldWidth))
      END
    ELSE
      Wr.PutText(stderr, "<none>")
    END;
    Wr.PutChar(stderr, '\n');
  END ShowVector;

PROCEDURE EtpLogP0(<*UNUSED*> true_cnt, nn, ghost_cnt: CARDINAL) =
(* true_cnt = # of true constraints; nn = # of true vars; ghost_cnt = # of
   ghost vars and constraints. *)
  BEGIN END EtpLogP0;

PROCEDURE P(
    nn, n: CARDINAL;
    VAR v: ARRAY OF T;
    READONLY c: ARRAY OF Constraint): BOOLEAN =
(*
| ON ENTRY:
| 
|	    v[]                
|	  ________                   
|	 |        |                  
|	 |  True  |                  
|	 |  Vars  |                  
|	 |        |        c[] 
|	 |________|    _____________ 
|  nn -> |        |   |             |
|	 |        |   |             |
|	 |  Ghost |   |    Ghost    |
|	 |  Vars  |   | Constraints |
|	 |        |   |             |
|	 |________|   |_____________|
|   n -> |........|   |             | <- ghost_cnt
|	 |........|   |    True     |
|	 |________|   | Constraints |
|	 |        |   |             |
|	 | Consts |   |_____________|
|	 |________|                   <- m (total # of constraints)
|
| MATRIX ORGANIZATION:
|
|		x[]
|	____________________         __ 
|      |                    |       |..|
|      |____________________|       |..| = unused
|                                   |__|
|      | <------ nn ------> |
|    
|    
|	       a[][]             errorVec[]
|       _______________________      __
|  ^   |                    |  |    |..|
|  |   |    Ghost           |  |    |..|
|  |   |    Equations       |  |    |..|
|  |   |____________________|__|    |__|
|  m   |                    |  |    |  | <- ghost_cnt
|  |   |    True            |  |    |  |
|  |   |    Equations       |  |    |  |
|  V   |____________________|__|    |__|
|                                        <- m
|      | <------ nn + 1 -----> |
*)
  VAR
    m := NUMBER(c);			 (* number of constraints (total) *)
    ghost_cnt := n - nn;		 (* number of ghost constraints *)
    true_cnt := m - ghost_cnt;		 (* number of true constraints *)

  PROCEDURE EvalConstraints(): T =
  (* Evaluate the ghost variables, and set "errorVec[i]" to the error in
     true constraint "i", for "i" in "[ghost_cnt..m-1]". Return the maximum
     absolute error of any true constraint.*)
    VAR result: T := 0.0; diff: T; BEGIN
      FOR i := 0 TO ghost_cnt - 1 DO
        WITH arg = c[i].arg DO
          CASE c[i].type OF <* NOWARN *>
          | ConType.Plus =>   v[arg[0]] := v[arg[1]] + v[arg[2]]
          | ConType.Minus =>  v[arg[0]] := v[arg[1]] - v[arg[2]]
          | ConType.Halve =>  v[arg[0]] := v[arg[1]] / 2.0
          | ConType.Times =>  v[arg[0]] := v[arg[1]] * v[arg[2]]
          | ConType.Atan =>   v[arg[0]] := JunoValue.Atan(v[arg[1]],v[arg[2]])
          | ConType.Sin =>    v[arg[0]] := JunoValue.Sin(v[arg[1]])
          | ConType.Cos =>    v[arg[0]] := JunoValue.Cos(v[arg[1]])
          | ConType.MultTan =>v[arg[0]] := v[arg[1]] * JunoValue.Tan(v[arg[2]])
          | ConType.Exp =>    v[arg[0]] := JunoValue.Exp(v[arg[1]])
          END
        END
      END;
      FOR i := ghost_cnt TO m - 1 DO
        WITH arg = c[i].arg, v0 = v[arg[0]], v1 = v[arg[1]] DO
          CASE c[i].type OF <* NOWARN *>
          | ConType.Plus =>    diff := (v1 + v[arg[2]]) - v0
          | ConType.Minus =>   diff := (v1 - v[arg[2]]) - v0
          | ConType.Halve =>   diff := (v1 / 2.0) - v0
          | ConType.Times =>   diff := (v1 * v[arg[2]]) - v0
          | ConType.Atan =>    diff := JunoValue.Atan(v1, v[arg[2]]) - v0
          | ConType.Sin =>     diff := JunoValue.Sin(v1) - v0
          | ConType.Cos =>     diff := JunoValue.Cos(v1) - v0
          | ConType.MultTan => diff := (v1 * JunoValue.Tan(v[arg[2]])) - v0
          | ConType.Exp =>     diff := JunoValue.Exp(v1)- v0
          END;
        END;
        errorVec[i] := diff;
        diff := ABS(diff);
        IF diff > result THEN result := diff END;
      END;
      RETURN result
    END EvalConstraints;

  PROCEDURE ZeroMatrix() =
    BEGIN
      FOR i := 0 TO m - 1 DO
        WITH row = a[i] DO
          FOR j := 0 TO nn - 1 DO
            row[j] := 0.0
          END
        END
      END
    END ZeroMatrix;

  PROCEDURE BuildMatrix() =
  (* Fill in the matrix "a" to solve one step of the iteration for the
     constraints "c" using the current values of the variables "v". *)
    BEGIN
      ZeroMatrix();
      (* compute gradients of ghost constraints *)
      FOR i := 0 TO ghost_cnt - 1 DO
        WITH con = c[i] DO
          GradCon(con, con.arg[0] - nn)
        END
      END;
      (* compute gradients of true constraints *)
      FOR i := ghost_cnt TO m - 1 DO
        WITH con = c[i] DO
          GradCon(con, i);
          GradVar(con.arg[0], i, -1.0);
          a[i, nn] := -(errorVec[i])
        END
      END;
    END BuildMatrix;

  PROCEDURE GradCon(con: Constraint; i: CARDINAL) =
  (* Set "row" to the gradient of the right hand side of "con". *)
    BEGIN
      CASE con.type OF <* NOWARN *>
      | ConType.Plus =>
          GradVar(con.arg[1], i, 1.0);
          GradVar(con.arg[2], i, 1.0)
      | ConType.Minus =>
          GradVar(con.arg[1], i,  1.0);
          GradVar(con.arg[2], i, -1.0)
      | ConType.Halve =>
          GradVar(con.arg[1], i, 0.5)
      | ConType.Times =>
          WITH arg1 = con.arg[1], arg2 = con.arg[2] DO
            GradVar(arg1, i, v[arg2]);
            GradVar(arg2, i, v[arg1])
          END
      | ConType.Sin =>
          WITH arg1 = con.arg[1] DO
            GradVar(arg1, i, JunoValue.Cos(v[arg1]))
          END
      | ConType.Cos =>
          WITH arg1 = con.arg[1] DO
            GradVar(arg1, i, -(JunoValue.Sin(v[arg1])))
          END
      | ConType.Atan =>
          (* d(atan2(y, x))
             = d(atan(y/x))
             = (1 / (1 + ((y/x)^2))) * ((x dy - y dx) / (x^2))
             = (1 / (x^2 + y^2)) * (x dy - y dx) *)
          WITH arg1 = con.arg[1], arg2 = con.arg[2] DO
            VAR y := v[arg1]; x := v[arg2]; denom := x*x + y*y; BEGIN
              GradVar(arg1, i,  x / denom);
              GradVar(arg2, i, -y / denom)
            END
          END
      | ConType.MultTan =>
          (* d(x * tan(y))
             = tan(y) dx + x * sec^2(y) dy
             = tan(y) dx + x * (1 + tan^2(y)) *)
          WITH arg1 = con.arg[1], arg2 = con.arg[2] DO
            VAR k := JunoValue.Tan(v[arg2]); BEGIN
              GradVar(arg1, i, k);
              GradVar(arg2, i, v[arg1] * (1.0 + k*k))
            END
          END
      | ConType.Exp =>
          WITH arg1 = con.arg[1] DO
            GradVar(arg1, i, JunoValue.Exp(v[arg1]))
          END
      END
    END GradCon;

  PROCEDURE GradVar(var, i: CARDINAL; k: T) =
  (* Add "k" to the coefficient of variable "var" in "row". However, if "var"
     is a ghost variable, we add "k" times the row in "a" for "var" to "row".
  *)
    BEGIN
      IF var < nn THEN
        (* true variable *)
        WITH row = a[i] DO
          row[var] := row[var] + k
        END
      ELSIF var < n THEN
        (* ghost variable *)
        WITH row = a[i], varRow = a[var - nn] DO
          FOR j := 0 TO nn DO
            row[j] := row[j] + (k * varRow[j])
          END
        END
      END
    END GradVar;

  PROCEDURE MaxDeltaRatio(READONLY x: LinearSolve.Vector): T =
  (* Returns the maximum of "ABS(x[i]/v[i])". *)
    VAR res: T := -1.0; BEGIN
      FOR i := 0 TO nn - 1 DO
        WITH val = v[i] DO
          IF val = 0.0 THEN RETURN LAST(JunoValue.Real) END;
          res := MAX(res, ABS(x[i]/val))
        END
      END;
      RETURN res
    END MaxDeltaRatio;

  PROCEDURE UpdateVars() =
    BEGIN
      FOR i := 0 TO nn - 1 DO
        WITH val = v[i] DO
          val := val + x[i]
        END
      END
    END UpdateVars;

  PROCEDURE IndexVal(i: CARDINAL): TEXT =
    BEGIN
      IF i >= n THEN RETURN Fmt.Real(v[i]) END;
      VAR res := ""; BEGIN
        LOOP
          res := Text.FromChar(VAL((i MOD 26) + ORD('a'), CHAR)) & res;
          IF i < 26 THEN EXIT END;
          i := i DIV 26
        END;
        RETURN res
      END
    END IndexVal;

  PROCEDURE ShowConstraint(i: CARDINAL) =
    VAR con := c[i]; BEGIN
      Wr.PutText(stderr, "  ");
      CASE c[i].type OF <* NOWARN *>
        ConType.Plus =>
          Wr.PutText(stderr, IndexVal(con.arg[0]) & " = "
            & IndexVal(con.arg[1]) & " + " & IndexVal(con.arg[2]))
      | ConType.Minus =>
          Wr.PutText(stderr, IndexVal(con.arg[0]) & " = "
            & IndexVal(con.arg[1]) & " - " & IndexVal(con.arg[2]))
      | ConType.Halve =>
          Wr.PutText(stderr, IndexVal(con.arg[0]) & " = "
            & IndexVal(con.arg[1]) & " / 2")
      | ConType.Times =>
          Wr.PutText(stderr, IndexVal(con.arg[0]) & " = "
            & IndexVal(con.arg[1]) & " * " & IndexVal(con.arg[2]))
      | ConType.Atan =>
          Wr.PutText(stderr, IndexVal(con.arg[0]) & " = ATAN("
            & IndexVal(con.arg[1]) & ", " & IndexVal(con.arg[2]) & ")")
      | ConType.Sin =>
          Wr.PutText(stderr, IndexVal(con.arg[0]) & " = SIN("
            & IndexVal(con.arg[1]) & ")")
      | ConType.Cos =>
          Wr.PutText(stderr, IndexVal(con.arg[0]) & " = COS("
            & IndexVal(con.arg[1]) & ")")
      | ConType.MultTan =>
          Wr.PutText(stderr, IndexVal(con.arg[0]) & " = "
            & IndexVal(con.arg[1]) & "* TAN(" & IndexVal(con.arg[2]) & ")")
      | ConType.Exp =>
          Wr.PutText(stderr, IndexVal(con.arg[0]) & " = EXP("
            & IndexVal(con.arg[1]) & ")")
      END;
      Wr.PutChar(stderr, '\n')
    END ShowConstraint;

  PROCEDURE ShowInput() =
    PROCEDURE P(lo, hi: INTEGER; kind: TEXT) =
      BEGIN
        Wr.PutText(stderr, "NonLinearSolve.P " & kind & " constraints:\n");
        IF lo > hi
          THEN Wr.PutText(stderr, "  <none>\n")
          ELSE FOR i := lo TO hi DO ShowConstraint(i) END
        END
      END P;
    BEGIN
      P(0, ghost_cnt - 1, "ghost");
      P(ghost_cnt, LAST(c), "true");
      Wr.PutText(stderr, "NonLinearSolve.P hints:\n");
      IF nn > 0 THEN
      	Wr.PutText(stderr, Fmt.Pad("", NameWidth));
      	FOR i := 0 TO nn - 1 DO
      	  Wr.PutText(stderr, Fmt.Pad(IndexVal(i), FieldWidth))
      	END;
        Wr.PutChar(stderr, '\n')
      END;
      ShowVector("Values", v, nn);
      Wr.Flush(stderr)
    END ShowInput;

  PROCEDURE ShowValuesAndErrors(itNum: INTEGER) =
    BEGIN
      Wr.PutText(stderr, "Iteration " & Fmt.Int(itNum) & ":\n");
      ShowVector("Values", v, nn);
      WITH used_err_vec = SUBARRAY(errorVec^, ghost_cnt, true_cnt) DO
        ShowVector("Errors", used_err_vec, true_cnt)
      END
    END ShowValuesAndErrors;

  PROCEDURE ShowSolution() =
    BEGIN
      Wr.PutText(stderr, "Solution Found:\n");
      ShowVector("Values", v, nn)
    END ShowSolution;

  PROCEDURE GrowArrays() =
  (* If necessary, make the arrays "x", "a", and "errorVec" larger. *)
    BEGIN
      IF nn > NUMBER(x^) OR m > NUMBER(errorVec^) THEN
	VAR
	  n2 := MAX(nn, 2 * NUMBER(x^));
	  m2 := MAX(m, 2 * NUMBER(errorVec^));
	BEGIN
	  x := NEW(REF LinearSolve.Vector, n2);
	  a := NEW(REF LinearSolve.Matrix, m2, n2 + 1);
	  errorVec := NEW(REF LinearSolve.Vector, m2)
	END
      END
    END GrowArrays;

  <* FATAL LinearSolve.BadShapes *>

  (* PROCEDURE P *)
  BEGIN
    <* ASSERT NUMBER(x^) + 1 = NUMBER(a[0]) *>
    <* ASSERT NUMBER(errorVec^) = NUMBER(a^) *>
    EtpLogP0(true_cnt, nn, ghost_cnt);
    GrowArrays();
    IF debug >= 1 THEN ShowInput() END;
    VAR 
      cnt := MaxIterations;
      error := EvalConstraints();
      prev_delta := MinDelta + 1.0; (* establish prev_delta > MinDelta *)
    BEGIN
      WHILE error > MaxError AND prev_delta > MinDelta AND cnt > 0 DO
        IF debug >= 2 THEN ShowValuesAndErrors(MaxIterations - cnt) END;
        BuildMatrix();
        WITH matrix = SUBARRAY(a^, ghost_cnt, true_cnt) DO
          IF NOT LinearSolve.P(true_cnt, nn, matrix, x^) THEN EXIT END
        END;
        IF debug >= 2 THEN ShowVector("Deltas", x^, nn) END;
        prev_delta := MaxDeltaRatio(x^);
        UpdateVars();
        IF oneStep THEN RETURN TRUE END;
        error := EvalConstraints();
        DEC(cnt)
      END;
      IF error <= MaxError OR prev_delta <= MinDelta THEN
        IF debug >= 1 THEN ShowSolution() END;
        RETURN TRUE
      ELSE
        IF debug >= 1 THEN
          Wr.PutText(stderr, "No Solution Found.\n\n");
          Wr.Flush(stderr)
        END;
        RETURN FALSE
      END
    END (* WHILE *)
  END P;

BEGIN
END NonLinearSolve.
