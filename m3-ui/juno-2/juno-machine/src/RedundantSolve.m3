(* Copyright (C) 1992, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* Last modified on Thu Apr 10 09:52:22 PDT 1997 by heydon   *)
(*      modified on Thu Dec  8 15:44:33 PST 1994 by gnelson  *)
(*      modified on Mon Oct 31 18:34:31 PST 1994 by isard    *)

MODULE RedundantSolve;

IMPORT RedundantLSolve, JunoValue, NonLinearSolveRep;
FROM RedundantLSolve IMPORT logWr;
FROM JunoValue IMPORT Real;
FROM NonLinearSolveRep IMPORT conAvail, conInUse, ConType;

IMPORT IO, Wr, Fmt, Text, Stdio;
FROM Thread IMPORT Alerted;
<* FATAL Wr.Failure, Alerted *>

VAR debug := 0; oneStep := FALSE; logFileName := "solver.log";
(* debug >= 1 => show input & solution
   debug >= 2 => show values & deltas at each iteration
   debug >= 3 => show values and roundoff errors at each iteration
   oneStep => take only one Newton step each solve; always report success
   logFile => name of file logging output is sent to; if NIL, output is
              sent to stdout
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

VAR
  MaxIterations := 20;
  MinDelta := 1.0e-7;
  RoundFudge := FLOAT(16.0, JunoValue.Real);

(* A constraint is considered satisfied if its error is at most "RoundFudge"
   times the estimated round-off error involved in computing it. 

   A change to a coordinate of the solution that has relative value less than
   "MinDelta" is considered tiny.  When all changes are tiny, the iteration
   stops. *)

CONST
  InitRows = 20;
  InitCols = 40;
  InitErrB = 100;

VAR
  a := NEW(REF RedundantLSolve.Matrix, InitRows, InitCols + 1); (* matrix *)
  x := NEW(REF RedundantLSolve.Vector, InitCols);        (* solution vector *)
  errorVec := NEW(REF RedundantLSolve.Vector, InitRows); (* one/constraint *)
  errb := NEW(REF RedundantLSolve.Vector, InitErrB);     (* error bound *)

CONST
  NameWidth = 9;
  Prec = 3;
  FieldWidth = Prec + 8;

PROCEDURE ShowVector(
    name: TEXT;
    READONLY v: RedundantLSolve.Vector;
    size: CARDINAL) =
  BEGIN
    Wr.PutText(logWr, "  ");
    IF size > 0 THEN
      Wr.PutText(logWr, Fmt.Pad(name, NameWidth - 2, align := Fmt.Align.Left));
      FOR i := 0 TO size - 1 DO
    	Wr.PutText(logWr,
          Fmt.Pad(Fmt.Real(v[i], Fmt.Style.Sci, prec := 3), FieldWidth))
      END
    ELSE
      Wr.PutText(logWr, "<none>")
    END;
    Wr.PutChar(logWr, '\n');
  END ShowVector;

PROCEDURE EtpLogP0(<*UNUSED*> true_cnt, nn, ghost_cnt, iterations: CARDINAL) =
(* true_cnt = # of true constraints; nn = # of true vars; ghost_cnt = # of
   ghost vars and constraints, "iterations" is the number of iterations. *)
  BEGIN END EtpLogP0;

PROCEDURE EvalRHS(
   c: Constraint; 
   READONLY v: ARRAY OF T; 
   READONLY errb: ARRAY OF T;
   n: CARDINAL;
   VAR (*OUT*) res: T; 
   VAR (*OUT*) err: T;
   supressNewline := FALSE) =
 (* Set "res" and "err" to the value and estimated round-off error of the
    right side of "c", assuming that "v" and "errb" give the value and
    round-off errors for the variables appearing in the right side of "c".
    The argument "res" may be aliased with an element of "v" that doesn't
    occur in the right side of "c", and "err" may be aliased to an element of
    "errb" that doesn't occur in the right side of "c". *)
 BEGIN
   WITH 
     arg = c.arg,
     y = v[arg[1]],
     x = v[arg[2]],
     dely = errb[arg[1]],
     delx = errb[arg[2]]
   DO
     (* res := c.type(y,x) *)
     CASE c.type OF <*NOWARN*>
     | ConType.Plus =>  
         res := y + x;
         err := dely + delx 
     | ConType.Minus =>  
         res := y - x;
         err := dely + delx 
     | ConType.Halve =>  
         res := y * 0.5;
         err := dely * 0.5 
     | ConType.Times =>  
         res := y * x;
         err := dely * ABS(x) + delx * ABS(y) 
     | ConType.Atan =>   
         res := JunoValue.Atan(y, x);
         err := (delx * ABS(y) + dely * ABS(x)) / ABS(x*x+y*y) 
     | ConType.Sin =>    
         res := JunoValue.Sin(y);
         err := ABS(JunoValue.Cos(y)) * dely 
     | ConType.Cos =>    
         res := JunoValue.Cos(y);
         err := ABS(JunoValue.Sin(y)) * dely 
     | ConType.MultTan =>
         WITH 
           tanx = JunoValue.Tan(x),
           cosx = JunoValue.Cos(x)
         DO
           res := y * tanx;
           err := dely * ABS(tanx) + delx / (cosx * cosx)
         END
     | ConType.Exp => 
         WITH
           expy = JunoValue.Exp(y)
         DO   
           res := expy;
           err := expy * dely
         END
     END;
     IF c.type # ConType.Halve THEN
       err := err + JunoValue.HalfEps * ABS(res)
     END;
     IF debug >= 3 THEN
       ShowConstraint(c, v, n, supressNewline)
     END
   END
 END EvalRHS;

PROCEDURE IndexVal(i, n: CARDINAL; READONLY v: ARRAY OF T): TEXT =
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

PROCEDURE ShowConstraint(
    con: Constraint;
    READONLY v: ARRAY OF T;
    n: CARDINAL;
    supressNewline := FALSE) =
  BEGIN
    Wr.PutText(logWr, "  " & IndexVal(con.arg[0], n, v) & " = ");
    CASE con.type OF <* NOWARN *>
      ConType.Plus =>
        Wr.PutText(logWr, IndexVal(con.arg[1],n,v)
          & " + " & IndexVal(con.arg[2],n,v))
    | ConType.Minus =>
        Wr.PutText(logWr, IndexVal(con.arg[1],n,v)
          & " - " & IndexVal(con.arg[2],n,v))
    | ConType.Halve =>
        Wr.PutText(logWr, IndexVal(con.arg[1],n,v) & " / 2")
    | ConType.Times =>
        Wr.PutText(logWr, IndexVal(con.arg[1],n,v)
          & " * " & IndexVal(con.arg[2],n,v))
    | ConType.Atan =>
        Wr.PutText(logWr, "ATAN(" & IndexVal(con.arg[1],n,v)
          & ", " & IndexVal(con.arg[2],n,v) & ")")
    | ConType.Sin =>
        Wr.PutText(logWr, "SIN(" & IndexVal(con.arg[1],n,v) & ")")
    | ConType.Cos =>
        Wr.PutText(logWr, "COS(" & IndexVal(con.arg[1],n,v) & ")")
    | ConType.MultTan =>
        Wr.PutText(logWr, IndexVal(con.arg[1],n,v)
          & "* TAN("& IndexVal(con.arg[2],n,v) & ")")
    | ConType.Exp =>
        Wr.PutText(logWr, "EXP(" & IndexVal(con.arg[1],n,v) & ")")
    END;
    IF NOT supressNewline THEN Wr.PutChar(logWr, '\n') END
  END ShowConstraint;

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
    allUpdatesTiny: BOOLEAN;             (* set by "UpdateVars" *)
    goodEnough: BOOLEAN;                 (* set by "EvalConstraints" *)

  PROCEDURE EvalConstraints(): T =
  (* Evaluate the ghost variables, and set "errorVec[i]" to the error in
     true constraint "i", for "i" in "[ghost_cnt..m-1]". Return the maximum
     absolute error of any true constraint.  Also set "errb[i]" to the
     estimated round-off error in the value of "v[i]", for each "i"
     in "[0..n-1]", and set "goodEnough" to record whether all constraints
     were met as closely as can be expected given the rounding error
     involved in computing them. *)
    VAR result: T := 0.0; rhsval, rhserr, diff: T; BEGIN
      goodEnough := TRUE;
      IF debug >= 3 THEN IO.Put("Evaluating constraints:\n", logWr) END;
      FOR i := FIRST(v) TO LAST(v) DO
        errb[i] := JunoValue.HalfEps * ABS(v[i])
      END;
      FOR i := 0 TO ghost_cnt - 1 DO
        WITH ci = c[i], arg0 = ci.arg[0] DO
          EvalRHS(ci, v, errb^, n, (*OUT*) v[arg0], (*OUT*) errb[arg0])
        END
      END;
      FOR i := ghost_cnt TO m - 1 DO
        WITH ci = c[i], arg0 = ci.arg[0] DO
          EvalRHS(ci, v, errb^, n, (*OUT*) rhsval, (*OUT*) rhserr,
            supressNewline := (debug >= 3));
          IF debug >= 3 THEN
            IO.Put(" = ", logWr);   IO.PutReal(rhsval, logWr);
            IO.Put(" +- ", logWr);  IO.PutReal(rhserr, logWr);
            IO.Put(" vs. ", logWr); IO.PutReal(v[arg0], logWr);
            IO.Put(" +- ", logWr);  IO.PutReal(errb[arg0], logWr)
          END;
          diff := rhsval - v[arg0];
          errorVec[i] := diff;
          diff := ABS(diff);
          IF diff > RoundFudge * (rhserr + errb[arg0]) THEN
            goodEnough := FALSE;
            IF debug >= 3 THEN IO.Put(" -> BAD", logWr) END
          END
        END;
        IF debug >= 3 THEN IO.Put("\n", logWr) END;
        IF diff > result THEN result := diff END
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

  PROCEDURE UpdateVars() =
    BEGIN
      allUpdatesTiny := TRUE;
      FOR i := 0 TO nn - 1 DO
        WITH val = v[i] DO
          allUpdatesTiny := allUpdatesTiny AND (ABS(x[i]) < MinDelta*ABS(val));
          val := val + x[i]
        END
      END
    END UpdateVars;

  PROCEDURE ShowInput() =
    PROCEDURE P(lo, hi: INTEGER; kind: TEXT) =
      BEGIN
        Wr.PutText(logWr, "RedundantSolve.P " & kind & " constraints:\n");
        IF lo > hi
          THEN Wr.PutText(logWr, "  <none>\n")
          ELSE FOR i := lo TO hi DO ShowConstraint(c[i], v, n) END
        END
      END P;
    BEGIN
      IF debug >= 1 THEN
        IO.Put("\n" & Fmt.Pad("", length := 45, padChar := '*') & "\n", logWr)
      END;
      P(0, ghost_cnt - 1, "ghost");
      P(ghost_cnt, LAST(c), "true");
      IO.Put("RedundantSolve.P hints:\n", logWr);
      IF nn > 0 THEN
      	Wr.PutText(logWr, Fmt.Pad("", NameWidth));
      	FOR i := 0 TO nn - 1 DO
      	  Wr.PutText(logWr,
            Fmt.Pad(IndexVal(i, n, v), FieldWidth))
      	END;
        Wr.PutChar(logWr, '\n')
      END;
      ShowVector("Values", v, nn);
      Wr.Flush(logWr)
    END ShowInput;

  PROCEDURE ShowValuesAndErrors(itNum: INTEGER) =
    BEGIN
      Wr.PutText(logWr, "Iteration " & Fmt.Int(itNum) & ":\n");
      ShowVector("Values:", v, nn);
      WITH used_err_vec = SUBARRAY(errorVec^, ghost_cnt, true_cnt) DO
        ShowVector("Errors:", used_err_vec, true_cnt)
      END
    END ShowValuesAndErrors;

  PROCEDURE ShowSolution() =
    BEGIN
      Wr.PutText(logWr, "Solution Found:\n");
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
	  x := NEW(REF RedundantLSolve.Vector, n2);
	  a := NEW(REF RedundantLSolve.Matrix, m2, n2 + 1);
	  errorVec := NEW(REF RedundantLSolve.Vector, m2)
	END
      END;
      IF NUMBER(v) > NUMBER(errb^) THEN
        VAR 
          n2 := MAX(NUMBER(v), 2 * NUMBER(errb^));
        BEGIN
          errb := NEW(REF RedundantLSolve.Vector, n2)
        END
      END
    END GrowArrays;

  (* PROCEDURE P *)
  BEGIN
    IF logWr = NIL AND debug > 0 THEN 
      IF logFileName # NIL
        THEN logWr := IO.OpenWrite(logFileName)
        ELSE logWr := Stdio.stdout
      END
    END;
    <* ASSERT NUMBER(x^) + 1 = NUMBER(a[0]) *>
    <* ASSERT NUMBER(errorVec^) = NUMBER(a^) *>
    GrowArrays();
    IF debug >= 1 THEN ShowInput() END;
    VAR 
      cnt := MaxIterations;
      error := EvalConstraints();
    BEGIN
      IF true_cnt > 0 THEN
        TRY
          WHILE (NOT goodEnough) AND (cnt > 0) DO
            IF debug >= 2 THEN ShowValuesAndErrors(MaxIterations - cnt) END;
            BuildMatrix();
            WITH matrix = SUBARRAY(a^, ghost_cnt, true_cnt) DO
              RedundantLSolve.P(true_cnt, nn, matrix, x^)
            END;
            IF debug >= 2 THEN ShowVector("Deltas", x^, nn) END;
      	    UpdateVars();
      	    IF allUpdatesTiny THEN EXIT END;
      	    IF oneStep THEN RETURN TRUE END;
      	    error := EvalConstraints();
      	    DEC(cnt)
      	  END;
          IF debug >= 1 THEN
            IF goodEnough
              THEN ShowSolution()
              ELSE IO.Put("No Solution Found.\n\n", logWr)
            END
      	  END;
          RETURN goodEnough
      	FINALLY
      	  EtpLogP0(true_cnt, nn, ghost_cnt, MaxIterations-cnt);
      	  IF debug >= 2 THEN Wr.Flush(logWr) END
        END (* TRY *)
      END; (* IF *)
      RETURN TRUE
    END
  END P;

(*
PROCEDURE SolveLoop(): BOOLEAN =
  CONST
    MaxCount = 20;
  VAR
    count := 0;
    goodEnough, allUpdatesTiny: BOOLEAN;
  BEGIN
    LOOP
      EVAL EvalConstraints();
      IF goodEnough OR (count = MaxCount) THEN EXIT END;
      BuildMatrix();
      EVAL RedundantLSolve();
      UpdateVars();
      IF allUpdatesTiny THEN EXIT END;
      INC(count)
    END;
    RETURN goodEnough
  END
END;
*)

BEGIN
END RedundantSolve.
