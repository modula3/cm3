(* Copyright (c) 2000 Mika Nystrom.  All Rights Reserved. *)
(* $Id$ *)
MODULE Newton;
IMPORT Quantity,Matrix,Fmt;
IMPORT Debug;
IMPORT LU;

TYPE
  Vector = REF ARRAY OF LONGREAL;

PROCEDURE Jacobian(funcs : Quantity.Vector; 
                   vars : REF ARRAY OF REF LONGREAL) : Quantity.Matrix 
  RAISES { DimensionError, Quantity.Recursive } =
  VAR
    j := NEW(Quantity.Matrix,NUMBER(funcs^),NUMBER(vars^));
  BEGIN
    IF NUMBER(vars^) # NUMBER(funcs^) THEN RAISE DimensionError END;
    FOR row := FIRST(funcs^) TO LAST(funcs^) DO
      FOR col := FIRST(vars^) TO LAST(vars^) DO
        j[row,col] := funcs[row].derivative(vars[col]);
      END
    END;
    RETURN j
  END Jacobian;

PROCEDURE EvaluateVector(v : Quantity.Vector) : Vector 
  RAISES { Quantity.IllegalOperands, Quantity.Recursive } =
  VAR
    values := NEW(Vector, NUMBER(v^));
  BEGIN
    FOR i:= FIRST(values^) TO LAST(values^) DO
      values[i] := v[i].value()
    END;
    RETURN values
  END EvaluateVector;

PROCEDURE EvaluateMatrix(m : Quantity.Matrix) : Matrix.T
  RAISES { Quantity.IllegalOperands, Quantity.Recursive } =
  VAR
    values := NEW(Matrix.T, NUMBER(m^), NUMBER(m[0]));
  BEGIN
    FOR row := FIRST(values^) TO LAST(values^) DO
      FOR col := FIRST(values[0]) TO LAST(values[0]) DO
        values[row,col] := m[row,col].value()
      END
    END;
    RETURN values
  END EvaluateMatrix;


PROCEDURE Solve(vars : REF ARRAY OF REF LONGREAL; 
                funcs : Quantity.Vector;
                maxsteps : CARDINAL;
                tolerance : LONGREAL (* total tolerance in positions *) ) 
  RAISES { DimensionError, NoConvergence, Matrix.Singular, 
           Quantity.Recursive } =
  (* XXX not done: Quantity.IllegalOperands SHOULD BE CAUGHT HERE.
     We could make a reliable solver that simply recovers from division
     by zero or logs of negative numbers... *)
  <* FATAL Matrix.DimensionMismatch *>
  VAR
    jacobian := Jacobian(funcs, vars);
  BEGIN TRY
    Debug.Out("Newton's method solving system:");
    FOR i := FIRST(funcs^) TO LAST(funcs^) DO
      Debug.Out(Fmt.Int(i) & ": " & funcs[i].format(FALSE)) 
    END;
    
    (*
    Debug.Out("Jacobian:");
    FOR i := FIRST(jacobian^) TO LAST(jacobian^) DO
      FOR j := FIRST(jacobian[0]) TO LAST(jacobian[0]) DO
        Debug.Out(Fmt.Int(i) & "," & Fmt.Int(j) &": " & 
          jacobian[i,j].format(FALSE))
      END
    END;
    *)

    FOR step := 1 TO maxsteps DO
      VAR
        f := EvaluateVector(funcs);
        j := EvaluateMatrix(jacobian);
        p := NEW(Vector, NUMBER(funcs^));
        d : LONGREAL;
        indx := NEW(REF ARRAY OF INTEGER, NUMBER(funcs^));
      BEGIN
        Debug.Out("Jacobian :\n" & Matrix.Format(j));
        VAR
          varStr := "";
        BEGIN
          FOR i := 0 TO LAST(vars^) DO 
            varStr := varStr & " " & Fmt.LongReal(vars[i]^)
          END;
          Debug.Out("Values   :\n" & varStr & "\n")
        END;
        Debug.Out("Funcs    :\n" & Matrix.Format(Matrix.RowVector(f)));

        FOR i := FIRST(p^) TO LAST(p^) DO p[i] := -f[i] END;

         j := Matrix.Zap(j,1.0d-7);

        LU.Decompose(j,indx,d); (* what is d for? *)
        VAR det := d; BEGIN
          FOR i := FIRST(j^) TO LAST(j^) DO det := det * j[i,i] END;
          Debug.Out("Det(j) = " & Fmt.LongReal(det))
        END;

        Debug.Out("j after LU-decomp:\n" & Matrix.Format(j));

(*        VAR lu := Matrix.Mul(Matrix.L(j),Matrix.U(j)); BEGIN
          Debug.Out("LU: \n" & Matrix.Format(lu));
       END;
*)        
        Debug.Out("To solve:\n" & Matrix.Format(Matrix.ColVector(p)));
        LU.BackSubstitute(j,indx,p);

        Debug.Out("LU-Step    :\n" & Matrix.Format(Matrix.ColVector(p)));

        Debug.Out("J * p = \n" & 
        Matrix.Format(Matrix.Mul(EvaluateMatrix(jacobian),
                                 Matrix.ColVector(p))));

        VAR
          error : LONGREAL;
          printvector := NEW(Vector,NUMBER(vars^));
          oldVars := NEW(Vector,NUMBER(vars^));
          oldError := EvaluateError(funcs);
          step := 1.0d0;
          hadProblems := FALSE;
        CONST
          (* parameters for global convergence algorithm *)
          descent = 0.8d0; (* descent rate (exponential) *)
          alpha = 0.00001d0; (* min. improvement to continue *)
        BEGIN

          (* copy vars *)
          FOR i:= FIRST(oldVars^) TO LAST(oldVars^) DO 
            oldVars[i] := vars[i]^ 
          END;

          LOOP
            (* this is the globally convergent part *)
            FOR i:= FIRST(p^) TO LAST(p^) DO
              vars[i]^ := vars[i]^ + p[i];
              printvector[i] := vars[i]^;
            END;
            TRY
              error := EvaluateError(funcs);
              Debug.Out("Funcs    :\n" & 
                Matrix.Format(Matrix.RowVector(EvaluateVector(funcs))));

              Debug.Out("Error: " & Fmt.LongReal(error) &
                "   oldError: " & Fmt.LongReal(oldError) &
                "   tolerance: " & Fmt.LongReal(tolerance) &
                "   descent: " & Fmt.LongReal(oldError-error) &
                "   step: " & Fmt.LongReal(step));
              
              Debug.Out("New values:\n" &
                Matrix.Format(Matrix.RowVector(printvector)));
              Debug.Out("Total absolute error = " &
                Fmt.LongReal(error) );
            EXCEPT
            | Quantity.IllegalOperands => 
              error := 1.0d0 + tolerance;
              Debug.Out("Quantity.IllegalOperands caught!");
              hadProblems := TRUE
            END;

            (* lets not be so careful if we had operand problems *)
            IF hadProblems AND oldError > error THEN EXIT END;

            IF error < tolerance OR 
              oldError-error > alpha * oldError * step THEN 
              EXIT  (* this step improved the situation *)
            ELSIF step < 0.0001d0 THEN 
              (* hasnt improved yet, and isnt going to... *)
              
              Debug.Out("Raising NoConvergence, step = " & 
                Fmt.LongReal(step));
              RAISE NoConvergence(FindMaxError(funcs))

            ELSE (* not converged yet, but might... *)
              FOR i:= 0 TO LAST(p^) DO
                p[i] := p[i] * descent;
                vars[i]^ := oldVars[i]
              END;
              step := step * descent
            END

          END; (* LOOP *)
          IF error < tolerance THEN RETURN END;
        END (* VAR *)
      END; (* VAR *)
    END; (* FOR step *)
    RAISE NoConvergence(FindMaxError(funcs))
  EXCEPT 
  | Quantity.IllegalOperands(q) =>
    Debug.Error("Illegal operands in Newton solver, quantity = " &
      q.format() & " value: " & Fmt.LongReal(q.value()) )
  END END Solve;

PROCEDURE FindMaxError(funcs : Quantity.Vector) : CARDINAL RAISES { Quantity.IllegalOperands, Quantity.Recursive } =
  
  (* find worst offender *)
  VAR
    maxFunc := -1;
    maxError := 0.0d0;
  BEGIN
    FOR i := FIRST(funcs^) TO LAST(funcs^) DO
      WITH abs = ABS(funcs[i].value()) DO
        IF abs > maxError THEN
          maxError := abs; maxFunc := i
        END
      END
    END;
    RETURN maxFunc
  END FindMaxError;

PROCEDURE EvaluateError(v : Quantity.Vector) : LONGREAL RAISES { Quantity.IllegalOperands, Quantity.Recursive } =
  VAR
    x := EvaluateVector(v);
    res := 0.0d0;
  BEGIN
    FOR i := FIRST(x^) TO LAST(x^) DO
      res := res + ABS(x[i])
    END;
    RETURN res
  END EvaluateError;

BEGIN
END Newton.
