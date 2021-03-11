(* $Id$ *)

MODULE BDDTruthTable;
IMPORT CardSet, BDD;
IMPORT Word;
IMPORT Debug;
IMPORT Fmt;

VAR doDebug := FALSE AND Debug.GetLevel() > 5;

PROCEDURE Dbg(z : TEXT) = BEGIN Debug.Out(z, 5) END Dbg;

PROCEDURE Make(zz            : BDD.T;
               READONLY vars : ARRAY OF BDD.T;
               set           : CardSet.T) : T =

  PROCEDURE Recurse(x : BDD.T; p : CARDINAL) =
    VAR z : BDD.T; BEGIN
      IF p = sz THEN 
        (* base case *)
        <*ASSERT x = BDD.True() OR x = BDD.False()*>
        
        VAR r : BOOLEAN; BEGIN
          IF x = BDD.True() THEN 
            r := TRUE
          ELSIF x = BDD.False() THEN
            r := FALSE
          ELSE
            <*ASSERT FALSE*>
          END;

          tt[rowVals][colVals] := r
        END
      ELSE
        (* not base case *)
        IF p < outN THEN
          (* modify row *)
          WITH idx = p DO
            z := BDD.MakeFalse(x, rowVars[idx]);
            Recurse(z, p + 1);
            rowVals := Word.Insert(rowVals, 1, idx, 1);
            z := BDD.MakeTrue(x, rowVars[idx]);
            Recurse(z, p + 1);
            rowVals := Word.Insert(rowVals, 0, idx, 1)
          END
        ELSE
          WITH idx = p - outN DO
            (* modify col *)
            z := BDD.MakeFalse(x, colVars[idx]);
            Recurse(z, p + 1);
            colVals := Word.Insert(colVals, 1, idx, 1);
            z := BDD.MakeTrue(x, colVars[idx]);
            Recurse(z, p + 1);
            colVals := Word.Insert(colVals, 0, idx, 1)
          END            
        END
      END
    END Recurse;

  PROCEDURE SetupVars() =
    VAR rp, cp := 0;
    BEGIN
      FOR i := 0 TO sz-1 DO
        IF set.member(i) THEN 
          colVars[cp] := vars[i]; INC(cp)
        ELSE
          rowVars[rp] := vars[i]; INC(rp)
        END
      END
    END SetupVars;

  VAR
    sz := NUMBER(vars);
    (* use set for the cols, ~set for the rows *)
    inN  := set.size();
    outN := sz - set.size();

    rows := Word.LeftShift(1, outN);
    cols := Word.LeftShift(1, inN);

    rowVars := NEW(REF ARRAY OF BDD.T, outN);
    colVars := NEW(REF ARRAY OF BDD.T, inN);

    rowVals : Word.T := 0;
    colVals : Word.T := 0;

    tt := NEW(REF ARRAY OF ARRAY OF BOOLEAN, rows, cols);

  BEGIN
    SetupVars();

    Recurse(zz, 0);

    IF doDebug THEN
      Dbg("==== Truth table! ====");
      FOR i := 0 TO rows-1 DO
        VAR rw := ""; BEGIN
          FOR j := 0 TO cols-1 DO
            rw := rw & Fmt.Int(ARRAY BOOLEAN OF [0..1] { 0, 1 }[tt[i,j]])
          END;
          Dbg(rw)
        END
      END
    END;

    RETURN T { rowVars, colVars, tt }

  END Make;

BEGIN END BDDTruthTable.
