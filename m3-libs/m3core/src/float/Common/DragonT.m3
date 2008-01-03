(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Jun 17 10:52:55 PDT 1994 by kalsow                   *)
(*      modified on Fri May  7 16:13:29 PDT 1993 by muller                   *)

MODULE DragonT;

IMPORT DragonInt;
FROM DragonInt IMPORT timesTenInPlace, divideTen, max, shift, compare, add;
FROM DragonInt IMPORT times2, copy, divmod, diff;

PROCEDURE F (e: INTEGER;
             f1, f0: INTEGER;
             p: INTEGER;
             cutoffMode: CutoffMode;
             cutoffPlace: INTEGER;
  VAR(*OUT*) digits: ARRAY OF Digit;
  VAR(*OUT*) count: CARDINAL;
  VAR(*OUT*) exp: INTEGER) =

  VAR
    roundUpFlag := FALSE;
    U, k: INTEGER;
    R, S, Mm, Mp: DragonInt.T;
    low, high: BOOLEAN;
    s: DragonInt.Session;
    f: DragonInt.T;

  PROCEDURE cutoffAdjust () =
    VAR a: INTEGER; y: DragonInt.T;
    BEGIN
      a := cutoffPlace - k;
      y := S;
      IF a >= 0 THEN 
        FOR j := 1 TO a DO y := timesTenInPlace (s, y); END;
      END;
      IF a <= 0 THEN 
        FOR j := 1 TO -a DO y := divideTen (s, y); END;
      END;
      Mm := max (s, y, Mm);
      Mp := max (s, y, Mp);
      IF compare (s, Mp, y) = 0 THEN roundUpFlag := TRUE; END;
    END cutoffAdjust;
    
  PROCEDURE fixup () = 
    BEGIN
      IF compare (s, f, shift (s, DragonInt.One, p-1)) = 0 THEN
        (* Account for unequal gaps *)
        Mp := times2 (s, Mp);
        R := times2 (s, R);
        S := times2 (s, S);
      END;
      
      k := 0;
      WHILE compare (s, R, divideTen (s, S)) < 0 DO
        DEC (k);
        R  := timesTenInPlace (s, R);
        Mm := timesTenInPlace (s, Mm);
        Mp := timesTenInPlace (s, Mp);
      END;
      
      REPEAT
        WHILE compare (s, add (s, times2 (s, R), Mp), times2 (s, S)) >= 0 DO
          S := timesTenInPlace (s, S);
          INC (k);
        END;
        
        (* Perform any necessary adjustment of Mm and Mp to take into account
           the formatting requirements *)
        CASE cutoffMode OF 
        | CutoffMode.normal =>      cutoffPlace := k;
        | CutoffMode.absolute =>    cutoffAdjust ();
        | CutoffMode.relative =>    INC (cutoffPlace, k);  cutoffAdjust ();
        END;
      UNTIL compare (s, add (s, times2 (s, R), Mp), times2 (s, S)) < 0;
    END fixup;

  BEGIN
    IF (f0 = 0) AND (f1 = 0) THEN
      count := 1;
      exp := 0;
      digits [0] := 0;
      RETURN;
    END;

    s := DragonInt.NewSession ();
    f := DragonInt.New (s, f1, f0);

    count := 0;
    R := shift (s, f, MAX (e - p, 0));
    S := shift (s, DragonInt.One, MAX (0, - (e - p)));
    Mm := shift (s, DragonInt.One, MAX (e - p, 0));
    Mp := copy (s, Mm);
    fixup ();

    LOOP
      DEC (k);
      R := divmod (s, timesTenInPlace (s, R), S, U);
      Mm := timesTenInPlace (s, Mm);
      Mp := timesTenInPlace (s, Mp);
      low := compare (s, times2 (s, R), Mm) < 0;
      
      VAR twoS := times2 (s, S); BEGIN
        IF compare (s, twoS, Mp) < 0 THEN
          high := TRUE;
        ELSIF roundUpFlag THEN
          high := compare (s, times2 (s, R), diff (s, twoS, Mp)) >= 0;
        ELSE
          high := compare (s, times2 (s, R), diff (s, twoS, Mp)) > 0;
        END;
      END;

      IF count = 0 THEN exp := k; END;

      IF low OR high OR (k = cutoffPlace) THEN EXIT; END;

      digits [count] := U;
      INC (count);
    END;

    IF low AND NOT high THEN
      digits [count] := U;
    ELSIF high AND NOT low THEN
      digits [count] := U + 1; 
    ELSE
      CASE compare (s, times2 (s, R), S) OF
      | -1 => digits [count] := U;
      | 0  => digits [count] := U  (* could be U + 1 *);
      | 1  => digits [count] := U + 1;
      END;
    END;
    INC (count);
    DragonInt.EndSession (s);
  END F;

BEGIN
END DragonT.
