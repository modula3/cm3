(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Copyright 1990 Digital Equipment Corporation.            *)
(* Distributed only by permission.                          *)
(* Last modified on Wed Jul  7 17:15:56 PDT 1993 by kalsow  *)
(*      modified on Tue Dec 17  7:48:37 PST 1991 by mcjones *)
(*      modified on Fri May 31 11:25:39 PDT 1991 by ellis   *)
(*      modified on     Dec 13 14:10    PST 1991 by saxe    *)

MODULE Main;

IMPORT Test, Wr, TextWr, Thread;
    
TYPE A = ARRAY [0..10] OF INTEGER;
 
EXCEPTION 
    E;
    EI (INTEGER);
    EA (A);
    EL (LONGREAL);

PROCEDURE RaiseE () RAISES {E} =
  BEGIN
    RAISE E;
  END RaiseE;

PROCEDURE RaiseEI (i: INTEGER) RAISES {EI} =
  BEGIN
    RAISE EI (i);
  END RaiseEI;


PROCEDURE RaiseEL (l: LONGREAL) RAISES {EL} =
  BEGIN
    RAISE EL (l);
  END RaiseEL;


PROCEDURE RaiseEA (VAR a: A) RAISES {EA} =
  BEGIN
    RAISE EA (a);
  END RaiseEA;


(*------------------------------------------------------ Basic TRY EXCEPT ---*)

PROCEDURE P1 () =
  VAR i: INTEGER;
  BEGIN
    i := 1;
    TRY RaiseE ();
    EXCEPT E => i := 2;
    END;
    Test.checkI (i, 2);
  END P1;


(*---------------------------------------- TRY EXCEPT with small argument ---*)

PROCEDURE P2 () =
  BEGIN
    TRY RaiseEI (10);
    EXCEPT
    | E =>
    | EI (i) =>  Test.checkI (i, 10); RETURN;
    END;
    Test.check (FALSE);
  END P2;


(*---------------------------------------- TRY EXCEPT with large argument ---*)

PROCEDURE P3 () =
  VAR a: A;
  BEGIN
    FOR i := FIRST (a) TO LAST (a) DO a[ i ] := i * i; END;
    TRY RaiseEA (a);
    EXCEPT
    | E =>
    | EA (b) =>
        FOR i := FIRST (a) TO LAST (a) DO Test.checkI (a[ i ], b[ i ]) ; END;
        RETURN;
        END;
    Test.check (FALSE);
  END P3;


(*------------------------------- Simple contiguous and nested TRY scopes ---*)

PROCEDURE P4 () =
  VAR i: INTEGER;
  BEGIN
    TRY i := 4;
    EXCEPT E => i := 5;
    END;
    TRY
      TRY RaiseE ();
      EXCEPT E => i := 6;
      END;
    EXCEPT E => i := 7;
    END;
    Test.checkI (i, 6);
  END P4;

(*-------------------------------------------------------- RAISES clauses ---*)

PROCEDURE P5 () =
  VAR i := 0;
  BEGIN
    TRY P6 ();
    EXCEPT 
    | E => i := 1;
    ELSE   i := 2;
    END;
    Test.checkI (i, 2);
  END P5;

PROCEDURE P6 () RAISES {EA, EI} =
  BEGIN
    RaiseEI (4);
  END P6;

(*----------------------------------------------------------- TRY PASSING ---*)
(*---   M3 doesn't have TRY-PASSING -----
PROCEDURE P7 () =
  VAR i: INTEGER;
  BEGIN
    i := 0;
    TRY P8 ();
    EXCEPT 
    | E => i := 1;
    ELSE   i := 2;
    END;
    Test.checkI (i, 2);
  END P7;

PROCEDURE P8 () =
  BEGIN
    TRY RaiseE (); PASSING {EA, EI} END;
  END P8;
----------------------------*)  

(*-------------------------------------------------------- Simple FINALLY ---*)

VAR i: INTEGER;

PROCEDURE P9 () =
  BEGIN
    i := 1;
    TRY P10 (2);
    EXCEPT E => (*SKIP*)
    END;
    Test.checkI (i, 7);
  END P9;

PROCEDURE P10 (j: INTEGER) RAISES {E} =
  BEGIN
    TRY P11 (3);
    FINALLY
        Test.checkI (i, 5);
        i := i + j;
    END;
  END P10;

PROCEDURE P11 (j: INTEGER) RAISES {E} =
  BEGIN
    TRY
      TRY RaiseE ();
      FINALLY
        Test.checkI (i, 1);
        i := i + j;
      END;
    FINALLY
      Test.checkI (i, 4);
      INC (i);
    END;
  END P11;

(*---------------------------------- FINALLY referencing nested variables ---*)

PROCEDURE P12 () =

    PROCEDURE J (j: INTEGER) RAISES {E} =
        PROCEDURE K (k: INTEGER) RAISES {E} =
          BEGIN
            TRY RaiseE ();
            FINALLY i := i + k + j;
            END;
          END K;
      BEGIN
        TRY K (5);
        FINALLY i := i + j;
        END;
      END J;

  VAR i := 1;
  BEGIN
    TRY J (3);
    EXCEPT E => (*SKIP*)
    END;
    Test.checkI (i, 12);
  END P12;

(*--------------------------------------------------- RETURN from FINALLY ---*)

PROCEDURE P13 () =

    PROCEDURE Q () RAISES {E} =
      BEGIN
        TRY RaiseE ();
        FINALLY
          i := i + 1;
          RETURN;
        END;
        i := i * 2; <*NOWARN*>
      END Q;

  VAR i := 5;
  BEGIN
    TRY Q ();
    EXCEPT E => (*SKIP*)
    END;
    Test.checkI (i, 6);
  END P13;


(*----------------------------------------------------- EXIT from FINALLY ---*)

PROCEDURE P14 () =

    PROCEDURE Q () RAISES {E} =
      VAR j := 4;
      BEGIN
        LOOP
          TRY RaiseE ();
          FINALLY
            i := i + j;
            EXIT;
          END;
          i := i + 1000; <*NOWARN*>
        END;
        i := i * j;
      END Q;

  VAR i := 5;
  BEGIN
    TRY Q ();
    EXCEPT E => (*SKIP*)
    END;
    Test.checkI (i, 36);
  END P14;

(*----------------------------------------- Uncaught RAISE within FINALLY ---*)

PROCEDURE P15 () =

    PROCEDURE Q () RAISES {E, EI} =
    BEGIN
      TRY RaiseE ();
      FINALLY RaiseEI (3);
      END;
    END Q;

  VAR i := 1;
  BEGIN
    TRY Q ();
    EXCEPT
    | E      =>  Test.check (FALSE);
    | EI (x) =>  Test.checkI (x, 3);
    END;
    Test.checkI (i, 1);
  END P15;

(*--------------------------- Caught RAISE within FINALLY, same exception ---*)

PROCEDURE P16 () =

    PROCEDURE Q () RAISES {EI} =
      VAR i: INTEGER;
      BEGIN
        TRY RaiseEI (3);
        FINALLY
          i := 2;
          TRY RaiseEI (4);
          EXCEPT EI (x) => Test.checkI (x, 4);
          END;
          Test.checkI (i, 2);
        END;
      END Q;

  VAR i := 2;
  BEGIN
    TRY Q ();
    EXCEPT EI (x) => Test.checkI (x, 3);
    END;
    Test.checkI (i, 2);
  END P16; 

(*---------------------- Caught RAISE within FINALLY, different exception ---*)

PROCEDURE P17 () =

    PROCEDURE Q () RAISES {EI} =
      VAR i: LONGREAL;
      BEGIN
        TRY RaiseEI (3);
        FINALLY
          i := 2.0D0;
          TRY RaiseEL (4.5D0);
          EXCEPT EL (x) =>  Test.checkL (x, 4.5D0);
          END;
          Test.checkL (i, 2.0D0);
        END;
      END Q;

  VAR i := 2;
  BEGIN
    TRY Q ();
    EXCEPT EI (x) => Test.checkI (x, 3);
    END;
    Test.checkI (i, 2);
  END P17;
               
(*------------------------------- Tail call allowed out of a RAISES scope ---*)

PROCEDURE P18 () =
  BEGIN
    TRY P18A ();
    EXCEPT E, EI => (*SKIP*)
    END;
  END P18;

PROCEDURE P18A () RAISES {E, EI} =
  BEGIN
    P18B ();
  END P18A;

PROCEDURE P18B () RAISES {E} =
  BEGIN
  END P18B;

(*--------------------------- Tail call not allowed out of a RAISES scope ---*)

PROCEDURE P19 () =
  BEGIN
    TRY P19A ();
    EXCEPT E, EI => (*SKIP*)
    END;
  END P19;

PROCEDURE P19A () RAISES {E, EI} =
  <*FATAL EA*>
  BEGIN
    P19B ();
  END P19A;

PROCEDURE P19B () RAISES {E, EA} =
  BEGIN
  END P19B;


(*------------------------------ Tail call allowed out of a PASSING scope ---*)
(**********
PROCEDURE P20 () =
  BEGIN
    TRY P20A (); PASSING {E, EI} END;
  END P20;

PROCEDURE P20A () RAISES {EI} =
  BEGIN
  END P20A;
****************)


(*-------------------------- Tail call not allowed out of a PASSING scope ---*)
(***************
PROCEDURE P21 () =
  BEGIN
    TRY P21A (); PASSING {E, EI} END;
  END P21;

PROCEDURE P21A () RAISES {EA} =
  BEGIN
  END P21A;
*******************)


(*------------------------------ Tail call allowed out of an EXCEPT scope ---*)

PROCEDURE P22 () =
  <*FATAL EA*>
  BEGIN
    TRY P22A ();
    EXCEPT E, EI => (*SKIP*)
    END;
  END P22;

PROCEDURE P22A () RAISES {EA} =
  BEGIN
  END P22A;


(*-------------------------- Tail call not allowed out of an EXCEPT scope ---*)

PROCEDURE P23 () =
  BEGIN
    TRY P23A ();
    EXCEPT E, EI => (*SKIP*)
    END;
    TRY P23A ();
    EXCEPT ELSE (*SKIP*)
    END;
  END P23;

PROCEDURE P23A () RAISES {E} =
  BEGIN
  END P23A;

    
(*-------------------------- Tail call not allowed out of a FINALLY scope ---*)

PROCEDURE P24 () =
  BEGIN
    TRY P24A ();
    FINALLY (*SKIP*)
    END;
  END P24;

PROCEDURE P24A () RAISES {} =
  BEGIN
  END P24A;


(*-------------------- Test RAISE in FINALLY with another RAISE on stack. ---*)

PROCEDURE P25 () =
  VAR raised := FALSE;  finally := FALSE;
  BEGIN
    TRY
      TRY
        raised := TRUE;
        RAISE E;
      FINALLY
        finally := TRUE;
        RAISE E;
      END;
      Test.check (FALSE); <*NOWARN*>
    EXCEPT E =>
      Test.check (raised AND finally);
    END;
  END P25;

(*----- Test RAISE in FINALLY, handled in the finally,
                                             with another RAISE on stack. ---*)

PROCEDURE P26 () =
  VAR x := ARRAY [0..10] OF BOOLEAN { FALSE, .. };
  BEGIN
    x[ 0 ] := TRUE;
    TRY
      x[ 1 ] := TRUE;
      TRY
        x[ 2 ] := TRUE;
        RAISE E;
        x[ 3 ] := TRUE; <*NOWARN*>
      FINALLY
        x[ 4 ] := TRUE;
        TRY
          x[ 5 ] := TRUE;
          RAISE E;
          x[ 6 ] := TRUE; <*NOWARN*>
        EXCEPT E =>
          x[ 7 ] := TRUE;
        END;
        x[ 8 ] := TRUE;
      END;
      x[ 9 ] := TRUE; <*NOWARN*>
    EXCEPT E =>
      x[ 10 ] := TRUE;
    END;
    Test.checkB (x [0], TRUE);
    Test.checkB (x [1], TRUE);
    Test.checkB (x [2], TRUE);
    Test.checkB (x [3], FALSE);
    Test.checkB (x [4], TRUE);
    Test.checkB (x [5], TRUE);
    Test.checkB (x [6], FALSE);
    Test.checkB (x [7], TRUE);
    Test.checkB (x [8], TRUE);
    Test.checkB (x [9], FALSE);
    Test.checkB (x [10], TRUE);
  END P26;
    
(*------------------------------------ TRY EXCEPT <use temp> inside a FOR ---*)

PROCEDURE P27 () =
  <*FATAL Wr.Failure, Thread.Alerted*>

    PROCEDURE Q (VAR i: INTEGER) =
      BEGIN
        EVAL i;
      END Q;

  VAR
    a     := 5;
    b     := 7;
    total := 0;
    wr    := TextWr.New ();
    r     := NEW (REF RECORD a, b: INTEGER; END);
  BEGIN
    FOR i := 1 TO a + b DO
      INC (total);
      TRY RAISE E;
      EXCEPT E =>
        Wr.PutText (wr, "Hello world");
        Q (r^.b);
      END;
      Test.check (total <= 12);
    END;
    Test.checkI (total, 12);
  END P27;

(*---------------------------------- TRY ... RETURN e FINALLY <use temps> ---*)

PROCEDURE P28 () =
  BEGIN
    Test.checkI (P28A (), 987);
  END P28;

PROCEDURE P28A (): INTEGER =
  <*FATAL Wr.Failure, Thread.Alerted*>

    PROCEDURE Q (VAR i: INTEGER) =
      BEGIN
        EVAL i;
      END Q;
  
  VAR
    r := NEW (REF RECORD a, b: INTEGER; END);
    wr := TextWr.New ();
  BEGIN
    TRY
      RETURN 987;
    FINALLY
      Wr.PutText (wr, "Hello world");
      Q (r^.b);
    END;
  END P28A;


(*---------------------------------------------------------------------------*)
BEGIN
    P1 ();
    P2 ();
    P3 ();
    P4 ();
    P5 ();
  (*  P7 (); *)
    P9 ();
    P12 ();
    P13 ();
    P14 ();
    P15 ();
    P16 ();
    P17 ();
    P18 ();
    P19 ();
 (*   P20 (); *)
 (*   P21 (); *)
    P22 ();
    P23 ();
    P24 ();
    P25 ();
    P26 ();
    P27 ();
    P28 ();
    Test.done ();
END Main.

