(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
UNSAFE MODULE Main;
IMPORT Test, RTMisc;

VAR
    a: ARRAY [0..100] OF CHAR;
    b: ARRAY [0..100] OF CHAR;

PROCEDURE Reset() =
  BEGIN
    FOR i := 0 TO LAST( a ) DO a[ i ] := 'a'; END;
    FOR i := 0 TO LAST( b ) DO b[ i ] := 'b'; END;
  END Reset;

PROCEDURE Check( j, n: INTEGER ) =
  BEGIN
    FOR i := 0     TO j - 1     DO  Test.checkC (a[i], 'a' )  END;
    FOR i := j     TO j + n - 1 DO  Test.checkC (a[i], 'b' )  END;
    FOR i := j + n TO LAST( a ) DO  Test.checkC (a[i], 'a' )  END;
  END Check;

BEGIN
  Reset();
  RTMisc.Copy ( ADR( b[ 0 ] ), ADR( a[ 0 ] ), LAST( a ) );
  Check( 0, LAST( a ) );

  Reset();
  SUBARRAY (a, 0, LAST(a)) := SUBARRAY (b, 0, LAST(a));
  Check( 0, LAST( a ) );

  Reset();
  RTMisc.Copy ( ADR( b[ 0 ] ), ADR( a[ 10 ] ), 20 );
  Check( 10, 20 );

  Reset();
  SUBARRAY (a, 10, 20) := SUBARRAY (b, 0, 20);
  Check( 10, 20 );
    
  Reset();
  RTMisc.Copy ( ADR( b[ 0 ] ), ADR( a[ 10 ] ), 20 );
  RTMisc.Copy ( ADR( a[ 5 ] ), ADR( a[ 10 ] ), 10 );
  Check (20, 10); (* not Check(15, 15) since RTMisc.Copy doesn't use memcpy *)

  Reset();
  SUBARRAY (a, 10, 20) := SUBARRAY (b, 0, 20);
  SUBARRAY (a, 10, 10) := SUBARRAY (a, 5, 10);
  Check( 15, 15 );

  Test.done ();    
END Main.
