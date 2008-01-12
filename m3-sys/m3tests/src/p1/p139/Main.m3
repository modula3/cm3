(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
UNSAFE MODULE Main;
IMPORT Test, RTMisc, Wr;
FROM Stdio IMPORT stdout;

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
    FOR i := 0     TO j - 1     DO
      Test.checkC (a[i], 'a' )
    END;
    FOR i := j     TO j + n - 1 DO
      Test.checkC (a[i], 'b' )
    END;
    FOR i := j + n TO LAST( a ) DO
     Test.checkC (a[i], 'a' )
    END;
  END Check;

PROCEDURE m( s: TEXT ) =
  BEGIN
    TRY
      Wr.PutText (stdout, s & "\n");
      Wr.Flush (stdout);
    EXCEPT ELSE END;
  END m;

BEGIN
  m ("RTMisc.Copy 1:");
  Reset();
  RTMisc.Copy ( ADR( b[ 0 ] ), ADR( a[ 0 ] ), LAST( a ) );
  Check( 0, LAST( a ) );

  m ("SUBARRAY Copy 1:");
  Reset();
  SUBARRAY (a, 0, LAST(a)) := SUBARRAY (b, 0, LAST(a));
  Check( 0, LAST( a ) );

  m ("RTMisc.Copy 2:");
  Reset();
  RTMisc.Copy ( ADR( b[ 0 ] ), ADR( a[ 10 ] ), 20 );
  Check( 10, 20 );

  m ("SUBARRAY Copy 2:");
  Reset();
  SUBARRAY (a, 10, 20) := SUBARRAY (b, 0, 20);
  Check( 10, 20 );
    
  m ("RTMisc.Copy 3:");
  Reset();
  RTMisc.Copy ( ADR( b[ 0 ] ), ADR( a[ 10 ] ), 20 );
  RTMisc.Copy ( ADR( a[ 5 ] ), ADR( a[ 10 ] ), 10 );
  Check (15, 15);

  m ("SUBARRAY Copy 3:");
  Reset();
  SUBARRAY (a, 10, 20) := SUBARRAY (b, 0, 20);
  SUBARRAY (a, 10, 10) := SUBARRAY (a, 5, 10);
  Check( 15, 15 );

  Test.done ();    
END Main.
