(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 

(* test of builtin ZERO function *)

UNSAFE MODULE Main;
IMPORT Test, RTMisc;

VAR a: ARRAY [0..5000] OF CHAR;

PROCEDURE Reset() =
  BEGIN
    FOR i := FIRST (a) TO LAST (a) DO a[ i ] := 'a'; END;
  END Reset;


PROCEDURE Check ( j, n: INTEGER ) =
  BEGIN
    FOR i := 0     TO j - 1     DO Test.checkC (a[ i ], 'a' )    END;
    FOR i := j     TO j + n - 1 DO Test.checkC (a[ i ], '\000' ) END;
    FOR i := j + n TO LAST (a)  DO Test.checkC (a[ i ], 'a' )    END;
  END Check;

BEGIN
  Reset();
  RTMisc.Zero( ADR( a ), NUMBER( a ) );
  Check( 0, NUMBER( a ) );

  Reset();
  RTMisc.Zero( ADR( a[ 10 ] ), 0 );
  Check( 0, 0 );    
 
  Reset();
  RTMisc.Zero( ADR( a[ 4 ] ), 3 );
  Check( 4, 3 );  
    
  Reset();
  RTMisc.Zero( ADR( a[ 5 ] ), 3 );
  Check( 5, 3 );  
    
  Reset();
  RTMisc.Zero( ADR( a[ 5 ] ), 8 );
  Check( 5, 8 );  
    
  Reset();
  RTMisc.Zero( ADR( a[ 4 ] ), 8 );
  Check( 4, 8 );  
    
  Reset();
  RTMisc.Zero( ADR( a[ 4 ] ), 256 );
  Check( 4, 256 );    

  Reset();
  RTMisc.Zero( ADR( a[ 7 ] ), 4000 );
  Check( 7, 4000 );    
 
  Reset();
  RTMisc.Zero( ADR( a[ 8 ] ), 4000 );
  Check( 8, 4000 );   
    
  FOR i := FIRST (a) TO LAST(a) BY 101 DO
    FOR n := FIRST (a) TO LAST (a) - i BY 101 DO
      Reset();
      RTMisc.Zero( ADR( a[ i ] ), n );
      Check( i, n );
    END;
  END;
 
  Test.done ();
END Main.

