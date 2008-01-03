(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;
IMPORT Test;

VAR 
    x: ARRAY [0..2] OF CHAR;
    y: ARRAY [0..3] OF INTEGER;
    z: INTEGER;

PROCEDURE P (<*NOWARN*> x1: ARRAY OF CHAR; y1: ARRAY OF INTEGER; z1: INTEGER )=
  BEGIN
    Test.checkI ( z1, z );
    FOR i := FIRST(x1) TO LAST( x1 ) DO
      Test.checkC ( x1[ i ], x[ i ] );
      x1[ i ] := '\000';
    END;
    FOR i := FIRST(y1) TO LAST( y1 ) DO
      Test.checkI ( y1[ i ], y[ i ] );
      y1[ i ] := 0;
    END;
    Q();
  END P;
    
PROCEDURE Q() =
  BEGIN 
  END Q;
    
BEGIN
    z := 10;
    x[ 0 ] := 'a';
    x[ 1 ] := 'b';
    x[ 2 ] := 'c';
    y[ 0 ] := 100;
    y[ 1 ] := 101;
    y[ 2 ] := 102;
    y[ 3 ] := 103;
    P( x, y, z );
    Test.checkI ( z, 10 );
    Test.checkC ( x[ 0 ], 'a' );    
    Test.checkC ( x[ 1 ], 'b' );    
    Test.checkC ( x[ 2 ], 'c' );    
    Test.checkI ( y[ 0 ], 100 );    
    Test.checkI ( y[ 1 ], 101 );    
    Test.checkI ( y[ 2 ], 102 );    
    Test.checkI ( y[ 3 ], 103 );
    Test.done ();
END Main.
