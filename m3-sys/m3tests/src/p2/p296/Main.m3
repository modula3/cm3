(* ----------------------------------------------------------------------1- *)
(* File Main.m3 for Modula3 compiler test p296                              *)
(* Copyright 2026, Rodney M. Bates.                                         *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)

(* Test storing fetching and storing of array elements typed BITS n FOR...,
   Where n is not a valid alignment.  In this case, elements of 5 bits.
   NOt that this means the entire array must fit in a native word, otherwise
   a field would straddle a word boundary, evoking a compile error.
*) 

MODULE Main 
  EXPORTS Main;
  IMPORT Fmt;
  IMPORT Stdio;
  IMPORT Thread;
  IMPORT Wr; 
TYPE
  Rng = [0 .. 31];
  ArrInner = ARRAY [0 .. 3] OF BITS 5 FOR Rng;
  ArrOuter = ARRAY [1 .. 3] OF ArrInner; 

CONST A1C = ArrInner{17, 0, 19, 0};
VAR A1V := A1C;
CONST A2C = ArrInner{0, 21, 0, 25};
VAR A2V := A2C;
CONST A3C = ArrInner{17, 19, 21, 25};
VAR A3V := A3C;

CONST Const = ArrOuter { A1C, A2C, A3C};

VAR Copy : ArrOuter ;

PROCEDURE V () : ArrOuter =
  VAR Result := ArrOuter {A1V , A2V , A3V}; (*<------- problem here*)
BEGIN
  RETURN Result;
END V;

PROCEDURE W ( VAR Arr : ArrOuter )
= BEGIN
    Arr [ 1 , 0 ] := 25; 
    Arr [ 1 , 1 ] := 0;
    Arr [ 1 , 2 ] := 19;
    Arr [ 1 , 3 ] := 0;
    Arr [ 2 , 0 ] := 17;
    Arr [ 2 , 1 ] := 0;
    Arr [ 2 , 2 ] := 21;
    Arr [ 2 , 3 ] := 0;
    Arr [ 3 , 0 ] := 25;
    Arr [ 3 , 1 ] := 21;
    Arr [ 3 , 2 ] := 19;
    Arr [ 3 , 3 ] := 17;
  END W;

VAR GOut : ArrInner ;

PROCEDURE Line ( Value : TEXT )
= <* FATAL Thread.Alerted , Wr . Failure *>
  BEGIN
    Wr . PutText ( Stdio . stdout , Value);  
    Wr . PutText ( Stdio . stdout , Wr . EOL ); 
  END Line; 

PROCEDURE Dump  ( READONLY Value : ArrOuter )
= VAR Out : ArrInner ;
  VAR In , Both : INTEGER;
  <* FATAL Thread.Alerted , Wr . Failure *>
  BEGIN (*Dump *)
   FOR R0 := FIRST ( ArrOuter ) TO LAST ( ArrOuter )
   DO
     FOR R1 := FIRST ( ArrInner ) TO LAST ( ArrInner )
     DO
       Out := Value [ R0 ];
       GOut := Out; 
       In := Out [ R1 ];
       Both := Value [ R0 ] [ R1 ]; 
       Wr . PutText
         ( Stdio . stdout
         , Fmt . Pad ( Fmt . Int ( Value [ R0 ] [ R1 ] ) , 4 ) 
         ) ;
     END ;
     Wr . PutText ( Stdio . stdout , Wr . EOL ) 
   END 
  END Dump;
      
BEGIN
  Line ( "Constant constructor:" );
  Dump ( Const ); 
  Copy := V ( );
  Line ( "Copied variable:" );
  Dump ( Copy );
  W ( Copy ); 
  Line ( "Variable with altered elements:" );
  Dump ( Copy );
  Line ( "Done." ); 
END Main 
.

