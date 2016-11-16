(* Copyright (C) 2016 Peter McKinna. All rights reserved. *)
(* See file COPYRIGHT-BSD for details. *)

INTERFACE FloatUtils;

IMPORT Word;
IMPORT DragonInt AS DI;

CONST 
  Bias = 1023;
  
TYPE
  Int32 = [-16_7FFFFFFF-1 .. 16_7FFFFFFF];

(* return the lowest 32 bit word of a longreal *)  
PROCEDURE Word0(r : LONGREAL) : Int32;

(* set the lowest 32 bit word of a longreal *)
PROCEDURE Word0Set(r : LONGREAL; r0 : Int32) : LONGREAL;

(* checks if all bits are set in a bigint *)
PROCEDURE allOn(s : DI.Session; b : DI.T; n : INTEGER) : BOOLEAN;

(* checks if any bits are set in a bigint *)
PROCEDURE anyOn(s : DI.Session; b : DI.T; n : INTEGER) : BOOLEAN;

(* set all bits of a bigint to 1 *)
PROCEDURE setOnes(s : DI.Session; b : DI.T; n : INTEGER) : DI.T;

(* removes the lowest zero bits ie shifts all the zeros right
and returns the number shifted and the result of the shift *)
PROCEDURE lo0bits(VAR y : Word.T) : INTEGER;

(* removes the highest zero bits ie shifts all the zeros left
and returns the number shifted and the result of the shift *)
PROCEDURE hi0bits(x : Word.T) : INTEGER;

(* returns the mantissa bits *)
PROCEDURE mantbits(d : LONGREAL) : INTEGER;

(* bigint diff ensuring a > b swapping if needed *)
PROCEDURE diff(s : DI.Session; a,b : DI.T; VAR dsign : [-1..1]) : DI.T;

(* multiply a by the kth power of 5 *)
PROCEDURE pow5mult(s : DI.Session; a : DI.T; k : INTEGER) : DI.T;

(* returns a longreal as a bigint *)
PROCEDURE d2b(s : DI.Session; l : LONGREAL; VAR exp,bits : INTEGER) : DI.T;

(* returns a string as a bigint *)
PROCEDURE s2b(s : DI.Session; READONLY a : ARRAY OF CHAR; nd0,nd : INTEGER; y9 : INTEGER) : DI.T;

(* returns ratio of a to b *)
PROCEDURE ratio(s : DI.Session; a,b : DI.T) : LONGREAL;

(* returns number of trailing zeros of a bigint *)
PROCEDURE trailz(s : DI.Session; b : DI.T) : INTEGER;

(* return the sign-extended bottom 32 bits of 'x' *)
PROCEDURE Fix32 (x: Word.T): Int32;

END FloatUtils.
