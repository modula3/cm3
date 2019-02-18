(* $Id$ *)
INTERFACE LongrealSort;
IMPORT LongrealType;

(* a longreal sort that works with NaNs *)

(* sorting mode for NaNs:
   Smallest :  FIRST(LONGREAL)
   Zero :      0.0d0
   Largest :   LAST(LONGREAL)
   DontTouch : Use standard code 
 *)

TYPE Mode = { Smallest, Largest, DontTouch };

TYPE Compar = PROCEDURE(a, b : LONGREAL) : [-1..1];

PROCEDURE Sort(VAR a: ARRAY OF LONGREAL; 
               cmp : Compar := LongrealType.Compare;
               treatNanAs := Mode.Largest);

END LongrealSort.


