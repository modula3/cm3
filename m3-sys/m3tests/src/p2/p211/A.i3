(*
 This program passes floats and doubles from Modula-3 to C code to check
 that they have the correct value. Strings are used as a presumed working
 transport.
*)

INTERFACE A;

<*EXTERNAL*> PROCEDURE CheckF(Float : REAL; Text : TEXT);
<*EXTERNAL*> PROCEDURE CheckD(Float : LONGREAL; Text : TEXT);

END A.
