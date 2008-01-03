(* Copyright (C) 1995, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Thu Jan 19 13:57:52 PST 1995 by kalsow     *)
(*      modified on Tue Sep 27 18:30:00 PDT 1994 by weich      *)

(* The example program declares a simple stable object and allows
   the user to drive state modifications and stability from the keyboard.

   \subsection{Object Interface}
   The stable object is called "Fib" and generates a sequence of Fibonacci
   numbers.  
*)
INTERFACE Fib;

TYPE T <: Public;
     Public = OBJECT
     METHODS 

(* "Fib" has three initialization methods: "init" is the standard,
    non-stable initializer; "initS" initializes the stable state; and
   "initT" initializes the transient state.
*)
       init(newFirst : INTEGER; newSecond : INTEGER) : T;
       initS(newFirst : INTEGER; newSecond : INTEGER);
       initT();

(* The other two methods modify
   and examine the state of the object: "crank" generates a new
   Fibonacci number.  "count" returns the number of times that the object
   has been cranked during the current invocation of the program.
*)
       crank(times : INTEGER) : INTEGER;
       count() : INTEGER;
     END;

(* Here is the "STABLE" pragma that the stub generator needs: *)
     <*PRAGMA STABLE *><*STABLE UPDATE METHODS initS, crank *>
(* "initS" is the only initialization function that "stablegen" needs
   to see.  "init" is used for the unstable version of the object, 
   and "initT" does not modify the stable state.
*)

(* The "Brand" is a convention used by all generic interfaces, including
   stable objects: *)
CONST Brand = "Fib";

END Fib.
