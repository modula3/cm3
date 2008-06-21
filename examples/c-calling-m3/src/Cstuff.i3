
INTERFACE Cstuff;

(* A new procedure type to be passed as a parameter to C functions. *)
TYPE
  IntProc = PROCEDURE (): INTEGER;

(* We still have to declare the signature for the C functions. *)
<*EXTERNAL*> 
PROCEDURE add_one (p: IntProc): INTEGER;
     (* Returns "1 + p()". *)

<*EXTERNAL*>
PROCEDURE add_one_again (): INTEGER;
	(* Returns "1 + m3_proc()". *)

<*EXTERNAL*>
VAR m3_proc: IntProc;
  
END Cstuff.
