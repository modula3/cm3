INTERFACE M3CG_Tee;

IMPORT M3CG;

PROCEDURE New (child1, child2: M3CG.T): M3CG.T;
(* A new code generator that forwards each operation
   to both child1 and child2. *)

END M3CG_Tee.