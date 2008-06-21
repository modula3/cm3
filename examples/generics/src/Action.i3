
INTERFACE Action;

(* "Action.T", denoting actions for commands,
    is just a procedure with no parameters
    and no results. *)

TYPE
  T = PROCEDURE();

(* "Action.Brand" is used by run-time to
   create composite brands for generics made out 
   of "Action"s. *)

CONST
  Brand =  "Action";

END Action.

