(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE Value;

(* A Value.T is a representation for a Modula-3 value; it is intended 
   to be used in conjunction with the typed Abstract Syntax Tree 
   interface, TypedAST.  A Value.T only contains enough information 
   to determine the value if you already know the value's type; 
   therefore Value.T's should generally be paired with Type.T's. 
   
   For good measue, this interface also defines types to represent 
   procedures and exceptions. *)

TYPE 
  T <: ROOT;
  (* Ordinal | Float | LongFloat | Extended | Array | Set | Record | 
     Text | Null *)

    Ordinal = T OBJECT ord: INTEGER END;
    (* ORD(the value) *)

    Float = T OBJECT val: REAL END;

    LongFloat = T OBJECT val: LONGREAL END;

    Extended  = T OBJECT val: EXTENDED END;

    Array = T OBJECT elements: REF ARRAY OF T END;

    Set = T OBJECT elements: REF ARRAY OF BOOLEAN END;

    Record = T OBJECT elements: REF ARRAY OF T END;
    (* The field values in the order the fields are declared. *)

    Txt = T OBJECT val: TEXT END;

    Null = T OBJECT END;
    (* The value NIL. *)

END Value.

