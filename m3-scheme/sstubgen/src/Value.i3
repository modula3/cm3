(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE Value;
IMPORT Atom;

(* A Value.T is a representation for a Modula-3 value; it is intended 
   to be used in conjunction with the typed Abstract Syntax Tree 
   interface, TypedAST.  A Value.T only contains enough information 
   to determine the value if you already know the value's type; 
   therefore Value.T's should generally be paired with Type.T's. 
   
   For good measue, this interface also defines types to represent 
   procedures and exceptions. *)

TYPE 
  T <: ROOT;
  (* Ordinal | Longint | Float | LongFloat | Extended | ArrayOrRecord | Set | 
     Text | Null *)

    Ordinal = T OBJECT ord: INTEGER END;
    (* ORD(the value) *)

    Longint = T OBJECT val: REFANY (* REF LONGINT *) END;

    Float = T OBJECT val: REAL END;

    LongFloat = T OBJECT val: LONGREAL END;

    Extended  = T OBJECT val: EXTENDED END;

    ArrayOrRecord = T OBJECT elements: REF ARRAY OF Element END;
    (* for array, elements can be either a normal T or a Propagate. 
       for record, he field values in the order the fields are declared. *)
       
    Set = T OBJECT elements: REF ARRAY OF Ordinal END;
    (* the ordinals corresponding to the base type of the set.
       note that this declaration has changed from what's in the SRC
       code, because 
       (1) AstToValue doesn't have enough information to generate the 
           old format
       (2) The old format can grow exponentially with the program text
           (TYPE S = SET OF [0..1024*1024]), whereas this version only 
           grows linearly with the program text.
    *)


    Txt = T OBJECT val: TEXT END;

    Null = T OBJECT END;
    (* The value NIL. *)

    Proc = T OBJECT intf, item : Atom.T END;

    (************************************************************)

    (* special things for ArrayOrRecord *)
    Element = BRANDED OBJECT END;

    Propagate = Element BRANDED OBJECT END; (* the last init. is propagated *)

    Range = Element OBJECT val : T END;

    Actual = Range OBJECT field : Atom.T END; 
    (* really not quite right, an Actual can also be a TYPE, but
       not in this context *)
    
END Value.

