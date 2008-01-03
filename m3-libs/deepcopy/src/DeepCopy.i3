INTERFACE DeepCopy;

TYPE 
  Refs = ARRAY OF REFANY;
  Special = OBJECT
  METHODS
    copy(from: REFANY): REFANY;
  END;

PROCEDURE Copy(from: REFANY; READONLY dontcopy := Refs{}): REFANY;

(* 
  Makes an copy of the 'from' reference and copies any traced objects
  that are pointed to. Untraced objects are not copied. Any objects that
  are pointed to in the 'dontcopy' array are not copied or followed.
*)


PROCEDURE RegisterSpecial(tc: CARDINAL; copier: Special);

(*
  For various reasons, you may want to handle certain objects in
  special ways. These can be registered by calling this function with the 
  typecode of the object and a Special object that does the work. Re-registering
  overrides the existing handler. A handler for Atom.T is built-in.
*)


END DeepCopy.
