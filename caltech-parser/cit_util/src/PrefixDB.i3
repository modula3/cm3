INTERFACE PrefixDB;

TYPE
  T <: Public;
  Public = OBJECT METHODS
    apply(to: TEXT): TEXT;
  END;

PROCEDURE FromFile(named: TEXT): T;
(* file format:
   #comment
   regex1 .. regExN prefix
*)

END PrefixDB.
