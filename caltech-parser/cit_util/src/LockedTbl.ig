(* $Id$ *)

GENERIC INTERFACE LockedTbl(KeyValueTbl);

(* provide LOCKing on any given generically generated table, from the
   standard Table generic. *)

(* 
   N.B. iterate() is very slow, because it copies the table!
*)

TYPE
  T = KeyValueTbl.T; (* hmm... why not? *)
  
  Default <: PubDefault;

  PubDefault = KeyValueTbl.Default OBJECT METHODS
    copy() : KeyValueTbl.Default; (* note copy not locked *)
  END;

  Iterator = KeyValueTbl.Iterator;

CONST Brand = "Locked " & KeyValueTbl.Brand;

END LockedTbl.
