(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)

(* An "AtomicItemTbl.T" is a subtype of "ItemTbl.T" that serializes
   concurrent accesses by locking. It also allocates new IDs to
   newly created objects. 
*)

INTERFACE AtomicItemTbl;

IMPORT ItemTbl;

TYPE T = MUTEX OBJECT
       state: State;
     END;

    State = REF RECORD 
      tbl: ItemTbl.Default;
      id: INTEGER;
    END;

(* The object must be locked before accessing its "state". 
   Note that "state" is what gets stored on disk.
   "id" is used to generate successively larger IDs for items created.
*)

END AtomicItemTbl.
