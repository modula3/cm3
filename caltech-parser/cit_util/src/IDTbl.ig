GENERIC INTERFACE IDTbl(Elem, IntElemTbl);
IMPORT IDGen;

(* This interface associates numerical IDs to objects so they can
   be conveniently referred to, like in "gdb".

   In addition to all the standard table operations,
   there is the "autoPut" method, which inserts an element with an
   automatically-created new ID.

   To list the entries in the order they were created and/or another order,
   this interface can be combined with
   "QueueTbl.ig" and/or "SortedTable.ig", as desired.
*)

TYPE
  T <: Public;
  Public = IntElemTbl.Default OBJECT
  METHODS
    setIDGen(idGen: IDGen.T);
    (* "setIDGen" changes the ID generator.
       By default (i.e., after "init" or whatever
       is used for "IntElemTbl.Default" initialization),
       the ID generator is "NEW(IDGen.Low).init()". *)

    autoPut(READONLY value: Elem.T): INTEGER;
    (* returns key that was used in automatic insertion *)
  END;
  Default = T;
  Iterator = IntElemTbl.Iterator;

END IDTbl.
