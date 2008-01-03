INTERFACE SharedObjRep;

IMPORT SharedObj, WeakRefList;

TYPE
 Byte8 = BITS 8 FOR [0..255];
 WireRep = RECORD byte: ARRAY [0..15] OF Byte8; END;

CONST NullT = WireRep {byte := ARRAY [0..15] OF Byte8{0, ..}};

(* The wire representation of our objects is represented using the
   WireRep provided by the event package. *)

REVEAL
  SharedObj.T = SharedObj.Public BRANDED SharedObj.Brand OBJECT 
    (* uncommenting the initializer will crash stubgen *)
    wrep  : WireRep (* := NullT *);

    (* this line, with or without the initilizer, will crash stubgen *)
    callbacks: WeakRefList.T := NIL;
  END;

END SharedObjRep.
