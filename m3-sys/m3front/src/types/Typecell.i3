(* Typecell -- a backend-neutral description of the runtime typecell of one REF or
   OBJECT type: the data parts of RT0.Typecell and of its ArrayTypecell and
   ObjectTypecell extensions.  RefType.Describe and ObjectType.Describe build
   it; the CG path (InitTypecell) and the MSIR path (InitTypecellMSIR) both
   write it out, so the two cannot disagree on what a cell holds.

   Not described: the code parts (TC_initProc, OTC_linkProc,
   OTC_defaultMethods), which each path generates in its own form; and the
   fields RTLinker computes (TC_typecode, OTC_dataOffset,
   OTC_methodOffset, ...) or that chain a unit's cells (TC_next).

   Sizes and alignments are in bytes. *)

INTERFACE Typecell;

IMPORT Brand, M3FP, M3RT, TipeMap;

TYPE
  Bytes = TipeMap.Bytes;  (* NIL: the cell has none *)

  T = RECORD
    uid        : INTEGER;         (* TC_selfID *)
    fp         : M3FP.T;          (* TC_fp *)
    traced     : BOOLEAN;         (* TC_traced *)
    kind       : M3RT.TypeKind;   (* TC_kind: Ref, Array or Obj *)
    dataAlign  : INTEGER;         (* TC_dataAlignment *)
    dataSize   : INTEGER;         (* TC_dataSize: the referent, the open-array
                                     dope, or the object's own fields *)
    typeMap    : Bytes;           (* TC_type_map (RTTypeMap, pickle ver1) *)
    gcMap      : Bytes;           (* TC_gc_map (RTHeapMap) *)
    typeDesc   : Bytes;           (* TC_type_desc (RTTipe, pickle ver2);
                                     traced types only *)
    brand      : Brand.T;         (* TC_brand; NIL if unbranded *)
    name       : TEXT;            (* TC_name; NIL if anonymous *)
    nDims      : INTEGER := 0;    (* Array: ATC_nDimensions *)
    elemSize   : INTEGER := 0;    (* Array: ATC_elementSize *)
    parentUID  : INTEGER := 0;    (* Obj: OTC_parentID; 0 if none *)
    methodSize : INTEGER := 0;    (* Obj: OTC_methodSize, own methods *)
  END;

END Typecell.
