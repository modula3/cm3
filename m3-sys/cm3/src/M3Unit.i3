(* Copyright 1996-2000 Critical Mass, Inc. All rights reserved.    *)
(* See file COPYRIGHT-CMASS for details. *)

INTERFACE M3Unit;

IMPORT IntRefTbl, M3ID, M3Loc, M3Path, Mx;

TYPE
  Kind = M3Path.Kind;

TYPE
  T = REF RECORD
    next         : T       := NIL;
    next_synonym : T       := NIL;  (* synonyms in the map *)
    name         : M3ID.T  := M3ID.NoID;
    loc          : M3Loc.T := NIL;
    library      : T       := NIL;  (* containing library unit *)
    
    (* object is what cm3 will produce,
     * including possible invocation of an external
     * backend such as m3cg.
     *
     * When cm3 -boot is not specified, it stops "late"
     * at .o/.mo/.io/_m.o/_i.o. i.e. all that remains
     * is making a library or linking.
     *
     * When cm3 -boot is specified, it stops "early"
     * at .ms/.is/.s or .c/.cpp. i.e. the next phase
     * of bootstrapping -- running an assembler or C compiler -- remains.
     *)
    object       : TEXT    := NIL;
    
    (* final_object is similar to object.
     * When cm3 -boot is not given, final_object is not used.
     * When cm3 -boot is used, final_object is what object
     * would have been. That is, these are the files written
     * into the makefiles that cm3 -boot produces.
     *)
    boot_makefile_object : TEXT    := NIL;
    link_info    : Mx.Unit := NIL;
    exporters    : Exporter:= NIL;
    kind         : Kind    := Kind.Unknown;
    hidden       : BOOLEAN := FALSE;
    imported     : BOOLEAN := FALSE;
    compiling    : BOOLEAN := FALSE;
    stale_src    : BOOLEAN := FALSE;
    missing_info : BOOLEAN := FALSE;
    shared       : BOOLEAN := FALSE;
    debug        : BOOLEAN := FALSE;
    optimize     : BOOLEAN := FALSE;
    (* for determining the compilation order *)
    low_link     : INTEGER := 0;
    class        : INTEGER := 0;
  END;

TYPE
  Exporter = REF RECORD
    next     : Exporter := NIL;
    name     : M3ID.T   := M3ID.NoID;
    unit     : T        := NIL;
    used     : BOOLEAN  := FALSE;
    verified : BOOLEAN  := FALSE;
  END;

TYPE
  TList = REF RECORD
    head : T;
    tail : TList;
  END;

TYPE
  Set = RECORD
    map  : IntRefTbl.T := NIL;
    head : T := NIL;
    tail : T := NIL;
  END;

PROCEDURE New (nm: M3ID.T;  k: Kind;  loc: M3Loc.T;  hidden, imported: BOOLEAN): T;

PROCEDURE InitSet (VAR x: Set);

PROCEDURE Add (VAR x: Set;  t: T);

PROCEDURE AddNew (VAR x: Set;  nm: M3ID.T;  k: Kind;  loc: M3Loc.T;
                  hidden, imported: BOOLEAN);
(* == Add (x, New (nm, k, loc, hidden, imported)) *)

PROCEDURE Get (READONLY x: Set;  nm: M3ID.T;  k: Kind): T;
(* Returns the unit in "x" with matching name and kind.  Otherwise, "NIL". *)

PROCEDURE FileName (t: T): TEXT;
(* Returns "t.name" with its proper host prefix and extension attached *)

PROCEDURE FullPath (t: T): TEXT;
(* Returns a full path to "t" on the host. *)

END M3Unit.
