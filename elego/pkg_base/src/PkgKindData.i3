(*---------------------------------------------------------------------------*)
INTERFACE PkgKindData;

IMPORT PkgError, FileInfo, MsgIF;

(*---------------------------------------------------------------------------*)
CONST Brand = "PkgKindData interface 0.0";

(*---------------------------------------------------------------------------*)
TYPE
  T <: Public;
  (* 
     PkgKindData consists of a table that maps symbolic names of
     actions to names, a name of this kind of package, and 
     of a simple kind of predicate that can be evaluated to ascertain
     that at a given root exists this kind of package structure. 

     The predicate consists of a series of conditions that must all
     be true. Only possible conditions are the existence or non-existence
     of a file of a directory. 
  *)

  PredKind = {Dir, File, Match, NoDir, NoFile, NoMatch, Platform,
              HostType, OSType};

  Public = OBJECT
  METHODS
    init(fc : FileInfo.T := NIL; msgif : MsgIF.T := NIL) : T;
    init2(n : TEXT; fc : FileInfo.T := NIL; msgif : MsgIF.T := NIL) : T;
    setCache(fc : FileInfo.T);
    setName(n : TEXT);
    name() : TEXT;
    putAction(name, cmds : TEXT) : BOOLEAN;
    getAction(name : TEXT; VAR cmds : TEXT) : BOOLEAN;
    addCondition(p : PredKind; arg : TEXT);
    evalCondition(path : TEXT;
                  hosttype := "unknown"; 
                  ostype   := "unknown") : BOOLEAN;
    createStructure(path : TEXT) RAISES {PkgError.E};
    ensureStructureExists(path : TEXT) RAISES {PkgError.E};
  END;

END PkgKindData.
