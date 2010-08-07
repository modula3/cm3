(*---------------------------------------------------------------------------*)
INTERFACE PkgBase;

IMPORT TextList, TextTextTbl;
IMPORT Rd, Pathname;
IMPORT PkgError, FileInfo, MsgIF;

(*---------------------------------------------------------------------------*)
TYPE
  Kind   = TEXT; (* the symbolic name of package kinds *)
  Name   = TEXT; (* the names of packages *)
  Action = TEXT; (* symbolic names for actions associated with packages *)
  CmdSeq = TEXT;

  T <: Public;
  (*
    A PkgBase.T is a configuration object that contains information
    about kinds of packages and associated actions. There may be any number
    of package kinds, and for each package kind any number of mappings
    from symbolic action names to command sequences.

    A PkgBase.T object is always created empty and then initialized
    from a stream which must conform to the following syntax:

|     <stream>       ::= <pkgtypedef>*
|     <pkgtypedef>   ::= pkgkind <pkgkind> has <idpred> <pkgactiondef>*
|     <pkgkind>      ::= <token>
|     <idpred>       ::= <expr1> 
|                     |  <expr1> and <idpred>
|     <expr1>        ::= dir <token>
|                     |  file <token>
|                     |  match <token>
|                     |  hosttype <token>
|                     |  ostype <token>
|                     |  platform <token>
|                     |  notdir <token>
|                     |  notfile <token>
|                     |  nomatch <token>
|     <pkgactiondef> ::= action <actionname> <commands>
|     <actionname>   ::= <token>
|     <commands>     ::= <stringortoken>

    Every character after a `#' that is not contained in a string 
    up to the end of line is considered to be a comment and discarded.

    Environment variables may be referenced with $NAME within commands,
    they are expanded at initialization time.

  *)

  Public = OBJECT
  METHODS
    (*-----------------------------------------------------------------------*)
    oldInit(hosttype, ostype : TEXT; fc : FileInfo.T := NIL; 
            msgif : MsgIF.T := NIL) : T;
    (* Return an empty object. If `fc' is non-NIL, it will be used to
       speed up disk lookup operations. 
       *** DEPRECATED *** This is an old constructor and will vanish
       in future versions.
    *)

    (*-----------------------------------------------------------------------*)
    init(env : TextTextTbl.T; fc : FileInfo.T := NIL; 
         msgif : MsgIF.T := NIL) : T;
    (* Return an empty object. If `fc' is non-NIL, it will be used to
       speed up disk lookup operations. 
       The table `env' will be used to lookup definitions of tpc-hosttype,
       tpc-ostype, and platform-suffix.
    *)

    (*-----------------------------------------------------------------------*)
    setCache(fc : FileInfo.T);
    (* Use the cache `fc' for all following disk lookup operations. *)

    (*-----------------------------------------------------------------------*)
    addDefs(rd : Rd.T) : BOOLEAN;
    (* Add definitions from stream `rd'. Return TRUE if no syntax errors
       were found, else FALSE. *)

    (*-----------------------------------------------------------------------*)
    kindDefined(k : Kind) : BOOLEAN;
    (* <=> There is a definition for `k'. *)

    (*-----------------------------------------------------------------------*)
    kindList() : TextList.T;
    (* Return the sorted list of all defined kinds. *)

    (*-----------------------------------------------------------------------*)
    getAction(k : Kind; a : Action) : CmdSeq;
    (* Return the command sequence for package kind `k' and action `a'. *)

    (*-----------------------------------------------------------------------*)
    isKind(p : Pathname.T; k : Kind) : BOOLEAN;
    (* <=> The package located at `p' is of kind `k' according to the
           known identification predicates. *)

    (*-----------------------------------------------------------------------*)
    kindFound(p : Pathname.T; VAR k : Kind) : BOOLEAN;
    (* This function procedure tries to identify a package at path `p'
       according to all known identification predicates. If `k' is not NIL,
       its value is treated as the `preferred package kind' and is tested
       first. 
    *)

    (*-----------------------------------------------------------------------*)
    createEmptyPkg(p : Pathname.T; k : Kind) RAISES {PkgError.E};
    (* Create an empty package of kind `k'; that is, all directories and
       files that are necessary to make it look like kind `k' at
       path `p'. *)

    (*-----------------------------------------------------------------------*)
    ensurePkgExists(p : Pathname.T; k : Kind) RAISES {PkgError.E};
    (* Ensure that a package structure of kind `k', that is, all 
       directories and files that are necessary to make it look like
       kind `k', exists at path `p'. *)

  END;


END PkgBase.
