(*---------------------------------------------------------------------------*)
INTERFACE Checkpoint;

IMPORT Pathname, Fingerprint, TextSeq, Rd;
IMPORT FileInfo, MsgIF, FindExpr;

EXCEPTION Error(TEXT);

(*---------------------------------------------------------------------------*)
TYPE
  Attr = {
    Changed,   (* different fingerprint *)
    Modified,  (* known to be locally modified *)
    Unmodified,(* known to be not locally modified *)
    UpToDate,  (* known to be up-to-date recently*)
    OutOfDate, (* known to be not up-to-date recently*)
    Conflicts, (* known to have conflicts recently*)
    NoConflicts,(* known to have conflicts recently*)
    DepMade,   (* dependencies known to have been computed recently *)
    BuildOkL,  (* known to have been recently built using the local pool *)
    BuildOk,   (* known to have been recently built using the all pools *)
    BuildFailed, (* last built known to have failed *)
    ShippedToLP, (* known to have been recently shipped to the local pool *)
    ShippedToPP, (* known to have been recently shipped to the project pool *)
    ShippedToGP, (* known to have been recently shipped to the global pool *)
    IsRelease,   (* known to be checked out on a release branch *)
    NoRelease,   (* known to be not checked out on a release branch *)
    None         (* this value used internally *)
  };

  AttrSet = SET OF Attr;

(*---------------------------------------------------------------------------*)
CONST
  AttrRepr = ARRAY Attr OF TEXT {
    "chg",
    "mod",
    "unmod",
    "utd",
    "ood",
    "cfl",
    "nocfl",
    "dep",
    "bokl",
    "bok",
    "bf",
    "slp",
    "spp",
    "sgp",
    "rel",
    "norel",
    ""
  };

(*---------------------------------------------------------------------------*)
(* A Checkpoint.T is a data structure that is used to store information
   about directory trees. The information stored is a set of attributes
   for every directory tree (Checkpoint.AttrSet) and a fingerprint
   (Fingerprint.T). The fingerprint of a directory is based on the
   names, sizes, and modification times of all files and subdirectories
   in that directory. The method used to compute these fingerprints
   guarantees that there is a very high probability that if any change
   is made to a directory or its contents, the fingerprint will change.
   The exact method to compute fingerprints from texts or byte sequences
   is explained in the Fingerprint.i3 interface. 

   To compute the fingerprints for directories, the Checkpoint module
   uses a file cache, since the same information may be needed several
   times and must not invoke multiple disk lookup operations.
   The FileInfo.T has been extended by the method `fingerprint()',
   which computes fingerprints based on the cache content for files
   and directories (recursively). Since the file cache may be used for 
   other purposes, too, it is not instantiated in the Checkpoint module,
   but rather passed as a parameter.

   The Checkpoint.T itself basically just realizes an associative 
   array, i.e. a mapping from path names to fingerprints and attributes.
|
|    checkpoint = pathname --> (fingerprint, set of attributes)
|
*)

(*---------------------------------------------------------------------------*)
TYPE
  T <: Public;

  Public = OBJECT
  METHODS
    (*-----------------------------------------------------------------------*)
    init(cache : FileInfo.T; msgif : MsgIF.T := NIL) : T;
    (* Create an empty Checkpoint object and use the `cache' for all
       operations on the file system. *)

    (*-----------------------------------------------------------------------*)
    addDir(dir : Pathname.T; fp : REF Fingerprint.T := NIL) RAISES {Error};
    (* Add `dir' to the checkpointed directories, initializing the
       checkpoint with `fp'. If `fp' is NIL, then the fingerprint
       is immediately computed from the current file system state. *)

    (*-----------------------------------------------------------------------*)
    delDir(dir : Pathname.T);
    (* Delete `dir' from the checkpointed directories. *)

    (*-----------------------------------------------------------------------*)
    dirs() : TextSeq.T;
    (* Return all checkpointed directories. *)

    (*-----------------------------------------------------------------------*)
    fingerprint(dir : Pathname.T) : REF Fingerprint.T;
    (* Return the cached fingerprint for `dir'. *)

    (*-----------------------------------------------------------------------*)
    update(dir : Pathname.T := NIL; missingOnly := FALSE) RAISES {Error};
    (* (Re)compute all fingerprints from the current file system state
       if `dir' is NIL, otherwise only the fingerprint for `dir'. *)

    (*-----------------------------------------------------------------------*)
    diff(cp : T) : TextSeq.T;
    (* Return all directories that haven't got the same fingerprint as
       those in `cp'. Mark all differing directories as `Changed' in this
       checkpoint object. *)

    (*-----------------------------------------------------------------------*)
    selectByAttr(attr : AttrSet) : TextSeq.T;
    (* Return all directories, for which any of the attributes in `attr'
       is set. *)

    (*-----------------------------------------------------------------------*)
    toText() : TEXT RAISES {Error};
    (* Convert a checkpoint to a text. *)
    
    (*-----------------------------------------------------------------------*)
    fromRd(rd : Rd.T) RAISES {Error};
    (* Initialize a checkpoint from an input stream. *)

    (*-----------------------------------------------------------------------*)
    fromText(t : TEXT) RAISES {Error};
    (* Initialize a checkpoint from a text. *)

    (*-----------------------------------------------------------------------*)
    toFile(fn : Pathname.T) RAISES {Error};
    (* Write the checkpoint to file `fn'. *)

    (*-----------------------------------------------------------------------*)
    fromFile(fn : Pathname.T) RAISES {Error};
    (* Initialize the checkpoint from file `fn'. *)

    (*-----------------------------------------------------------------------*)
    getAttr(dir : Pathname.T) : AttrSet RAISES {Error};
    (* Return the cached attributes for `dir'. *)

    (*-----------------------------------------------------------------------*)
    setAttr(dir : Pathname.T; attr : AttrSet) RAISES {Error};
    (* Remember attributes `attr' for `dir'. *)

    (*-----------------------------------------------------------------------*)
    setVal(dir : Pathname.T; name, val : TEXT) RAISES {Error};
    (* Define a binding from `name' to `val' in the checkpoint. *)

    (*-----------------------------------------------------------------------*)
    getVal(dir : Pathname.T; name : TEXT) : TEXT RAISES {Error};
    (* Return the value of variable `name' in the checkpoint. *)

    (*-----------------------------------------------------------------------*)
    delVal(dir : Pathname.T; name : TEXT) RAISES {Error};
    (* Delete the binding with name `name' in the checkpoint. *)

  END;

(*---------------------------------------------------------------------------*)
PROCEDURE AttrToText(attr : AttrSet) : TEXT;

(*---------------------------------------------------------------------------*)
PROCEDURE AttrFromText(t : TEXT; VAR attr : AttrSet) RAISES {Error};

(*---------------------------------------------------------------------------*)
PROCEDURE New(c : FileInfo.T; msgif : MsgIF.T := NIL) : T;

(*---------------------------------------------------------------------------*)
PROCEDURE DefineIgnorePatterns(
    cacheIgnoreDirs : TEXT := NIL; 
    cacheIgnoreFiles : TEXT := NIL;
    fingerprintIgnoreDirs : TEXT := NIL;
    fingerprintIgnoreFiles : TEXT := NIL) RAISES {Error};
  (* If any of the parameters are non-nil, they override the internal
     default patterns that are used to keep the cache and checkpoint
     clean of rubbish files. All patterns are treated as `find expressions'
     from the package fsfind, module FindExpr. Syntactically they consist
     of boolean expressions (and, or, not, true, false, prentheses) with
     shell globbing patterns or regular expressions as basic elements.
     Regular expressions must be prefixed by a `+' character.
  *)

(*---------------------------------------------------------------------------*)
VAR
  ignoreDirExpr : FindExpr.T;   (* cache ignore dirs *)
  ignoreFileExpr : FindExpr.T;  (* cache ignore files *)
  skipDirExpr : FindExpr.T;     (* fingerprint ignore dirs *)
  skipFileExpr : FindExpr.T;    (* fingerprint ignore files *)
END Checkpoint.
