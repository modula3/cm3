(*---------------------------------------------------------------------------*)
INTERFACE PoolSet;

IMPORT TextSeq, TextTextTbl, Pathname;
IMPORT PkgBase, PkgVC, Checkpoint, FileInfo, MsgIF;

(*---------------------------------------------------------------------------*)
EXCEPTION Error(TEXT);

(*---------------------------------------------------------------------------*)
TYPE
  PkgVCAccessor = OBJECT
  METHODS
    getVCIF(dir : Pathname.T) : PkgVC.T RAISES {Error} := NoVCIF;
  END;

(*---------------------------------------------------------------------------*)
PROCEDURE NoVCIF(self : PkgVCAccessor; dir : Pathname.T) : PkgVC.T 
  RAISES {Error};
  (* Returns NIL *)

(*---------------------------------------------------------------------------*)
TYPE
  PkgVCCreator = PkgVCAccessor OBJECT
    env   : TextTextTbl.T;(* environment with settings for the new interface *)
    msgif : MsgIF.T; (* msg interface used by the new vc interface *)
  METHODS
  OVERRIDES
    getVCIF := NewVCIF;
  END;

(*---------------------------------------------------------------------------*)
PROCEDURE NewVCIF(self : PkgVCCreator; dir : Pathname.T) : PkgVC.T 
  RAISES {Error};
  (* Returns a new package version control interface *)

(*---------------------------------------------------------------------------*)
TYPE
  T <: Public;

  (*
    A PoolSet.T is an ordered list of pools that can be searched for
    software packages (of different types).

  *)
  Public = OBJECT
  METHODS
    (*-----------------------------------------------------------------------*)
    init(cfg : PkgBase.T;
         fn  : TEXT := NIL;
         useCache := TRUE;
         p1  : Pathname.T := NIL;
         p2  : Pathname.T := NIL;
         p3  : Pathname.T := NIL;
         p4  : Pathname.T := NIL;
         p5  : Pathname.T := NIL;
         msgif  : MsgIF.T := NIL;
         pkgvcAcc : PkgVCAccessor := NIL;
         verboseCache := TRUE;
         prefkind : TEXT := NIL;
         cacheEarly := FALSE) : T RAISES {Error};
    (* 
       Initialize the search list with the given paths. `p1' has the greatest
       priority. The package type and action configuration must be contained
       in `cfg'.
       If `pkgvcAcc' is defined, then all version control actions will
       be performed internally and not by external programs. The method
       pkgvcAcc.getVCIF(pkgPath) will be called to get an existing
       version control interface for each package. If this method
       returns NIL, an internal interface will be created and
       initialized properly.
    *)

    (*-----------------------------------------------------------------------*)
    prependPool(p : Pathname.T) RAISES {Error};
    (* Prepend `p' to the search list. *)

    (*-----------------------------------------------------------------------*)
    appendPool(p : Pathname.T) RAISES {Error};
    (* Append `p' to the search list. *)

    (*-----------------------------------------------------------------------*)
    setPreferredPkgKind(k : PkgBase.Kind);
    (* Set the package kind that is preferred for all packages in case
       of several possibilities.
    *)

    (*-----------------------------------------------------------------------*)
    exists(pkg : PkgBase.Name; hint : Pathname.T := NIL) : BOOLEAN
    RAISES {Error};
    (* 
       Check for the existence (and the type) of package `pkg' in the list
       of pools and remember the results in an internal cache. Return
       TRUE if the package was found in one of the pools.
       If `hint' is non-NIL, only try the specified path to find the
       package.
    *)

    (*-----------------------------------------------------------------------*)
    pkgPath(name : PkgBase.Name) : Pathname.T;
    (*
      Return the path of the package with name `name' if cached,
      NIL else.
    *)

    (*-----------------------------------------------------------------------*)
    pkgVCIF(name : PkgBase.Name) : PkgVC.T RAISES {Error};
    (* Return the version control interface for the given package, if it
       exists (if internal version control is used), or NIL. *)

    (*-----------------------------------------------------------------------*)
    checkAll(pkgList : TextSeq.T; VAR res : TEXT;
             VAR missingPackages : TextSeq.T;
             hints : TextTextTbl.T := NIL; checkHomogeneity := TRUE;
             ignoreMissingPackages := FALSE) : BOOLEAN;
    (*
      Check for the existence and (type) homogeneity of all packages
      in `pkgList'. Return TRUE if all packages exist and are of the
      same type.
      If `hints' are non-NIL, then every package that is contained
      in the hint-table will be looked up at the specified location.
      This is a sort of `overwrite' for the already known pools.
    *)

    (*-----------------------------------------------------------------------*)
    pkgType(name : PkgBase.Name) : PkgBase.Kind;
    (* Return the type of package `name'. *)
      
    (*-----------------------------------------------------------------------*)
    execAction(pkg : PkgBase.Name; action : PkgBase.Action;
               VAR res : TEXT;
               externalShell : TEXT := NIL; 
               parameters : TextTextTbl.T := NIL) : INTEGER RAISES {Error};
    (*
      Execute the action associated with `action' in the
      package root directory of `pkg' via `System.ExecuteList' and 
      return the exit code of the process (shell).
      If `parameters' is not NIL, then is for each mapping (key -> val)
      that it contains, the appropriate parameter substitution is
      done on the command sequence associated with `action' before it
      is executed.
      If the action should produce a textual result and this is cached,
      then it is returned in `res'.
    *)

    (*-----------------------------------------------------------------------*)
    execCmdList(pkg : PkgBase.Name; cmd : TEXT;
                externalShell : TEXT := NIL) : INTEGER RAISES {Error};
    (*
      Execute the command list `cmd' in the package root directory of
      `pkg' via `System.ExecuteList' and return the exit code of
      the process (shell).
    *)

    (*-----------------------------------------------------------------------*)
    getCmdOutput(pkg : PkgBase.Name; cmd : TEXT; VAR ret : INTEGER) : TEXT 
      RAISES {Error};
    (* 
       Execute the command `cmd' in the package root directory of `pkg'
       via `System.RdExecute' and return the stdout and stderr of 
       the process.
    *)

    (*-----------------------------------------------------------------------*)
    getAndCacheVersionState(pkg : PkgBase.Name) : TEXT RAISES {Error};
    (*
      Get the version status of `pkg' via `pkgvm -sstat' and update
      the cache accordingly.
    *)

    (*-----------------------------------------------------------------------*)
    fileContents(pkg : PkgBase.Name; fn : TEXT) : TEXT RAISES {Error};
    (* Return the contents of file `fn' in package `pkg' as TEXT. *)

    (*-----------------------------------------------------------------------*)
    checkout(pkg : PkgBase.Name; checkoutCmd : PkgBase.Action;
             externalShell : TEXT := NIL;
             rootDir : Pathname.T := NIL;
             parameters : TextTextTbl.T := NIL) : INTEGER RAISES {Error};
    (*
      Checkout package `pkg' using the specified checkout command
      and external shell (if non-NIL). Substitute all `parameters'
      before the execution.
    *)

    (*-----------------------------------------------------------------------*)
    getFileCache() : FileInfo.T;
    (* Return the file cache used internally for all file system operations. *)

    (*-----------------------------------------------------------------------*)
    newCheckpoint(update := FALSE) : Checkpoint.T RAISES {Error};
    (* 
       Return a new checkpoint object associated with the internal
       file cache. Fingerprints for all packages are computed; all
       attributes are set to default values. If `update' is TRUE,
       the file cache information is synchronized with the actual
       disk state before the checkpoint is constructed; otherwise,
       the cache contents are used.
    *)

    (*-----------------------------------------------------------------------*)
    replaceStateCache(sc : Checkpoint.T);
    (* Replace the internally used state cache. *)

    (*-----------------------------------------------------------------------*)
    cachedState() : Checkpoint.T;
    (* Return the internally known state of all packages. *)

    (*-----------------------------------------------------------------------*)
    updateCache() RAISES {Error};
    (* (Re)compute the fingerprints of all packages from the disk state. *)

    (*-----------------------------------------------------------------------*)
    setAttr(pkg : PkgBase.Name; attr : Checkpoint.Attr) RAISES {Error};
    (* Set the attribute `attr' in the state cache. *)

    (*-----------------------------------------------------------------------*)
    clearAttr(pkg : PkgBase.Name; attr : Checkpoint.Attr) RAISES {Error};
    (* Clear the attribute `attr' in the state cache. *)

    (*-----------------------------------------------------------------------*)
    attrIsSet(pkg : PkgBase.Name; attr : Checkpoint.Attr) : BOOLEAN 
    RAISES {Error};
    (* <=> The attribute `attr' is set in the state cache. *)

    (*-----------------------------------------------------------------------*)
    setVal(pkg : PkgBase.Name; name, val : TEXT) RAISES {Error};
    (* Set the variable `name' to `val' in the state cache. *)

    (*-----------------------------------------------------------------------*)
    getVal(pkg : PkgBase.Name; name : TEXT) : TEXT RAISES {Error};
    (* Return the value of variable `name' in the state cache; 
       NIL if undefined. *)

    (*-----------------------------------------------------------------------*)
    delVal(pkg : PkgBase.Name; name : TEXT) RAISES {Error};
    (* Delete the variable `name' in the state cache. *)

    (*-----------------------------------------------------------------------*)
    updateStateCache(dir : Pathname.T; action : PkgBase.Action; 
                     ret : INTEGER; rescan := TRUE) RAISES {Error};
    (* Update the state for package `pkg' considering the `action' 
       executed and its return value `ret'. The special actions 
       `need-mkdep-build-ship' and `need-build-ship' may be used 
       to reset some cache attributes for recently changed packages,
       also the actions `clear-mod-unmod' and `clear-utd-nocfl'. 
       Parameter `rescan' may be set to false, if the fingerprint
       needs no updating; otherwise, this procedure will decide based 
       on the action performed. *)

    (*-----------------------------------------------------------------------*)
    actionProbablyNeeded(pkg     : PkgBase.Name;  
                         dir     : Pathname.T; 
                         action  : PkgBase.Action;
                         VAR ret : INTEGER;
                         VAR res : TEXT) : BOOLEAN RAISES {Error};
    (* <=> Based on information in the state cache, `action' is probably
       needed to achieve a satisfactory result :-) `ret' is the
       fake return value of the action and is based on cache information. *)

    (*-----------------------------------------------------------------------*)
    dumpStateCache(header : TEXT);
    (* Write the `header' followed by the contents of the state cache
       to stdout. *)
  END;

END PoolSet.
