(*---------------------------------------------------------------------------*)
INTERFACE PrjDesc;

IMPORT Rd, TextSeq, TextTextTbl, Time;
IMPORT Confirmation, PkgBase, PoolSet, ProcessEnv, MsgIF;

(*---------------------------------------------------------------------------*)
EXCEPTION Error(TEXT);

(*---------------------------------------------------------------------------*)
CONST Brand = "PrjDesc 0.0";

(*---------------------------------------------------------------------------*)
CONST ExecutionFailure = -8888;

(*---------------------------------------------------------------------------*)
VAR
  debugStateCache := FALSE;

(*---------------------------------------------------------------------------*)
TYPE
  T <: Public;

  Public = OBJECT
  METHODS
    (*-----------------------------------------------------------------------*)
    init(fn : TEXT; cfg : PkgBase.T; check := FALSE;
         defaultCollectionRoot : TEXT := NIL; cpfn : TEXT := NIL;
         useCache := TRUE; env : ProcessEnv.T := NIL; 
         msgif : MsgIF.T := NIL;
         pkgvcAcc : PoolSet.PkgVCAccessor := NIL;
         verboseCacheMsgs := TRUE;
         preferredPkgKind : TEXT := NIL;
         depsMandatory := TRUE;
         cacheEarly := FALSE) : T 
    RAISES {Error};
    (* Read in the project description file `fn', store its information,
       check all the collections and packages listed in it if check = TRUE. 
       Raise Error in case of any syntactical, semantical or other error.

       If `pkgvcAcc' is defined, then all version control actions will
       be performed internally and not by external programs. The method
       pkgvcAcc.getVCIF(pkgPath) will be called to get an existing
       version control interface for each package. If this method
       returns NIL, an internal interface will be created and
       initialized properly.
    *)

    (*-----------------------------------------------------------------------*)
    reinit(fn : TEXT; cfg : PkgBase.T; check := FALSE;
           defaultCollectionRoot : TEXT := NIL; cpfn : TEXT := NIL;
           useCache := TRUE; env : ProcessEnv.T := NIL; 
           msgif : MsgIF.T := NIL;
           pkgvcAcc : PoolSet.PkgVCAccessor := NIL) RAISES {Error};
    (* re-initialize all internal settings *)

    (*-----------------------------------------------------------------------*)
    parse(rd : Rd.T) RAISES {Error};
    loadFile(fn : TEXT) RAISES {Error};
    (* Read and parse the file `fn'. *)

    (*-----------------------------------------------------------------------*)
    getName() : TEXT;
    getCreationDate() : Time.T;
    getModificationDate() : Time.T;
    getUser() : TEXT;
    getDescription() : TEXT;
    setName(name : TEXT);
    setCreationDate(date : Time.T);
    setModificationDate(date : Time.T);
    setUser(user : TEXT);
    setDescription(desc : TEXT);
    (* Attribute access via get/set procedures *)

    (*-----------------------------------------------------------------------*)
    logText() : TEXT;
    (* Release or snapshot log text *)
    snapshotText(snapshotName : TEXT) : TEXT RAISES {Error};
    releaseText(snapshotName : TEXT) : TEXT RAISES {Error};
    toText() : TEXT RAISES {Error};

    (*-----------------------------------------------------------------------*)
    pkgsOkay(VAR res : TEXT) : BOOLEAN;
    (* Check all the collections and packages and return TRUE in case
       of success, FALSE else. *)

    (*-----------------------------------------------------------------------*)
    getPoolSet() : PoolSet.T;
    (* Return the pool set associated with this project. *)

    (*-----------------------------------------------------------------------*)
    setExternalShell(shell : TEXT);
    (* Define an external shell to execute all commands. By default the
       simple built-in command list interpreter from System.ExecuteList()
       is used. *)

    (*-----------------------------------------------------------------------*)
    setPreferredPkgKind(k : PkgBase.Kind);
    (* Set the package kind that is preferred for all packages in case
       of several possibilities.
    *)

    (*-----------------------------------------------------------------------*)
    defineGlobalVar(name, val : TEXT);
    (* Define the value of a global variable whose value is substituted 
       in all external program calls for the pattern {:name} in each
       call. 
    *)

    (*-----------------------------------------------------------------------*)
    deleteGlobalVar(name : TEXT);
    (* Delete the value for global variable `name'. *)

    (*-----------------------------------------------------------------------*)
    defineGlobalVars(vars : TextTextTbl.T);
    (* Define all variables contained in `vars'. *)

    (*-----------------------------------------------------------------------*)
    collectionPath() : TEXT;
    (* Return the collection root directory. This is the collection path
       set in the PrjDesc file with any ${HOME}, ${USER}, ${PRJ_ROOT}
       variables substituted by their values in the environment.
    *)

    (*-----------------------------------------------------------------------*)
    packages() : TextSeq.T;
    (* Return a list of all packages. *)

    (*-----------------------------------------------------------------------*)
    locations() : TextSeq.T;
    (* Return a complete list of all package locations. *)

    (*-----------------------------------------------------------------------*)
    packagesCollection() : TextTextTbl.T;
    (* Return a mapping from packages to their collection. *)

    (*-----------------------------------------------------------------------*)
    collections() : TextSeq.T;
    (* Return a list of all collections. *)

    (*-----------------------------------------------------------------------*)
    collectionsLocation() : TextTextTbl.T;
    (* Return a mapping from collections to their location. *)

    (*-----------------------------------------------------------------------*)
    snapshots() : TextSeq.T;
    (* Return a list of all snapshots. *)

    (*-----------------------------------------------------------------------*)
    releases() : TextSeq.T;
    (* Return a list of all releases. *)

    (*-----------------------------------------------------------------------*)
    snapshot(name : TEXT) : TextTextTbl.T;
    (* Return the mapping from packages to versions for snapshot `name'. *)

    (*-----------------------------------------------------------------------*)
    release(name : TEXT) : TextTextTbl.T;
    (* Return the mapping from packages to versions for release `name'. *)

    (*-----------------------------------------------------------------------*)
    defineSnapshot(name : TEXT; snap : TextTextTbl.T) RAISES {Error};
    (* Create a new snapshot of the given package -> version mapping and call
       it `name'. *)

    (*-----------------------------------------------------------------------*)
    defineRelease(name : TEXT; snap : TextTextTbl.T) RAISES {Error};
    (* Create a new release of the given package -> version mapping and call
       it `name'. *)

    (*-----------------------------------------------------------------------*)
    getTags(packageList : TextSeq.T) : TextTextTbl.T RAISES {Error};
    (* Get all the current tags of the packages in packageList. *)

    (*-----------------------------------------------------------------------*)
    newSnapshot(name : TEXT) RAISES {Error};
    (* Create a new snapshot of the current project description and call
       it `name'. *)

    (*-----------------------------------------------------------------------*)
    newRelease(name : TEXT) RAISES {Error};
    (* Create a new release of the current project description and call
       it `name'. This obviously fails if not all packages are checked
       out as released versions. *)

    (*-----------------------------------------------------------------------*)
    checkoutSnapshot(name : TEXT) RAISES {Error};
    (* Check out the snapshot `name'. *)

    (*-----------------------------------------------------------------------*)
    checkoutRelease(name : TEXT) RAISES {Error};
    (* Check out the snapshot `name'. *)

    (*-----------------------------------------------------------------------*)
    checkoutHead() RAISES {Error};
    (* Check out the current development versions of all packages. *)

    (*-----------------------------------------------------------------------*)
    checkoutTrunkOrBranchHead(pkgs : TextSeq.T) RAISES {Error};
    (* Check out the latest versions of the given packages either on the
       development trunk or the current release branch. *)

    (*-----------------------------------------------------------------------*)
    checkoutPackages(pkgs : TextSeq.T; tag := "head") RAISES {Error};
    (* Check out the versions of the given packages with the given tag *)

    (*-----------------------------------------------------------------------*)
    buildDepGraph(confirmationCl : Confirmation.Closure := NIL) 
    RAISES {Error};
    (* Build the dependency graph for all packages contained in the project. 
       If packages are missing, this procedure tries to check them out.
       If confirmationCl is not NIL, is asks the user for confirmation
       before doing it. *)

    (*-----------------------------------------------------------------------*)
    readDepGraph(fn : TEXT) RAISES {Error};
    (* Read the dependency graph from file `fn' *)

    (*-----------------------------------------------------------------------*)
    writeDepGraph(fn : TEXT) RAISES {Error};
    (* Write the dependency graph to file `fn' *)

    (*-----------------------------------------------------------------------*)
    readDepGraphAsText(fn : TEXT) RAISES {Error};
    (* Read the dependency graph in ASCII representationn from file `fn' *)

    (*-----------------------------------------------------------------------*)
    writeDepGraphAsText(fn : TEXT) RAISES {Error};
    (* Write the dependency graph in ASCII representation to file `fn' *)

    (*-----------------------------------------------------------------------*)
    write(fn : TEXT) RAISES {Error};
    (* Write the contents excluding the dependency graph to file `fn'. *)

    (*-----------------------------------------------------------------------*)
    writeSnapshot(fn : TEXT; name : TEXT) RAISES {Error};
    (* Write the complete project snapshot but no other information, 
       excluding the dependency graph, to file `fn'. *)

    (*-----------------------------------------------------------------------*)
    writeRelease(fn : TEXT; name : TEXT) RAISES {Error};
    (* Write the complete project releaseshot but no other information, 
       excluding the dependency graph, to file `fn'. *)

    (*-----------------------------------------------------------------------*)
    packageUpdateSequence() : TextSeq.T;
    (* Return the list of packages topological sorted by their import
       relation. *)

    (*-----------------------------------------------------------------------*)
    ignoredPackages() : TextSeq.T;
    (* Return the list of packages, that are imported but not listed in 
       the project and thus cannot be processed. *)

    (*-----------------------------------------------------------------------*)
    missingPackages() : TextSeq.T;
    (* Return the list of packages that were found to be missing,
       incomplete or incorrect during the last check. *)

    (*-----------------------------------------------------------------------*)
    applyToPackages(action  : PkgBase.Action; 
                    action2 : PkgBase.Action := NIL; 
                    action3 : PkgBase.Action := NIL;
                    cpkgs   : TextSeq.T := NIL;
                    ordered := TRUE;
                    breakOnZeroReturn := FALSE;
                    breakOnError := TRUE; 
                    breakOnFailure := TRUE;
                    tag1Values : TextTextTbl.T := NIL;
                    tag2Values : TextTextTbl.T := NIL) : INTEGER 
    RAISES {Error};
    (* Apply the command sequence with the symbolic name `action' to
       all packages of the project. If `ordered', use the order defined
       by the import relation of the dependency graph. If `breakOnError',
       stop if any execution terminates with a result code # 0. If
       `breakOnFailure', stop if any execution raises an exception, 
       otherwise ignore it and continue. The exit code of the last
       command execution is returned. If this was an exception,
       the constant value `ExecutionFailure' is returned.
       If `breakOnZeroReturn' is TRUE, then the return code 0 is
       considered to be a termination criterium.
       If `cpkgs # NIL', only those packages in `cpkgs' and in
       the project are used.
       If `tag1Values' is # NIL, then the variable TAG1 will be
       defined with the mapping contained in it for every package
       name for which an action is executed. If `tag2Values' is # NIL,
       the same holds for TAG2.
    *)

    (*-----------------------------------------------------------------------*)
    applyCmdListDirectly(cmd     : TEXT;
                         cpkgs   : TextSeq.T := NIL;
                         ordered := TRUE;
                         breakOnZeroReturn := FALSE;
                         breakOnError := TRUE; 
                         breakOnFailure := TRUE) : INTEGER RAISES {Error};
    (* Apply the command sequence `cmd' to
       all packages of the project. If `ordered', use the order defined
       by the import relation of the dependency graph. If `breakOnError',
       stop if any execution terminates with a result code # 0. If
       `breakOnFailure', stop if any execution raises an exception, 
       otherwise ignore it and continue. The exit code of the last
       command execution is returned. If this was an exception,
       the constant value `ExecutionFailure' is returned.
       If `breakOnZeroReturn' is TRUE, then the return code 0 is
       considered to be a termination criterium.
       If `cpkgs # NIL', only those packages in `cpkgs' and in
       the project are used.
    *)

    (*-----------------------------------------------------------------------*)
    selectPackages(pred : PkgBase.Action; 
                   ordered := TRUE;
                   selectOnZeroReturn := TRUE;
                   breakOnFailure := TRUE;
                   tag1Values : TextTextTbl.T := NIL;
                   tag2Values : TextTextTbl.T := NIL) : TextSeq.T
    RAISES {Error};
    (* Apply the predicate with the symbolic name `pred' to all
       packages of the project. If `ordered', use the order defined
       by the import relation of the dependency graph. If 
       `selectOnZeroReturn' is TRUE, select the package if the last
       command of the predicate returned 0, otherwise select the
       package if it yielded anything not equal to 0. If `breakOnFailure'
       is TRUE, any exceptions raised by the execution will raise
       `Error'. In case of success, the list of selected packages
       is returned. 
       If `tag1Values' is # NIL, then the variable TAG1 will be
       defined with the mapping contained in it for every package
       name for which an action is executed. If `tag2Values' is # NIL,
       the same holds for TAG2.
    *)

    (*-----------------------------------------------------------------------*)
    selectByCmdList(cmd : TEXT;
                    ordered := TRUE;
                    selectOnZeroReturn := TRUE;
                    breakOnFailure := TRUE) : TextSeq.T RAISES {Error};
    (* Apply the predicate `cmd' to all
       packages of the project. If `ordered', use the order defined
       by the import relation of the dependency graph. If 
       `selectOnZeroReturn' is TRUE, select the package if the last
       command of the predicate returned 0, otherwise select the
       package if it yielded anything not equal to 0. If `breakOnFailure'
       is TRUE, any exceptions raised by the execution will raise
       `Error'. In case of success, the list of selected packages
       is returned. 
    *)

    (*-----------------------------------------------------------------------*)
    dependendPackages(pkg : TEXT) : TextSeq.T RAISES {};
    (* Return all packages that depend on `pkg'. *)

    (*-----------------------------------------------------------------------*)
    packageDependencies(pkg : TEXT) : TextSeq.T RAISES {};
    (* Return all packages that `pkg' depends on, i.e. all imports. *)

    (*-----------------------------------------------------------------------*)
    addDependingPackages(pkgs : TextSeq.T) : TextSeq.T;
    (* Return all packages in `pkgs' and all depending on them in the
       correct update order. *)

    (*-----------------------------------------------------------------------*)
    modifiedPackages() : TextSeq.T RAISES {Error};
    (* Return all modified packages of the project. *)

    (*-----------------------------------------------------------------------*)
    modifiedAndDependingPackages() : TextSeq.T RAISES {Error};
    (* Return all modified packages and those depending on them. *)

    (*-----------------------------------------------------------------------*)
    upToDatePackages() : TextSeq.T RAISES {Error};
    (* Return all packages of the project that are up-to-date. *)

    (*-----------------------------------------------------------------------*)
    outOfDatePackages() : TextSeq.T RAISES {Error};
    (* Return all packages of the project that are not up-to-date. *)

    (*-----------------------------------------------------------------------*)
    outOfDateAndDependingPackages() : TextSeq.T RAISES {Error};
    (* Return all out-of-date packages and those depending on them. *)

    (*-----------------------------------------------------------------------*)
    packagesWithConflicts() : TextSeq.T RAISES {Error};
    (* Return all packages of the project that are conflicting with the
       repository. *)

    (*-----------------------------------------------------------------------*)
    testAllPackagesReleased() : BOOLEAN;
    (* True if all packages in the project are checked out as release
       version. *)

    (*-----------------------------------------------------------------------*)
    testNoPackageModified() : BOOLEAN;
    (* True if no package has been modified. *)

    (*-----------------------------------------------------------------------*)
    cacheAllStateLabels(lazy := FALSE) RAISES {Error};
    (* Read all state labels of all versions of the packages and store
       them in a cache for further calls of the following three procedures. 
       If `lazy' is TRUE, old .label files will be used if found. *)

    (*-----------------------------------------------------------------------*)
    checkCurrentLabels(pattern : TEXT; lazy := FALSE) : TextSeq.T 
    RAISES {Error};
    (* Check all the current state labels of the packages against the
       regular expression `pattern'. *)

    (*-----------------------------------------------------------------------*)
    checkCurrentLabelsGen(pattern : TEXT; useCachedLabels := FALSE) 
    : TextSeq.T RAISES {Error};
    (* Check all the current state labels of the packages against the
       regular expression `pattern'. *)

    (*-----------------------------------------------------------------------*)
    checkLabelsOfSnapshot(name : TEXT; pattern : TEXT;
                          useCachedLabels := FALSE) : TextSeq.T RAISES {Error};
    (* Check all state labels of snapshot `name' against the
       regular expression `pattern'. *)

    (*-----------------------------------------------------------------------*)
    checkLabelsOfRelease(name : TEXT; pattern : TEXT;
                         useCachedLabels := FALSE) : TextSeq.T RAISES {Error};
    (* Check all state labels of release `name' against the
       regular expression `pattern'. *)

    (*-----------------------------------------------------------------------*)
    newCheckpoint(fn : TEXT; update := FALSE) RAISES {Error};
    (* Compute a new checkpoint from the current file system state,
       set all attributes to sensible start values for further operations,
       and write the checkpoint to file `fn'. *)

    (*-----------------------------------------------------------------------*)
    loadNewCheckpoint(fn : TEXT; update := FALSE) RAISES {Error};
    (* Load the checkpoint file `fn' and use it as the new project state.
       The old internal project state is remembered to do comparisons. *)

    (*-----------------------------------------------------------------------*)
    writeCheckpoint(fn : TEXT) RAISES {Error};
    (* Write the current internal state to checkpoint file `fn' for
       future references. *)

    (*-----------------------------------------------------------------------*)
    invalidateCachedUnsureVersionInfo() RAISES {Error};
    (* The attriutes `utd' and `nocfl' will be removed from all project
       state cache elements. *)

    (*-----------------------------------------------------------------------*)
    invalidateCachedBuildInfo() RAISES {Error};
    (* All building and shipping information will be removed from all project
       state cache elements. *)

  END;

END PrjDesc.
