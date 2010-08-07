(*---------------------------------------------------------------------------*)
INTERFACE Snapshots;

IMPORT TextSeq, PrjDesc, PkgBase, PkgVC, ChangeSet, SortedTextPrjDescTbl,
       SortedTimePrjDescTbl, SortedTextChangeSetTbl, SortedTimeChangeSetTbl;

(*---------------------------------------------------------------------------*)
TYPE Sort = {None, ByName, ByDate};

(*---------------------------------------------------------------------------*)
EXCEPTION Error(TEXT);

(*---------------------------------------------------------------------------*)
TYPE
  T <: Public;

  Public = OBJECT
  METHODS
    (*-----------------------------------------------------------------------*)
    init(dir : TEXT; cfg : PkgBase.T; pkgvc : PkgVC.T := NIL) : T 
    RAISES {Error};
    (* Initialize a new snapshot cache from directory `dir'. Read in an
       index file, if it exists, otherwise create one. Even create the
       directory if needed. *)

    (*-----------------------------------------------------------------------*)
    buildIndex() RAISES {Error};
    (* Build a new index file from the files contained in the cache. *)

    (*-----------------------------------------------------------------------*)
    readIndex() RAISES {Error};
    (* Read in the index file of the snapshot cache. *)

    (*-----------------------------------------------------------------------*)
    writeIndex() RAISES {Error};
    (* Write the index file of the snapshot cache to disk. *)

    (*-----------------------------------------------------------------------*)
    snapshotDefined(name : TEXT) : BOOLEAN;
    (* <=> A snapshot with `name' exists. *)

    (*-----------------------------------------------------------------------*)
    releaseDefined(name : TEXT) : BOOLEAN;
    (* <=> A release with `name' exists. *)

    (*-----------------------------------------------------------------------*)
    changeSetDefined(name : TEXT) : BOOLEAN;
    (* <=> A change set with `name' exists. *)

    (*-----------------------------------------------------------------------*)
    getSnapshot(name : TEXT) : PrjDesc.T RAISES {Error};
    (* Recreate a project description from the named snapshot. *)

    (*-----------------------------------------------------------------------*)
    getRelease(name : TEXT) : PrjDesc.T RAISES {Error};
    (* Recreate a project description from the named release. *)

    (*-----------------------------------------------------------------------*)
    getChangeSet(name : TEXT) : ChangeSet.T RAISES {Error};
    (* Retrieve the named change set. *)

    (*-----------------------------------------------------------------------*)
    putSnapshot(name : TEXT; snap : PrjDesc.T; ovwr := FALSE) RAISES {Error};
    (* Save a project description as a new snapshot. *)

    (*-----------------------------------------------------------------------*)
    putRelease(name : TEXT; snap : PrjDesc.T; ovwr := FALSE) RAISES {Error};
    (* Save a project description as a new release. *)

    (*-----------------------------------------------------------------------*)
    putChangeSet(name : TEXT; cs : ChangeSet.T; ovwr := FALSE) RAISES {Error};
    (* Save a change set. If ovwr = TRUE, then an old change set may be
       overwritten. *)

    (*-----------------------------------------------------------------------*)
    listSnapshots(so := Sort.None; up := TRUE) : TextSeq.T RAISES {Error};
    (* List all snapshots. *)

    (*-----------------------------------------------------------------------*)
    listReleases(so := Sort.None; up := TRUE) : TextSeq.T RAISES {Error};
    (* List all release. *)

    (*-----------------------------------------------------------------------*)
    listChangeSets(so := Sort.None; up := TRUE) : TextSeq.T RAISES {Error};
    (* List all change sets. *)

    (*-----------------------------------------------------------------------*)
    snapshotsByName() : SortedTextPrjDescTbl.T RAISES {Error};
    (* Generate a sorted (name -> snapshot) mapping *)

    (*-----------------------------------------------------------------------*)
    snapshotsByDate(sortByModificationDate := FALSE) : SortedTimePrjDescTbl.T
    RAISES {Error};
    (* Generate a sorted (name -> snapshot) mapping sorted by creation
       (or modification) date *)

    (*-----------------------------------------------------------------------*)
    releasesByName() : SortedTextPrjDescTbl.T RAISES {Error};
    (* Generate a sorted (name -> release) mapping *)

    (*-----------------------------------------------------------------------*)
    releasesByDate(sortByModificationDate := FALSE) : SortedTimePrjDescTbl.T
    RAISES {Error};
    (* Generate a sorted (name -> snapshot) mapping sorted by creation
       (or modification) date *)

    (*-----------------------------------------------------------------------*)
    changeSetsByName() : SortedTextChangeSetTbl.T RAISES {Error};
    (* Generate a (name -> changeset) mapping sorted by name *)

    (*-----------------------------------------------------------------------*)
    changeSetsByDate() : SortedTimeChangeSetTbl.T RAISES {Error};
    (* Generate a (name -> changeset) mapping sorted by date *)

    (*-----------------------------------------------------------------------*)
    everythingUnderVersionControl() RAISES {Error};
    (* Ensure that all snapshot related files are subject to version 
       control. *)

  END;

END Snapshots.
