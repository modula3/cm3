(*---------------------------------------------------------------------------*)
INTERFACE PkgVC;

IMPORT TextSeq, TextTextTbl, TextTupleSeq, Pathname;
IMPORT APN AS APN, APNSeq AS APNSeq, Tag, TagSeq;
IMPORT Confirmation, MsgIF, TextLockInfoTbl;

CONST Brand = "PkgVC 2.0";

(*---------------------------------------------------------------------------*)
(*
  This is an interface for package version control using CVS. 
  Currently there are no other implementations available. If they get
  added, probably the interface will not only have to be abstracted
  in a technical, but also in a semantical way. I try to minimize the
  assumptions made by this interface, but you never know in advance...

  The VC-interface is realized by a singular object (w.r.t. network 
  objects), that is accessable via the global variable VC of type 
  PkgVC.T. (Do not create other instances, since they can only be 
  initialized within the implementation module.) All version control 
  funktions are realized as methods of this object.

  Please note that sometimes tags are passed as TEXTs, sometimes as
  Tag.Ts. I am not yet sure if this is a bug or a feature...
  The output of the last action of the underlying version control 
  system (CVS) can be accessed via the object variable lastVCMsg.
  Every error situation reported by the underlying CVS module 
  will raise an PkgVC.E(TEXT) exception, where TEXT represents
  an appropriate error message. 
*)


(*------------------------------------------------------------------------*)
TYPE
  ConfirmClosure = Confirmation.Closure;

(*---------------------------------------------------------------------------*)
TYPE
  CommitType = {Major, Minor, Patch};

(*---------------------------------------------------------------------------*)
TYPE
  Mode = {Operational, Test};

(*---------------------------------------------------------------------------*)
TYPE
  LockType = {None, Binary, All};

(*---------------------------------------------------------------------------*)
EXCEPTION E(TEXT);

(*---------------------------------------------------------------------------*)
TYPE
  T <: Public;
  Public = OBJECT 
              (*-------------------------------------------------------------*)
              lastVCMsg : TEXT;
              (* The last message to stdout resp. an internal result 
                 message of the underlying version control system (CVS).
              *)
  METHODS
    (*-----------------------------------------------------------------------*)
    init(msgif : MsgIF.T := NIL) : T;
       (* Define default values for everything. *)

    (*-----------------------------------------------------------------------*)
    flushCache();
       (* Purge any cached version information. *)

    (*-----------------------------------------------------------------------*)
    ignorePatterns() : TEXT;
       (* Return the current patterns for ignored files. *)

    (*-----------------------------------------------------------------------*)
    packageRelativePathname(pn : Pathname.T) : Pathname.T RAISES {E};
       (* Return a pathname that is relative to the package root
          directory, if possible.
       *)

    (*-----------------------------------------------------------------------*)
    symbolicRevisionName(tag : Tag.T; avoidHead := FALSE) : TEXT RAISES {E};
       (* Return the textual representation of a tag, considering the
          special meaning of Tag.Head and Tag.Tip in the environment
          of the current package.
       *)

    (*-----------------------------------------------------------------------*)
    setPackageRoot(rootDir : APN.T) RAISES {E};
       (* Set the global root directory of the package we are working on. 
          Get the name of the package from this directory.
       *)

    (*-----------------------------------------------------------------------*)
    setMode(m : Mode);
       (* Set mode to operational (default) or test (no actions are
          executd. *)

    (*-----------------------------------------------------------------------*)
    setEnvironment(env : TextTextTbl.T; repoMapping : TextTupleSeq.T := NIL);
       (* This is the general interface to pass various (possibly)
          implementation dependent parameters to the underlying version
          control system. The current (CVS-only) implementation uses the
          following values, if they exist:
|          o editor
|          o repository
|          o cvspath
|          o dcvs-repository
|          o dcvspath
|          o prefer-dcvs
|          o vc-options
|          o vc-locking (none|lazy, binary|default, all|strict)
       *)

    (*-----------------------------------------------------------------------*)
    getEnvironment() : TextTextTbl.T;

    (*-----------------------------------------------------------------------*)
    lockingScheme() : LockType;

    (*-----------------------------------------------------------------------*)
    newCollection(path : APN.T; msg : TEXT := NIL; 
                  msgFile : APN.T := NIL) RAISES {E} ;
       (* Create a new empty collection at `path' and put it under
          version control. 
       *)

    (*-----------------------------------------------------------------------*)
    checkout(pkgname : TEXT; tag : Tag.T) RAISES {E} ;
       (* Checkout the package using symbolic revision `tag' from the 
          repository. Return any results displayed by the underlying 
          version control system in `res'. `Tag' may have the special 
          value `Tag.Head'. 
       *)

    (*----------------------------------------------------------------------*)
    update(tag : Tag.T) RAISES {E} ;
       (* Update the package using symbolic revision `tag' from the repository.
          Return any results displayed by the underlying version control
          system in `res'. `Tag' may have the special values `Tag.Head' and
          `Tag.Tip'. 
       *)

    (*----------------------------------------------------------------------*)
    merge(tag : Tag.T; tag2 : Tag.T := NIL; with_d_option : BOOLEAN := FALSE)
    RAISES {E} ;
       (* Merge in changes using symbolic revision `tag' from the repository.
          If `tag2' is not NIL, merge only the differences between `tag'
          and `tag2'.
       *)

    (*----------------------------------------------------------------------*)
    log(fn : APN.T := NIL) : TEXT RAISES {E} ;
       (* Return the package log if fn = NIL, else the corresponding file
          log. *)

    (*----------------------------------------------------------------------*)
    niceLog(fn : APN.T := NIL; displayTags := TRUE; 
            displayRevs := TRUE; displayLog := TRUE) : TEXT RAISES {E} ;
       (* Return a nicely formatted package or file log. *)

    (*----------------------------------------------------------------------*)
    diff(from : Tag.T := NIL; to : Tag.T := NIL;
         udiff := FALSE; cdiff := FALSE;
         flist : APNSeq.T := NIL) : TEXT RAISES {E} ;
       (* Return a diff between the current version and the repository or
          between the versions identified by `from' and `to'. If `flist'
          is not NIL, consider only the files in the list to produce the 
          diff.
       *)

    (*----------------------------------------------------------------------*)
    annotate(flist : APNSeq.T := NIL) : TEXT RAISES {E};
       (* Return an annotated file listing *)

    (*----------------------------------------------------------------------*)
    commitChanges(ct : CommitType; msg : TEXT := NIL; 
                  msgFile : APN.T := NIL) RAISES {E} ;
       (* Commit any changes made to the local workspace of the package to the
          current branch (usually the main trunk). Stick a development tag
          on the whole package after the commit. Use `msg' or `msgFile' as
          source for the commit log if they are not NIL. The development tag
          is of the form `devel_pkgname_major_minor_release'. If `ct' is
          CommitType.Major, then the next major version is computed and used;
          if it is CommitType.Minor, the next minor version is computed and
          used; if it is CommitType.Patch, the next patchlevel is computed 
          and used.
       *)

    (*----------------------------------------------------------------------*)
    commitRelease(ct : CommitType; msg : TEXT := NIL; 
                  msgFile : APN.T := NIL) RAISES {E} ;
       (* Commit any changes made to the local workspace of the package to the
          current branch (usually the main trunk). Stick a release tag
          on the whole package after the commit. Use `msg' or `msgFile' as
          source for the commit log if they are not NIL. The release tag
          is of the form `release_pkgname_major_minor_release'. If `ct' is
          CommitType.Major, then the next major version is computed and used;
          if it is CommitType.Minor, the next minor version is computed and
          used; if it is CommitType.Patch, the next patchlevel is computed 
          and used. Finally a release stable branch is established for the
          new release. The branch tag used is of the form
          `release_pkgname_major_minor_stable'.
       *)

    (*----------------------------------------------------------------------*)
    commitToChangeBranch(changeName : TEXT; changeType : Tag.Kind;
                         ct : CommitType; newBranch := FALSE;
                         msg : TEXT := NIL; msgFile : APN.T := NIL) RAISES {E};

      (*
        Commit changes to a local change branch (for use with DCVS).
      *)

    (*----------------------------------------------------------------------*)
    tagAll(tag : Tag.T; branch := FALSE; force := FALSE) RAISES {E} ;
       (* Tag all elements of the package with `tag', 
          if it is not yet used. 
       *)

    (*----------------------------------------------------------------------*)
    add(obj : APN.T; recursive := FALSE; interactive := FALSE;
        binary := FALSE) : BOOLEAN RAISES {E} ;
       (* Add the object denoted by `obj' to the package (i.e. put it under
          version control). The parameters `recursive' and `interactive' mean
          their obvious meaning...
       *)

    (*----------------------------------------------------------------------*)
    remove(obj : APN.T; recursive := FALSE; interactive := FALSE) : BOOLEAN 
    RAISES {E};
    (* Remove the object denoted by `obj' from the package (i.e. discontinue
       version control). The parameters `recursive' and `interactive' mean
       their obvious meaning...
    *)

    (*----------------------------------------------------------------------*)
    setLocking(obj : APN.T; cmd := "on"; recursive := TRUE) RAISES {E};
    (* Set strict locking regardless of the currently active locking
       scheme. *)

    (*----------------------------------------------------------------------*)
    lockForEdit(obj : APN.T; recursive := TRUE) RAISES {E};
    (* Lock the object denoted by `obj' for editing and make it writable.
       If the given object is already being edited by others, this
       provokes an exception. *)

    (*----------------------------------------------------------------------*)
    revert(obj : APN.T; recursive := TRUE) RAISES {E};
    (* Revert all local modifications made to a file that has previously
       been locked by `lockForEdit'. *)

    (*----------------------------------------------------------------------*)
    editorInfo(obj : APN.T; recursive := TRUE) : TextLockInfoTbl.T RAISES {E};
    (* Return a table of CVS locking information for the specified file
       or directory. *)

    (*----------------------------------------------------------------------*)
    editors(obj : APN.T; recursive := TRUE) : TextSeq.T RAISES {E};
    (* Return a list of all editors of the given object. *)

    (*----------------------------------------------------------------------*)
    lockedByMe(obj : APN.T; recursive := TRUE) : BOOLEAN RAISES {E};
    (* <=> The given object is locked for editing solely by the current
       user. *)

    (*----------------------------------------------------------------------*)
    lockedByOther(obj : APN.T; recursive := TRUE) : BOOLEAN RAISES {E};
    (* <=> The given object is locked for editing by somebody else. *)

    (*----------------------------------------------------------------------*)
    known(obj : APN.T) : BOOLEAN RAISES {E};
    (* The object denoted by `obj' is under version control. *)

    (*----------------------------------------------------------------------*)
    upToDate() : BOOLEAN RAISES {E};
       (* <=> The package is up-to-date *)

    (*----------------------------------------------------------------------*)
    modified() : BOOLEAN RAISES {E};
       (* <=> Elements of the package have been locally modified 
          (or added or) removed. 
       *)

    (*----------------------------------------------------------------------*)
    conflicts() : BOOLEAN RAISES {E};
       (* <=> Elements of the package conflict with the repository. *)

    (*----------------------------------------------------------------------*)
    tagList(prefix : TEXT) : TextSeq.T RAISES {E};
       (* Return all tags beginning with `prefix' *)

    (*----------------------------------------------------------------------*)
    tags(prefix : TEXT; fn : APN.T := NIL) : TagSeq.T RAISES {E};
       (* Return all tags beginning with `prefix' *)

    (*----------------------------------------------------------------------*)
    tagExists(t : Tag.T) : BOOLEAN RAISES {E};

    (*----------------------------------------------------------------------*)
    isSticky(VAR tag : Tag.T) : BOOLEAN RAISES {E};
       (* <=> The package is checked out with a sticky tag. Return the
          appropriate tag if true. 
       *)

    (*----------------------------------------------------------------------*)
    isRelease(VAR tag : Tag.T) : BOOLEAN RAISES {E};
       (* <=> The package is checked out with a release tag. Return the
          appropriate tag if true. 
       *)

    (*----------------------------------------------------------------------*)
    isReleaseBranch(VAR tag : Tag.T) : BOOLEAN RAISES {E};
       (* <=> The package is checked out on a release branch. Return the
          appropriate tag if true. 
       *)

    (*----------------------------------------------------------------------*)
    latestReleaseBranch(VAR tag : Tag.T) : BOOLEAN RAISES {E};
       (* <=> There has been a release of this package. Return the
          appropriate tag.
       *)

    (*----------------------------------------------------------------------*)
    isChangeBranch(VAR tag : Tag.T) : BOOLEAN RAISES {E};

    (*----------------------------------------------------------------------*)
    latestChangeBranch(kind : Tag.Kind;
                       VAR tag : Tag.T) : BOOLEAN RAISES {E};

    (*----------------------------------------------------------------------*)
    currentDevelopmentTag() : Tag.T RAISES {E};
       (* Return the current development tag for the package. *)

    (*----------------------------------------------------------------------*)
    nextDevelopmentTag(ct : CommitType) : Tag.T RAISES {E};
       (* Return the next development tag for the package. *)

    (*----------------------------------------------------------------------*)
    currentReleaseTag() : Tag.T RAISES {E};
       (* Return the current release tag for the package. *)

    (*----------------------------------------------------------------------*)
    nextReleaseTag(ct : CommitType; branch : Tag.T := NIL) : Tag.T RAISES {E};
       (* Return the next release tag for the package. *)

    (*----------------------------------------------------------------------*)
    currentLocalTag() : Tag.T RAISES {E};
       (* Return the current local tag of the package, which is either
          a sticky tag (if all elements are tagged as sticky), or the 
          current development tag (if no elements are sticky and the
          package is up-to-date, or the tag that was current when the 
          package was checked out (last entry of PkgTags). *)

    (*----------------------------------------------------------------------*)
    (* Support for Package Directories and Package Element Control          *)
    (*----------------------------------------------------------------------*)
    (*
      In the package root directory, a listing of all package elements
      with complete relative pathnames in POSIX notation may reside.
      This file is named `PkgDirectory' and is usually maintained 
      automatically by the PkgVC module. If it exists when the
      package root is defined, its contents are read in and cached.
    *)
    (*----------------------------------------------------------------------*)
    packageDirectory() : APNSeq.T RAISES {E};
      (* Get the list of files from a Manifest or Directory file if it
         exists in the package root. Return NIL if no file exists.
         Raise an exception in case of read errors only. *)

    (*----------------------------------------------------------------------*)
    overwritePackageDirectory(dir : APNSeq.T) RAISES {E};
      (* Write the list `dir' as the package directory file. *)

    (*----------------------------------------------------------------------*)
    updatePackageDirectory(VAR added, removed : APNSeq.T;
                           confirmRemoval := TRUE;
                           confirmAddition := FALSE) RAISES {E};
      (* Read the package directory file again and add and remove
         elements according to its contents. Ask for confirmation
         according to the third and fourth parameter. *)

    (*----------------------------------------------------------------------*)
    addFiles(flist : APNSeq.T) RAISES {E};
      (* Put all files in `flist' under version control and update the
         package directory. *)

    (*----------------------------------------------------------------------*)
    (* Operations ignoring the PkgDirectory file                            *)
    (*----------------------------------------------------------------------*)
    versionControlledFiles(includePkgName := FALSE) : APNSeq.T RAISES {E};
      (* Return a list of all files that are under version control
         in the current package. Gather this information directly from the
         underlying version control machine. *)

    (*----------------------------------------------------------------------*)
    checkVersionControlStatus(fl               : APNSeq.T; 
                              VAR vcFiles      : APNSeq.T;
                              VAR unknownFiles : APNSeq.T) RAISES {E};
      (* Check all files in `fl' and put those under version control into
         `vcFiles' and the others into `unknownFiles'. Do not use the
         PkgDirectory file, but gather the information directly from the
         underlying version control machine. *)

    (*----------------------------------------------------------------------*)
    getFileStatus(VAR added : APNSeq.T;
                  VAR removed : APNSeq.T;
                  VAR modified : APNSeq.T;
                  VAR conflicts : APNSeq.T;
                  VAR needingUpdate : APNSeq.T;
                  VAR unknown : APNSeq.T;
                  includePkgName := FALSE) RAISES {E};
      (* Return lists of all files in the packages sorted by their
         version control status, i.e. modified, needing update etc. *)

    (*----------------------------------------------------------------------*)
    (* Support for Package States and Labels                                *)
    (*----------------------------------------------------------------------*)
    (*
      Packages may be labelled by an arbitrary text. This feature is used
      to record the quality state of the package as e.g. Exp (experimental,
      CVS default), buggy (contains known non-too-serious bugs), broken
      (contains serious bugs and must not be used), or approved (released 
      for use by quality control).
    *)
    (*----------------------------------------------------------------------*)
    stateLabels() : TextTextTbl.T RAISES {E};
      (* Return a mapping from all known tags to state labels. *)

    (*----------------------------------------------------------------------*)
    stateLabel(tag : Tag.T := NIL) : TEXT RAISES {E};
      (* Return the state label for `tag'. If `tag' = NIL, the currently
         checked out state is assumed. *)

    (*----------------------------------------------------------------------*)
    setLabel(tag : Tag.T; label : TEXT; msg := NIL) RAISES {E};
      (* Set the state label for version `tag' of the package to `label'.
         Record the setting in the log file for the package. If `msg' is
         NIL, just add date and user of the event. *)

    (*----------------------------------------------------------------------*)
    labelLog() : TEXT RAISES {E};
      (* Get all log messages about changes of state labels for this
         package. *)

  END; (* Public *)


VAR
  VC : T;
  confirmation : ConfirmClosure;
END PkgVC.

