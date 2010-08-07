(*---------------------------------------------------------------------------*)
INTERFACE CVS;

IMPORT TextSeq, TextTextTbl, TextTextSeqTbl, TextLockInfoTbl;
IMPORT APN AS APN, APNSeq AS APNSeq, FileRevisionSeq;
IMPORT MsgIF;

(*---------------------------------------------------------------------------*)
EXCEPTION E(TEXT);

(*---------------------------------------------------------------------------*)
TYPE
  T <: Public;
  Public = OBJECT 
  METHODS
    (*-----------------------------------------------------------------------*)
    init(msgif : MsgIF.T := NIL) : T;

    (*-----------------------------------------------------------------------*)
    setCVSPath(cvsPath : APN.T);
    (* sets the command used to execute cvs actions (default = "cvs") *)

    (*-----------------------------------------------------------------------*)
    setCVSROOT(cvsroot : TEXT);
    (* Define the CVSROOT setting used in calls to cvs. *)

    (*-----------------------------------------------------------------------*)
    setCVSEDITOR(cvseditor : TEXT);
    (* Define the editor used for commit messages. *)

    (*-----------------------------------------------------------------------*)
    setCVSIgnore(ignorePatterns : TEXT);
    (* Define the patterns passed as CVSIGNORE environment variable. *)

    (*-----------------------------------------------------------------------*)
    setDCVSPath(cvsPath : APN.T);
    (* sets the command used to execute dcvs actions (default = "dcvs") *)

    (*-----------------------------------------------------------------------*)
    setDCVSROOT(cvsroot : TEXT);
    (* Define the CVSROOT setting used in calls to dcvs. *)

    (*-----------------------------------------------------------------------*)
    setPreferCVS(val : BOOLEAN := TRUE);

    (*-----------------------------------------------------------------------*)
    allFiles(dir : APN.T) : FileRevisionSeq.T;
    (* scan `dir' and all its subdirectories for CVS/Entries files and
       return a list of all entries (all files known to CVS *)

    (*-----------------------------------------------------------------------*)
    logFile(fn : APN.T; heedErrors := TRUE; headerOnly := FALSE;
            local := FALSE) : TEXT
    RAISES {E};
    (* returns the CVS log file of `fn' *)

    (*-----------------------------------------------------------------------*)
    listSnap(pattern : TEXT; heedErrors := TRUE) : TEXT RAISES {E};
    (* lists all DCVS snapshots matching `pattern' *)

    (*-----------------------------------------------------------------------*)
    snapExists(pattern : TEXT; heedErrors := TRUE) : BOOLEAN RAISES {E};
    (* <=> TRUE if a DCVS snapshots matching `pattern' exists *)

    (*-----------------------------------------------------------------------*)
    snapList(pattern : TEXT; heedErrors := TRUE; longList := FALSE) : TextSeq.T
    RAISES {E};
    (* return a list of snapshots matching `pattern'. If longList is
       FALSE, the server prefix (/<sid>/) will be removed *)

    (*-----------------------------------------------------------------------*)
    readStickySnapFile(fn: APN.T): TEXT RAISES {E};
    (* return the contents of the sticky snapshot file if it exists or
       NIL *)

    (*-----------------------------------------------------------------------*)
    status(fn : APN.T; VAR rev, stat : TEXT) RAISES {E};
    (* returns the revision, status, and label of  `fn' *)

    (*-----------------------------------------------------------------------*)
    changeDesc(fn : APN.T) : TEXT RAISES {E};
    (* Returns a package state change description. *)

    (*-----------------------------------------------------------------------*)
    setLabel(fn : APN.T; rev, label: TEXT) : BOOLEAN;
    (* sets the label of revision `rev' of `fn' *)

    (*-----------------------------------------------------------------------*)
    getLabel(fn : APN.T; rev : TEXT) : TEXT RAISES {E};
    (* returns the state label of revision `rev' of file `fn' *)

    (*-----------------------------------------------------------------------*)
    revisionsAndLabels(rlog : TEXT) : TextTextTbl.T RAISES {E};
    (* returns a mapping from revisons to state labels for a given
       rlog file *)

    (*-----------------------------------------------------------------------*)
    tags(fn : APN.T; prefix : TEXT; local := FALSE) : TextSeq.T RAISES {E};
    (* List the tags of file or directory `fn' that begin with `prefix'.
       Note: if `prefix' is empty, all existing tags are listed. *)
    
    (*-----------------------------------------------------------------------*)
    tagsAndSnaps(fn : APN.T; prefix : TEXT; local := FALSE;
                 pkgName : TEXT := NIL) : TextSeq.T RAISES {E};
    (* List the tags of file or directory `fn' that begin with
       `prefix' as well as all snapshots matching pattern `prefix'.
       Note: if `prefix' is empty, all existing tags are listed. *)
    
    (*-----------------------------------------------------------------------*)
    stateList(fn : APN.T) : TextSeq.T RAISES {E};
    (* Return the result of `cvs -n fn' *)
    
    (*-----------------------------------------------------------------------*)
    taggedRevisions(fn : APN.T; local := FALSE) : TextTextTbl.T RAISES {E};
    (* Return a mapping from tags to revision numbers for file or
       directory `fn'. *)

    (*-----------------------------------------------------------------------*)
    revisionTags(fn : APN.T; local := FALSE) : TextTextSeqTbl.T RAISES {E};
    (* Return a mapping from revision numbers to tags for file or
       directory `fn'. *)

    (*-----------------------------------------------------------------------*)
    checkout(fn : APN.T; tag : TEXT := NIL; VAR res : TEXT) : BOOLEAN;
    (* Check out file OR directory `fn' from the CVS repository, using
       `tag' as symbolic revision, if not NIL. *)

    (*-----------------------------------------------------------------------*)
    commit(fn : APN.T; msg : TEXT := NIL; msgFile : TEXT := NIL;
           force := FALSE; desc := ""; pkg := "";
           changeSetName : TEXT := NIL) : BOOLEAN RAISES {E};
    (* Commit all changes made to file or directory `fn' and use `msg' or
       the text from `msgFile' as log message, if not NIL. Otherwise we
       will call an interactive editor. `desc' may contain a generated
       description of the changes, and `pkg' the package name. 
       If `changeSetName' is not NIL, then for DCVS servers a change
       set with the given name is created during the commit. *)

    (*-----------------------------------------------------------------------*)
    update(fn : APN.T; rev : TEXT; VAR res : TEXT;
           createDirs := TRUE; pruneDirs := TRUE) : BOOLEAN;
    (* Update file or directory `fn' to the (symbolic) revision `rev'. *)

    (*-----------------------------------------------------------------------*)
    merge(fn : APN.T; rev : TEXT; rev2 : TEXT := NIL;
          with_d_option : BOOLEAN := FALSE;
          VAR res : TEXT) : BOOLEAN;
    (* Merge the changes from branch `rev' for file or directory `fn'
       into the current branch. If `rev2' is not NIL, merge only the 
       differences between `rev' and `rev2'. *)

    (*-----------------------------------------------------------------------*)
    diff(fn : APN.T; from : TEXT := NIL; to : TEXT := NIL;
         udiff := FALSE; cdiff := FALSE; 
         flist : APNSeq.T := NIL; VAR res : TEXT) : BOOLEAN;
    (* Produce a diff listing for file or directory `fn', either 
|       o between the current workspace and the repository, or
|       o between the current workspace and `from', or
|       o between revisions `from' and `to'
       and return the result in `res'. Produce a context diff, if `cdiff' is
       TRUE, produce a unified context diff, if `udiff' is TRUE, otherwise
       produce a traditional unix diff.

       If `flist' is not NIL, use fn as the base directory and the
       contents of `flist' as relative pathnames.
    *)

    (*----------------------------------------------------------------------*)
    annotate(fn : APN.T; flist : APNSeq.T := NIL) : TEXT RAISES {E};
       (* Return an annotated file listing *)

    (*-----------------------------------------------------------------------*)
    tag(fn : APN.T; tag : TEXT; branch := FALSE; force := FALSE) : BOOLEAN;
    (* Tag file or directory `fn' with the tag `tag' *)
    
    (*-----------------------------------------------------------------------*)
    tagAgain(fn : APN.T; oldtag, newtag : TEXT; 
             branch := FALSE; force := FALSE) : BOOLEAN;
    (* Tag file or directory `fn' with the new tag `newtag' at `oldtag'. *)

    (*-----------------------------------------------------------------------*)
    snapshot(fn : APN.T; tag : TEXT) : BOOLEAN;
    (* Create a DCVS snapshot with the given tag as name *)

    (*-----------------------------------------------------------------------*)
    modified(fn : APN.T; VAR res : TEXT) : BOOLEAN RAISES {E};
    (* Return TRUE if file or directory `fn' is modified compared to the
       CVS repository. Fill `res' with a list of differing or questionable
       files. *)

    (*-----------------------------------------------------------------------*)
    upToDate(fn : APN.T; VAR res : TEXT) : BOOLEAN RAISES {E};
    (* Return TRUE if file or directory `fn' is up-to-date compared to the
       CVS repository. Fill `res' with a list of differing or questionable
       files. *)

    (*-----------------------------------------------------------------------*)
    conflicts(fn : APN.T; VAR res : TEXT) : BOOLEAN RAISES {E};
    (* Return TRUE if file or directory `fn' conflicts with the
       CVS repository. Fill `res' with a list of differing or questionable
       files. *)

    (*-----------------------------------------------------------------------*)
    import(fn : APN.T; vtag, rtag : TEXT; VAR res : TEXT;
           msg : TEXT := NIL; msgFile : TEXT := NIL;
           desc := ""; pkg := "") : BOOLEAN 
    RAISES {E};
    (* Import everything beneath directory `fn' as `fn' using
       vendor tag `vtag' and release tag `rtag'. *)

    (*-----------------------------------------------------------------------*)
    add(fn : APN.T; binary := FALSE) : BOOLEAN RAISES {E};
    (* Add the file or directory `fn' to the repository. *)
    
    (*-----------------------------------------------------------------------*)
    remove(fn : APN.T) : BOOLEAN RAISES {E};
    (* Remove the file or directory `fn' from the repository. *)

    (*-----------------------------------------------------------------------*)
    currentStickyTags(fn : APN.T) : TextSeq.T RAISES {E};
    (* Return the current sticky tags of file or directory `fn'. *)

    (*-----------------------------------------------------------------------*)
    flushCache();
    (* Flush the internal CVS operation cache. *)

    (*-----------------------------------------------------------------------*)
    (* The following operations form a minimal interface to CVS advisory
       file locks. There seem to be two major problems with the current
       CVS implementation: (1) There is no way to list files that are
       `watched' if there are no actual `watchers', and (2) there is no
       atomic locking operation (CVS edit allows multiple simultaneous
       edits). So we may be surprised if we check for other editors,
       issue an edit command because there's no one else at work,
       and then re-check the list of editors... This is bad (TM).
    *)
    (*-----------------------------------------------------------------------*)
    watch(fn : APN.T; cmd : TEXT := "on"; recursive := TRUE) RAISES {E};
    (* Issue a cvs watch command with the given argument `cmd', which
       may be 'on|off|add|remove'. *)

    (*-----------------------------------------------------------------------*)
    editorInfo(fn : APN.T; recursive := TRUE) : TextLockInfoTbl.T RAISES {E};
    (* Return a table of CVS locking information for the specified file
       or directory. *)

    (*-----------------------------------------------------------------------*)
    editors(fn : APN.T; recursive := TRUE) : TextSeq.T RAISES {E};
    (* Return a list of users editing the specified file or directory. *)

    (*-----------------------------------------------------------------------*)
    edit(fn : APN.T; recursive := TRUE) RAISES {E};
    (* Issue a cvs edit command, which (1) notes the fact that we are
       now going to change this file, and (2) make the file writeable
       in our workspace. *)

    (*-----------------------------------------------------------------------*)
    unedit(fn : APN.T; recursive := TRUE) RAISES {E};
    (* Issue a cvs unedit command, which (1) abandons all changes to the
       file, and (2) makes the file readonly again. *)
  END;

(*---------------------------------------------------------------------------*)
PROCEDURE IsCVSBranch(revNum: TEXT): BOOLEAN;
(* <=> n > 2 AND n MOD 2 = 1 OR 
       n > 2 AND n MOD 2 = 0 AND Last(Prefix(r)) = 0
       for n = NumParts(r) *)

(*---------------------------------------------------------------------------*)
VAR
  noAction := FALSE;
  useCvsLock := FALSE;
END CVS.
