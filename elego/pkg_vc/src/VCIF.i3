(*---------------------------------------------------------------------------*)
INTERFACE VCIF;

IMPORT TextSeq;

TYPE 
  T = OBJECT 
  METHODS
    (*-----------------------------------------------------------------------*)
    (* initialization *)

    setRepository(nameOrPath : TEXT);
    (* Define the location of the source repository to be used. *)

    getRepository() : TEXT;
    (* Get the location of the currently used source repository. *)

    setPackageRoot(path : TEXT);
    (* Define the package root path (no relation to working directory). *)

    setPackageName(name : TEXT);
    (* Define the package name (no relation to working directory). *)

    (*-----------------------------------------------------------------------*)
    (* package creation and import *)

    addPackage(name : TEXT);
    (* Create a new empty package in the repository. *)

    (*-----------------------------------------------------------------------*)
    (* state information *)

    listLog(elem : TEXT) : TEXT;
    (* Return the log for package element `elem'. *)

    listTags(prefix : TEXT) : TextSeq.T;
    (* List the package tags that begin with `prefix'.
       Note: if `prefix' is empty, all existing tags are listed. *)

    getStateList() : TextSeq.T;
    (* Return a list of added, removed, changed, conflicting, unknown, and
       to-be-updated elements of the packages a la cvs -n update. *)

    isModified() : BOOLEAN;
    (* <=> The current package is modified. *)

    isUpToDate() : BOOLEAN;
    (* <=> The current package is up-to-date. *)

    hasConflicts() : BOOLEAN;
    (* <=> The current package contains conflicts. *)

    (*-----------------------------------------------------------------------*)
    (* element creation and removal *)

    add(elem : TEXT) : BOOLEAN;
    (* Add element `elem' to the current package. *)

    remove(elem : TEXT) : BOOLEAN;
    (* Remove element `elem' from the current package. *)

    (*-----------------------------------------------------------------------*)
    (* version state changes *)

    commit(msg : TEXT := NIL; msgFile : TEXT := NIL) : BOOLEAN;
    (* Commit all changes made to the repository, if possible. *)

    update(rev : TEXT; VAR res : TEXT) : BOOLEAN;
    (* Update the workspace to revision `rev'. *)

    merge(rev : TEXT; VAR res : TEXT) : BOOLEAN;
    (* Merge changes from revision `rev' into the workspace. *)

    tag(tag : TEXT; branch := FALSE) : BOOLEAN;
    (* Tag the current repository state with `tag'. *)

    (*-----------------------------------------------------------------------*)
    (* implementation details *)

    flushCache();
    (* If the implementation uses a cache for efficiency reasons, 
       flush all its entries. 
    *)
  END;

END VCIF.
