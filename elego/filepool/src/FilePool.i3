(*---------------------------------------------------------------------------*)
INTERFACE FilePool;

IMPORT Rd, Wr, TextSeq, Pathname;

(* A FilePool.T is an object that facilitates operations on a set of
   files contained in one directory. *)

(*---------------------------------------------------------------------------*)
EXCEPTION Error(TEXT);

(*---------------------------------------------------------------------------*)
TYPE 
  ProcClosure = OBJECT
  METHODS
    proc(fn : TEXT) RAISES ANY;
  END;

(*---------------------------------------------------------------------------*)
TYPE 
  PredClosure = OBJECT
  METHODS
    pred(fn : TEXT) : BOOLEAN RAISES {Error};
  END;

(*---------------------------------------------------------------------------*)
TYPE
  T <: Public;

  Public = OBJECT
  METHODS
    (*-----------------------------------------------------------------------*)
    init(dir : Pathname.T) : T RAISES {Error};
    (* Initialize the internal cache with information about all files
       in directory `dir'. 
    *)

    (*-----------------------------------------------------------------------*)
    update() RAISES {Error};
    (* Reinitialize the internal cache from the file system. *)

    (*-----------------------------------------------------------------------*)
    list(pattern : TEXT := NIL; ordinaryOnly := TRUE) : TextSeq.T 
    RAISES {Error};
    (* Return a list of all files, excluding the current and parent
       directory. If `pattern' is non-NIL, it is used to match against
       all the file names. 
    *)

    (*-----------------------------------------------------------------------*)
    getReader(fn : TEXT) : Rd.T RAISES {Error};
    (* Return a reader for file `fn'. *)

    (*-----------------------------------------------------------------------*)
    getWriter(fn : TEXT) : Wr.T RAISES {Error};
    (* Return a destructive writer for file `fn'. Destructive means that if
       this operations is called, the contents of the file are erased.
    *)

    (*-----------------------------------------------------------------------*)
    content(fn : TEXT) : TEXT RAISES {Error};
    (* Return the contents of file `fn' as a text. *)

    (*-----------------------------------------------------------------------*)
    createNewFile(fn : TEXT := NIL) : TEXT RAISES {Error};
    (* Create a new file with a unique name and return this name. If
       `fn' is non-NIL, take it as the file name and raise an exception
       if it already exists.
    *)

    (*-----------------------------------------------------------------------*)
    delete(fn : TEXT) RAISES {Error};
    (* Delete the named file. *)

    (*-----------------------------------------------------------------------*)
    apply(cl : ProcClosure; pattern : TEXT := NIL; ordinaryOnly := TRUE)
    RAISES ANY;
    (* Apply the closure `cl' to all cached file names. *)

    (*-----------------------------------------------------------------------*)
    select(cl : PredClosure; pattern : TEXT := NIL; ordinaryOnly := TRUE) 
    : TextSeq.T RAISES {Error};
    (* Apply the predicate `cl' to all cached file names and return the
       list of the names that evaluated to TRUE.
    *)

  END;
END FilePool.
