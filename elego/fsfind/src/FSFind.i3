(*---------------------------------------------------------------------------*)
INTERFACE FSFind;

IMPORT Pathname, TextSeq, Wr;
IMPORT FSFindError, FindExpr, FindExprSeq, FileClassification;

(*---------------------------------------------------------------------------*)
PROCEDURE List(
    root : Pathname.T;              (* root of tree traversal *)
    expr : FindExpr.T;              (* expression to select files *)
    ignoreDirs : FindExpr.T := NIL; (* selects which directories to ignore *)
    stripPrefix := TRUE;
  ) : TextSeq.T RAISES {FSFindError.E};
  (* Traverses a directory tree beginning at `root' and selects files
     that match the given expression, avoiding all directories
     selected by `ignoreDirs'. *)
  
(*---------------------------------------------------------------------------*)
PROCEDURE SimpleClassify(
    root       : Pathname.T;        (* root of tree traversal *)
    patterns   : FindExprSeq.T;     (* list of expressions to select files *)
    ignoreDirs : FindExpr.T := NIL; (* selects which directories to ignore *)
    recursive  := TRUE;
    stripPrefix := TRUE;
  ) : REF ARRAY OF TextSeq.T RAISES {FSFindError.E};
  (* Traverses a directory tree beginning at `root' and selects files
     that match any of the expressions given in `patterns', avoiding
     all directories selected by `ignoreDirs'. Classify returns an array of
     lists of the same length as `patterns'; each list contains the
     names of files matching the corresponding expression in `patterns'. *)
  
(*---------------------------------------------------------------------------*)
PROCEDURE ClassifyToWr(
    root : Pathname.T;             (* root of tree traversal *)
    fc   : FileClassification.T;   (* file classification rules *)
    wr   : Wr.T;
    recursive  := TRUE;
    flush  := FALSE;
    stripPrefix := TRUE;
  ) RAISES {FSFindError.E};
  (* Traverses a directory tree beginning at `root' and classifies
     files according to rules in `fc'. The results are written 
     to `wr'. If `flush' is TRUE, then the writer is flushed after
     every line. *)

(*---------------------------------------------------------------------------*)
PROCEDURE Classify(
    root : Pathname.T;             (* root of tree traversal *)
    fc   : FileClassification.T;   (* file classification rules *)
    recursive  := TRUE;
    stripPrefix := TRUE;
  ) : TEXT RAISES {FSFindError.E};
  (* Traverses a directory tree beginning at `root' and classifies
     files according to rules in `fc'. *)
  
END FSFind.
