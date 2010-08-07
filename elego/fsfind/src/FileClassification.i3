(*---------------------------------------------------------------------------*)
INTERFACE FileClassification;

IMPORT TextSeq, TextTextTbl, Pathname;
IMPORT FindExpr, FindExprSeq;

EXCEPTION E(TEXT);

TYPE
  T <: Public;

  (*-------------------------------------------------------------------------*)
  (*
    A file classification is a rule consiting if a left hand side and
    a right hand side: lhs => rhs. The left hand side is a FindExpr.T
    (a regular expression or a shell globbing expression); the right 
    hand side is a text which possibly contains some predefined
    variables.

    A typical classification could look like

|     "*.i3"        =>  "m3_interface({:fn})"
|     "*.m3"        =>  "m3_module({:fn})"
|     "*.ig"        =>  "m3_generic_interface({:fn})"
|     "*.mg"        =>  "m3_generic_module({:fn})"
|     "+tmp[0-9]*"  =>  "temporary_file({:fn})"
|     "CVS"         =>  skip
|     "*~"          =>  ignore
|     default       =>  "undefined({:fn})"

    The left hand side is called `expression' in this module, the
    right hand side `classSpec'. If the left hand side consists of
    the keyword `default', this is not handled as a pattern, but rather
    as a special fall-back value for names that don't match any
    other rule.

    File classification rules may be read from a file or a text.
  *)
  Public = OBJECT
  METHODS
    (*-----------------------------------------------------------------------*)
    init(usePosixPathnames := FALSE) : T;
    (* Returns an empty file classification object. *)

    (*-----------------------------------------------------------------------*)
    size() : CARDINAL;
    (* Returns the number of rules for file classification. *)

    (*-----------------------------------------------------------------------*)
    addFromText(t : TEXT) RAISES {E};
    (* Add one or more rules for file classification from the test `t'.
       The rules are appended to the existing ones. In case of syntax
       errors, an exception with a descriptive text is raised. *)

    (*-----------------------------------------------------------------------*)
    addFromFile(fn : Pathname.T) RAISES {E};
    (* Add one or more rules for file classification from the file `fn'.
       The rules are appended to the existing ones. In case of syntax
       errors, an exception with a descriptive text is raised. *)

    (*-----------------------------------------------------------------------*)
    patterns() : FindExprSeq.T;
    (* Returns a list of all pattern expressions (left hand sides)
       of the rules in form of a list of FindExpr.T objects. *)

    (*-----------------------------------------------------------------------*)
    classSpecs() : TextSeq.T;
    (* Returns a list of all right hand sides of the rules, i.e. all
       class specifications. *)

    (*-----------------------------------------------------------------------*)
    pattern(n : CARDINAL) : FindExpr.T;
    (* Returns the left hand side of rule `n'. Rules are numbered from
       0 to size() - 1. *)

    (*-----------------------------------------------------------------------*)
    classSpec(n : CARDINAL) : TEXT;
    (* Returns the right hand side of rule `n'. *)

    (*-----------------------------------------------------------------------*)
    ignoreDir(dir : Pathname.T) : BOOLEAN;
    (* <=> ignore directory `dir' and all its sub directories *)

    (*-----------------------------------------------------------------------*)
    matches(fn : Pathname.T) : INTEGER RAISES {E};
    (* Match the name `fn' against all pattern expressions beginning
       with rule 0 until a match is found or all rules have failed.
       Return the rule number or -1 in case of failure. *)

    (*-----------------------------------------------------------------------*)
    match(fn : TEXT; env : TextTextTbl.T := NIL) : TEXT RAISES {E};
    (* Match the name `fn' against all pattern expressions beginning
       with rule 0 until a match is found or all rules have been
       tried. If a pattern matches, return the class specification
       of that rule with all variables substituted, or NIL if no
       match is found. If `env' is not NIL, it is used as environment
       for variable substitution; in any case, the variable `fn'
       will be defined. If not all variable references can be
       resolved, an exception is raised. *)

    (*-----------------------------------------------------------------------*)
    substClassSpec(n : CARDINAL; fn : Pathname.T; 
                   env : TextTextTbl.T := NIL) : TEXT RAISES {E};
    (* Use the right hand side of rule `n' to return a class specification
       where the variable `fn' and all additional variables from `env'
       have been substituted by their values. Raise an exception if
       any reference cannot be resolved. *)

  END;

(*---------------------------------------------------------------------------*)
PROCEDURE New(t : TEXT; usePosixPathnames := FALSE) : T RAISES {E};
  (* Return a new file classification object with all rules of `t'. *)

(*---------------------------------------------------------------------------*)
PROCEDURE Read(fn : Pathname.T; usePosixPathnames := FALSE) : T RAISES {E};
  (* Return a new file classification object with all rules read from
     file `fn'. *)

END FileClassification.
