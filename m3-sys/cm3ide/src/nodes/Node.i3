(* Copyright 1995-96 Critical Mass, Inc. All rights reserved.    *)

(* A "Node.T" is the basic entry in the browsable database. *)

INTERFACE Node;

IMPORT Wr, Thread;
IMPORT ID, RegExpr, Wx;

TYPE
  T = OBJECT
  METHODS
    class     () : Class;
    arcname   () : ID.T;
    filename  () : TEXT;
    printname () : TEXT;
    match     (re: RegExpr.T): BOOLEAN := MatchName;
    iterate   (VAR s: IteratorState);
    next      (VAR s: IteratorState): BOOLEAN;
    gen_page  (wx: Wx.T; action: ID.T;  data: FormData)
                RAISES {Wr.Failure, Thread.Alerted};
  END;

TYPE
  Named_T = T OBJECT
    name    : ID.T := ID.NoID;
    parent  : T    := NIL;
    sibling : T    := NIL;
  OVERRIDES
    arcname   := DefaultArcName;
    filename  := DefaultName;
    printname := DefaultName;
  END;

TYPE
  Class = {
    Unknown, Root, BuildPkgRoot, BrowsePkgRoot,
    (** BuildPackage, BrowsePackage, **) Directory,
    Program, Library, Interface, Module, GenericInterface,
    GenericModule, CSource, HSource, QuakeSource, MiscSource,
    Type, TypeDecl, ProcDecl, Proc,
    Except, Var, Const, GFormal,
    ClassDir, Resource, CacheEntry, BuildError, BuildWarn,
    RawFile,
    (* package roots *)
    PR00, PR01, PR02, PR03, PR04, PR05, PR06, PR07, PR08, PR09,
    PR10, PR11, PR12, PR13, PR14, PR15, PR16, PR17, PR18, PR19,
    PR20, PR21, PR22, PR23, PR24, PR25, PR26, PR27, PR28, PR29,
    PR30, PR31, PR32, PR33, PR34, PR35, PR36, PR37, PR38, PR39,
    PR40, PR41, PR42, PR43, PR44, PR45, PR46, PR47, PR48, PR49
  };

CONST
    FirstPkgRoot = Class.PR00;
    LastPkgRoot  = Class.PR49;

CONST
  ClassName = ARRAY Class OF TEXT {
    "??", "Root", "Build package root", "Browse package root",
    (** "Build package", "Browse package", **) "Directory",
    "Program", "Library", "Interface", "Module", "Generic interface",
    "Generic module", "C source", "C include", "Quake source", "Misc source",
    "Type", "Type declaration", "Procedure declaration", "Procedure",
    "Exception", "Variable", "Constant", "Generic formal",
    "Class", "Resource", "Build result",  "Build error", "Build warning",
    "File",
    (* package roots *)
    "Package", ..
   };

VAR
  ClassTag := ARRAY Class OF TEXT {
    NIL, NIL, NIL, NIL, (** "build-pkg", "browse-pkg", **) "directory",
    "program", "library", "interface", "module", "generic-interface",
    "generic-module", "c-source", "h-source", "quake", "misc-source",
    "type", "type-decl", "proc-decl", "procedure",
    "exception", "variable", "constant", "generic-formal",
    NIL, "rsrc", "cache-entry", "build-error", "build-warning",
    "file",
    (* package roots *)
    NIL, ..
   };

VAR (*READONLY*)
  ClassID: ARRAY Class OF ID.T;
  (* ClassID[c] == ID.Add (ClassTag[c]) *)

VAR
  ClassPlural := ARRAY Class OF TEXT {
    "??", "Roots", "Build package roots", "Browse package roots",
    (** "Build packages", "Browse packages", **) "Subdirectories",
    "Programs", "Libraries", "Interfaces", "Modules", "Generic interfaces",
    "Generic modules", "C sources", "C includes", "Quake sources", "Misc sources",
    "Types", "Type declarations", "Procedure declarations", "Procedures",
    "Exceptions", "Variables", "Constants","Generic formals",
    "Categories", "Resources", "Builds", "Build errors", "Build warnings",
    "Files",
    (* package roots *)
    "Packages", ..
   };

CONST
  ClassIcon = ARRAY Class OF TEXT {
    "unknown", "roots", "build-root", "browse-root",
    (** "build-pkg", "browse-pkg", **) "dir",
    "pgm", "lib", "x-i3", "x-m3", "x-ig",
    "x-mg", "x-c", "x-h", "x-quake", "x-misc",
    "type", "type-decl", "proc-decl", "proc-body",
    "except", "var", "const", "gformal",
    "class", "rsrc", "cache", "error", "warn",
    "file",
    (* package roots *)
    "pkg", ..
   };

CONST (* for HTTP Window targeting *)
  ClassWindow = ARRAY Class OF TEXT {
    NIL, NIL, NIL, NIL,
    (** "package", "package", **) "package",
    "source", "source", "source", "source", "source",
    "source", "source", "source", "source", "source",
    "type", "source", "source", "source",
    "source", "source", "source", "source",
    NIL, NIL, "build",  "source", "source",
    NIL,
    (* package roots *)
    "package", ..
   };

CONST (* whether need M3MarkUp.ThisDecl tags for the url *)
  ClassHasDecl = ARRAY Class OF BOOLEAN {
    FALSE, FALSE, FALSE, FALSE,
    (** FALSE, FALSE, **) FALSE,
    FALSE, FALSE, FALSE, FALSE, FALSE,
    FALSE, FALSE, FALSE, FALSE, FALSE,
    FALSE, TRUE, TRUE, TRUE,
    TRUE, TRUE, TRUE, TRUE,
    FALSE, FALSE, FALSE, FALSE, FALSE,
    FALSE,
    (* package roots *)
    FALSE, ..
  };

TYPE
  FormData = REF RECORD
    field: TEXT;
    value: TEXT;
    next : FormData;
  END;

PROCEDURE DefaultName (t: Named_T): TEXT;
(* Returns "ID.ToText (t.name)". *)

PROCEDURE DefaultArcName (t: Named_T): ID.T;
(* Returns "t.name". *)

PROCEDURE MatchName (t: T;  re: RegExpr.T): BOOLEAN;
(* Returns "RegExpr.Match (re, t.arcname())". *)

(*-------------------------------------------------------- iterators ---*)

TYPE
  IteratorState = RECORD
    pattern : RegExpr.T;
    match   : T;
    a, b, c : INTEGER;
    d, e, f : REFANY;
  END;

(* To iterate over the children of a node "n", finding those
   that match regular expression "r" (or "NIL" to match all
   children):

|     VAR istate: IteratorState;
|     istate.pattern := r;
|     n.iterate (istate);
|     WHILE n.next (istate, x) DO
|       << process child "istate.match" >>
|     END;

*)

(*------------------------------------------------------------ sets ---*)

TYPE
  List  = REF RECORD head: T;  tail: List;  END;
  Array = REF ARRAY OF T;
  Set   = RECORD cnt: INTEGER := 0;  elts: Array := NIL;  END;

PROCEDURE Append (VAR s: Set;  t: T);
(* Add node "t" to set "s". *)

PROCEDURE Sort (VAR s: Set);
(* Sort the elements of "s" by (class, name, full name) *)

PROCEDURE Squash (VAR s: Set);
(* Sort "s" and remove any duplicates. *)

(*----------------------------------------------------------- names ---*)

PROCEDURE FullPath (t: T): TEXT;
(* Returns the fully qualified filesystem path of node "t". *)

PROCEDURE CompareFullName (a, b: T): INTEGER;
(* Returns the lexicographic comparison of "a"'s and "b"'s
   full names (URLs) without fully materializing them. *)

PROCEDURE FindArcs (t: T;  VAR x: ARRAY OF T): CARDINAL;
(* Returns the number of arcs in "t"'s fully qualified name
   and sets "x[i]" to the "i"th arc from the root of the name.
   It is a checked runtime error if "x" is too short. *)

PROCEDURE Init ();

END Node.
