(*---------------------------------------------------------------------------*)
INTERFACE FindExpr;

IMPORT FSFindError, GlobTree;

CONST Brand = "FindExpr rev. 0.0";

(*---------------------------------------------------------------------------*)
TYPE T = GlobTree.T;

(*---------------------------------------------------------------------------*)
VAR RegularExpressionPrefix := '+';

(*---------------------------------------------------------------------------*)
PROCEDURE New(t : TEXT) : T RAISES {FSFindError.E};

END FindExpr.
