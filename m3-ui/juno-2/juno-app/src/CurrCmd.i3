(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Sun Oct 26 13:38:59 PST 1997 by heydon                   *)
(*      modified on Sun Feb 19 16:09:28 PDT 1995 by gnelson                  *)
(*      modified on Fri Aug  7 21:51:52 PDT 1992 by myers                    *)

(* A "CurrCmd.T" maintains an abstract syntax tree and allows editing commands
   to be performed on it. A "CurrCmd.T" has no graphical representation.

   A "CurrCmd" is a Juno command of the form:

|    CurrCmd ::= VAR <variables> IN [ <constraint> -> ] <command> END       (1)
|              | <command>                                                  (2)

   where <variables> is a near-var list (i.e., a list of variables with
   optional hints), <constraint> is a Juno constraint (hence, a conjunction),
   and <command> is a Juno command. The two forms of a "CurrCmd" are referred
   to in the rest of this interface as "case (1)" and "case (2)",
   respectively. The two cases are distinguished because the outer-most
   command in case (2) is NOT a projection.

   "CurrCmd"s talk to their clients using the "update" and "enable"
   methods.  "update" is called when the AST is changed.  "enable" is
   called whenever "DisableClients" is called, typically when one of the
   clients wishes to obtain an exclusive lock on the AST.

   It is an error to make modifications to the AST contained in the
   "CurrCmd.T", except through the procedures provided here.

   Some of the variables in the AST are considered to be points.  A point
   is a variable that is declared to be either near a pair "(h,v)", where
   "h" and "v" are both real-number constants, or near a position "(x,y) REL
   (p,q)" where "p" and "q" are identifiers representing points.  "CurrCmd"
   provides procedures for accessing and modifying the points.

   A "CurrCmd.T" also contains a scope, in which the current command is
   compiled. *)

INTERFACE CurrCmd;

IMPORT JunoAST, Atom, JunoScope, JunoPt;
IMPORT JunoValue, JunoRT;

TYPE
  T <: TPublic;
  TPublic = ROOT OBJECT
    codeValid := FALSE;
    skipify: BOOLEAN;
    slot := -1
  END;
  Real = JunoValue.Real;

(* If "t: T", the boolean "t.codeValid" is "TRUE" iff the compiled code in the
   global code table reflects "t"'s AST and "t.skipify". If "t.codeValid" is
   "TRUE", then "slot" is the index of the compiled code in "JunoRT.code_tbl"
   for the current command. *) 

(* ----------------- Creation / Replacement / Accessors -------------------- *)

PROCEDURE New(ast: JunoAST.Cmd; scp: JunoScope.T := NIL): T;
(* Create a new "CurrCmd.T" object.  If "scp" is "NIL", a new top-level
   scope is created. Requires "ast # NIL". *)

PROCEDURE GetAST(cc: T): JunoAST.Cmd;
PROCEDURE ChangeAST(cc: T; ast: JunoAST.Cmd);
(* Respectively return the current command associated with "cc", or set the
   current command associated with "cc" to "cmd". The latter procedure
   requires "ast # NIL". *)

(* Various components of the AST returned by "GetAST(cc)" may be extracted
   using the following procedures: *)

PROCEDURE GetVariables(ast: JunoAST.Cmd): JunoAST.NearVarList;
(* In case (1), return the <variables> section of "ast"; in case (2), return
   an emtpy "NearVarList". *)

PROCEDURE GetVariable(ast: JunoAST.T; v: JunoAST.Id): JunoAST.NearVarLink;
(* If the variable "v" is one of the variables in "GetVariables(ast)", then
   return its link. Otherwise, return NIL. *)

PROCEDURE GetConstraint(ast: JunoAST.Cmd): JunoAST.Formula;
(* In case (1), return the <constraint> section of "ast", or Juno "TRUE" if
   there isn't one; in case (2), return Juno "TRUE". *)

PROCEDURE GetCmd(ast: JunoAST.Cmd): JunoAST.Cmd;
(* Return the <command> section of "ast". *)

(* --------------------- Scope-Related Operations -------------------------- *)

PROCEDURE GetScope(cc: T): JunoScope.T;
PROCEDURE SetScope(cc: T; scp: JunoScope.T);
(* Observe / report "cc"'s top-level scope. *)

PROCEDURE NewDeclName(cc: T; prefix: TEXT; tryEmptySuffix := FALSE): TEXT;
(* Return a name of the form "prefix & N", where "N" is the smallest
   non-negative integer such that that name is not bound in "ccmd"'s scope.
   If "tryEmptySuffix" is TRUE and "prefix" is not defined in the current
   scope, then "prefix" is returned. *)

(* --------------------------- Modification -------------------------------- *)

PROCEDURE Skipify(ast: JunoAST.Cmd): JunoAST.Cmd;
(* Return a version of "ast" in which the portion of "ast" that would be
   returned by "GetCmd(ast)" is replaced by "SKIP". *)

PROCEDURE AddVariable(cc: T; name: JunoAST.Id;
    loc: JunoPt.T; near: JunoAST.Expr; frozen := FALSE);
(* In case (1), appends a new variable to the <variables> of "cc"; in case (2),
   changes "cc" to have the form of case (1) with a single new variable. In
   either case, the new variable is named "name", has value "loc", and hint
   "near". If "frozen = TRUE", then the new variable is asserted to be equal
   to its hint. Requires that "near # NIL" and that, in case (1), <variables>
   does not already contain a variable named "name". *)

PROCEDURE AddConstraint(cc: T; con: JunoAST.Constraint);
(* Conjoins the constraint "con" to the right-most disjunct of the
   <constraint> of "cc". If "cc" has no <constraint>, "con" is added as a new
   guard of the "cc" <command>. *)

PROCEDURE AddCommand(cc: T; cmd: JunoAST.Cmd);
(* Appends "cmd" to the <command> of "cc". If that <command> is "SKIP", then
   "SKIP" is replaced by "cmd". *)

PROCEDURE RemCommand(cc: T): BOOLEAN;
(* Destructively remove the last command from the <command> of "cc". This
   procedure is a no-op if the <command> is "SKIP". If the <command> is not a
   sequence, then it is replaced by "SKIP". Return TRUE iff the current
   command was changed. *)

PROCEDURE DoRel(cc: T; c, a, b: JunoAST.Id);
(* Destructively change the hint for the point "c" in the AST. If the hint was
   previously of the form "c ~= (x, y) REL (a, b)", then it is changed to an
   absolute hint of the form "c ~= (x', y')". Otherwise, it is changed to a
   hint of the form "c ~= (x, y) REL (a, b)", without changing its position
   (unless the points "a" and "b" have the same location, in which case this
   procedure is a no-op). The names "c", "a", and "b" must be local variables
   of the current AST. *)

PROCEDURE DoRel1(cc: T; c, a: JunoAST.Id);
(* Like "DoRel", but makes "c"'s hint relative to the single point "a", that
   is, of the form "R2.Plus(a, (x,y))". *)

(* ------------------------ Operations on Points --------------------------- *)

PROCEDURE PointLocation(cc: T; id: JunoAST.Id; VAR (*OUT*) h, v: Real):
  BOOLEAN;
(* Report the location of the point "id".  Return "FALSE" if the variable "id"
   does not exist, or is not a point; return "TRUE" otherwise. *)

PROCEDURE FreezePoint(cc: T; a: JunoAST.Id);
(* Toggle the "frozen" attribute of the point named "a" in "cc". *)

PROCEDURE IsFrozen(cc: T; a: JunoAST.Id): BOOLEAN;
(* Return TRUE iff "a" is the name of a frozen point in "cc". *)

PROCEDURE MovePoint(cc: T; a: JunoAST.Id; h, v: Real);
(* Move the point "a" in "cc" to (h,v) by changing its hint. The syntactic
   form of "a"'s hint will be preserved.  A checked run-time error occurs if
   "a" is not one of "cc"'s variables or if it does not have a hint. *)

TYPE PointProc = PROCEDURE(atom: Atom.T; READONLY pt: JunoPt.T);

PROCEDURE ForAllPoints(cc: T; p: PointProc);
(* Call the procedure "p" for each point. The procedure "p" will be invoked
   with the name and absolute location of each point-valued variable named in
   the current command "cc". *)

(* ------------------------ Folding Operations ----------------------------- *)

TYPE FoldKind = { Pred, Proc, ProcNoArgs };

EXCEPTION BadFoldArg(JunoAST.Id);
(* Indicates that the argument identifier was named as an explicit argument to
   the folded procedure, but no such variable exists in the current command. *)

PROCEDURE GetFoldArgs(cc: T): JunoAST.IdList;
(* Return the names of variables in the current command with no hints or with
   literal, point-valued hints. These are the variables that should become
   arguments to the folded procedure when the user has not specified any
   arguments explicitly. *)

PROCEDURE FoldByHeader(cc: T; hdr: JunoAST.PredHeader; kind := FoldKind.Proc):
  JunoAST.Decl RAISES {BadFoldArg};
(* Return a new predicate or procedure declaration named "name" corresponding
   to the current command. If "kind" is "Pred", then the current command's
   body is ignored, and a predicate definition is made; otherwise, a procedure
   definition is made. If "kind" is "ProcNoArgs", then the folded procedure
   has not arguments; all locals of the current command become locals of the
   folded procedure. Otherwise, "hdr.ins" provides the lits of arguments to
   the resulting predicate or procedure, and any locals of "cc" that are not
   in "hdr.ins" and that have absolute hints will automatically be converted
   to have relative hints. *)

PROCEDURE FoldByName(cc: T; name: JunoAST.Id; kind := FoldKind.Proc):
  JunoAST.Decl;
(* Like "FoldByHeader", but when "kind # ProcNoArgs", the arguments of the
   folded predicate or procedure are determined automatically using
   "GetFoldArgs" above. The parameters of the folded predicate or procedure
   are those locals that are either unhinted, or whose hints are literal point
   values. *)

PROCEDURE FoldAnim(cc: T; hdr: JunoAST.PredHeader;
  sliderPts: JunoAST.IdList): JunoAST.ProcDecl RAISES {BadFoldArg};
(* Produce the declaration of the current command "cc" folded as an animation
   with name and arguments determined from "hdr", and slider points
   "sliderPts". *)

PROCEDURE FoldAnimFrame(cc: T; hdr: JunoAST.PredHeader;
  sliderPts: JunoAST.IdList): JunoAST.ProcDecl;
(* Return the declaration for the "Frame" procedure resulting from folding
   the current command "cc" as an animation with name and arguments determined
   from "hdr", and slider points "sliderPts". The name of the resulting
   procedure will be the "hdr.name" concatenated with "Frame". If that
   procedure is already defined in the current command scope, then trailing
   digits are added as necessary to produce an unused name. *)

PROCEDURE FoldAnimCreator(cc: T; hdr: JunoAST.PredHeader;
  frameNm: JunoAST.Id): JunoAST.ProcDecl;
(* Return the declaration for the "Anim"-creation procedure resulting from
   folding the current command "cc" as an animation with name and arguments
   determined from "hdr". The name of the resulting procedure will be the
   "hdr.name" concatenated with "Anim". If that procedure is already defined
   in the current command scope, then trailing digits are added as necessary
   to produce an unused name. The resulting procedure returns an animation
   whose closure is for the procedure named "frameNm". *)

PROCEDURE FoldAnimCmd(cc: T; args: JunoAST.IdList; animProcNm: JunoAST.Id):
  JunoAST.Cmd;
(* Return the command that should become the current command when the current
   command "cc" is folded as an animation, "animProcNm" is the name of the
   procedure produced by "FoldAnimCreator", and "args" is the list of fold
   arguments. *)

(* ------------------------- Running / Updating ---------------------------- *)

TYPE
  RTError = RECORD
    errorMsg: TEXT;
    execRes: JunoRT.ExecRes
  END;

(* In the event of a run-time error, "errorMsg" is a descriptive error
   message, and "execRes" is the execution result produced by the Juno
   machine. *)

EXCEPTION
  CompileError(TEXT);
  RuntimeError(RTError);

PROCEDURE Run(cc: T; skipify: BOOLEAN): BOOLEAN
  RAISES {CompileError, RuntimeError};
(* Compile the current command in the current scope, and then execute it.
   If "skipify" is TRUE, apply "Skipify" to the current command before
   compiling it. Returns "TRUE" iff the current command was modified due to
   the hints of the variables being updated.

   Raises "CompileError" in the event of a compilation error; the argument of
   the exception is a descriptive error message. Raises "RuntimeError" in the
   event of a run-time error. *)

PROCEDURE UpdateHints(cc: T): BOOLEAN;
(* Update the hints for the variables in "cc" to reflect their current values
   in the oldest frame of the run-time stack. Requires that the indexes of the
   variables in that stack agree with the indexes in "cc". Returns "TRUE" iff
   any hints were updated. *)

END CurrCmd.
