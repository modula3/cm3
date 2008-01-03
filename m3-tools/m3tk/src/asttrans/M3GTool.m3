MODULE M3GTool;

(***************************************************************************)
(*                      Copyright (C) Olivetti 1989                        *)
(*                          All Rights reserved                            *)
(*                                                                         *)
(* Use and copy of this software and preparation of derivative works based *)
(* upon this software are permitted to any person, provided this same      *)
(* copyright notice and the following Olivetti warranty disclaimer are     *) 
(* included in any copy of the software or any modification thereof or     *)
(* derivative work therefrom made by any person.                           *)
(*                                                                         *)
(* This software is made available AS IS and Olivetti disclaims all        *)
(* warranties with respect to this software, whether expressed or implied  *)
(* under any law, including all implied warranties of merchantibility and  *)
(* fitness for any purpose. In no event shall Olivetti be liable for any   *)
(* damages whatsoever resulting from loss of use, data or profits or       *)
(* otherwise arising out of or in connection with the use or performance   *)
(* of this software.                                                       *)
(***************************************************************************)


IMPORT Text, StdIO, IO, IOErr, Err, Fmt, PathNameStream, PathName;
IMPORT M3Args, M3Conventions;
IMPORT M3AST, M3AST_AS, M3Context, M3CUnit;
IMPORT M3CImportASTrans;
IMPORT M3AST_FE_F;


IMPORT M3ASTDisplay, M3ASTWalk;

CONST 
  Version = "5-Feb-92";

VAR 
  tool_g: M3Args.T;

TYPE
  ContextClosure = M3Context.Closure OBJECT
    s: IO.Stream;
  OVERRIDES
    callback := DisplayGeneric;
  END;

PROCEDURE Get(): M3Args.T RAISES {}=
  BEGIN
    RETURN tool_g;
  END Get;

PROCEDURE Run(c: M3Context.T): INTEGER RAISES {}=
  BEGIN
    IF M3Args.Find(tool_g) THEN
      M3Context.ApplyToSet(c, NEW(ContextClosure, s := StdIO.Out()),
          M3CUnit.TypeSet{M3CUnit.Type.Interface_gen_ins,
                          M3CUnit.Type.Module_gen_ins});
      RETURN 0
    ELSE
      RETURN -1
    END;
  END Run;

PROCEDURE RunWithAST(
    c: M3Context.T;
    cu: M3AST_AS.Compilation_Unit;
    s: IO.Stream)
    RAISES {IO.Error}=
  BEGIN
    DisplayGeneric(NEW(ContextClosure, s := s),
        M3CUnit.Type.Interface, (* value unimportant *)
        NIL,
        cu);
  END RunWithAST;

PROCEDURE DisplayGeneric(
    cl: ContextClosure;
    ut: M3CUnit.Type;
    name: TEXT;
    cu: M3AST_AS.Compilation_Unit)
    RAISES {IO.Error}=
  BEGIN
    IF M3Conventions.PrimarySource IN cu.fe_status THEN
      IF M3Args.GetFlag(tool_g, SubstituteAP_Arg) THEN
        M3CImportASTrans.Set(cu, TRUE);
      END; 
      M3ASTDisplay.Nodes(M3CUnit.ToGenIns(cu, ut), cl.s);
    END; (* if *)
  END DisplayGeneric;


BEGIN
  tool_g := M3Args.New("m3generic", "Generic Unit Instantiation", Version);
  M3Args.RegisterFlag(tool_g, SubstituteAP_Arg, 
    "substitute actual parameters in unit body");
END M3GTool.
