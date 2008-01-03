(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Jan 27 16:13:49 PST 1997 by heydon                       *)
(*      modified on Tue Feb 21 14:44:17 1995 by gnelson                      *)
(*      modified on Fri Aug  7 21:53:57 PDT 1992 by myers                    *)

INTERFACE JunoCompile;

(* Compile Juno expressions, predicates, functions, and procedures.

   Each of the compilation procedures in this interface takes a JunoAST.T to
   compile and a JunoScope.T "scope" that is used to resolve free variables.
   Each has the side-effect of decorating its argument AST, and each raises
   "Error" in the event of a compilation error.

   Compiling a Juno predicate, function, or procedure is a two-step process.
   Clients must first call "NewPred", "NewFunc", or "NewProc" to transform the
   declaration AST into a "JunoScope.Entity". They should then pass that
   entity to "PredDecl", "FuncDecl", or "ProcDecl", respectively, to install a
   bytestream in the run-time code table.

   These procedures are re-entrant; they use an internal mutex to guarantee
   serialization. *)

IMPORT JunoAST, JunoScope, JunoRT;
FROM JunoCompileErr IMPORT Error;

IMPORT Wr, Rd;

PROCEDURE Expr(
    expr: JunoAST.Expr;
    scp: JunoScope.T;
    nm: JunoAST.Id;
    VAR (*OUT*) val_slot: CARDINAL;
    pure := FALSE):
    JunoRT.ByteStream RAISES {Error};
(* Sets "val_slot" and returns the bytestream of a command compiled under
   "scp" that, when executed, stores the value of the expression "expr"
   in "JunoRT.value_tbl[val_slot]". "nm" is printed in debugging output for
   this compilation. If "pure" is "TRUE" and "expr" references a global
   variable or procedure, then raises "Error". *)

PROCEDURE PredDecl(nm: JunoAST.Id; pred: JunoScope.Pred; scp: JunoScope.T)
    RAISES {Error};
(* Compile the predicate with signature and body determined from "pred" using
   "scp" to resolve all its free variables, and place the resulting code in
   "JunoRT.code_tbl[pred.index]". "nm" is printed in debugging output for this
   compilation.

   By "compiling a predicate", we mean: 1) installing the normal form for the
   predicate body in the "JunoScope.Pred" entity corresponding to "nm" in
   "scp", and 2) compiling the query "(body)?()" -- where "body" is the
   predicate body -- with all IN arguments to the predicate assumed to be
   known variables. *)

PROCEDURE FuncDecl(nm: JunoAST.Id; func: JunoScope.Func; scp: JunoScope.T)
    RAISES {Error};
(* Compile the function with signature and body determined from "func" using
   "scp" to resolve all its free variables, and place the resulting code in
   "JunoRT.code_tbl[func.index]". "nm" is printed in debugging output for this
   compilation.

   By "compiling a function", we mean: 1) installing the normal form for the
   function body in the "JunoScope.Func" entity corresponding to "nm" in
   "scp", and 2) compiling the query "(body)?(res)" -- where "body" is the
   function body and "res" is its single OUT parameter -- with all IN
   arguments to the function assumed to be known variables. *)

PROCEDURE ProcDecl(nm: JunoAST.Id; proc: JunoScope.Proc; scp: JunoScope.T):
    JunoAST.Cmd RAISES {Error};
(* Compile the procedure with signature and body determined from "proc" using
   "scp" to resolve all its free variables, and place the resulting code in
   "JunoRT.code_tbl[proc.index]". "nm" is printed in debugging output for this
   compilation. Return the command produced by the front-end of the compiler
   that is to be fed into the back-end (assembler). *)

PROCEDURE SaveSlots(wr: Wr.T);
(* Write to "wr" the indexes of any internal "JunoRT" slots that are stored
   in the compiler or assembler. *)

PROCEDURE RestoreSlots(rd: Rd.T);
(* Read from "wr" the indexs stored by "SaveSlots", and set the internal
   slots to the values read. *)

END JunoCompile.
