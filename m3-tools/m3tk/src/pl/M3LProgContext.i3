(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE M3LProgContext;

IMPORT M3Context, M3AST_AS, SeqM3AST_AS_Interface;

PROCEDURE Build(m: M3AST_AS.Module): M3Context.T RAISES {};
(* This procedure enters all the units that go to make up
the program identified by 'm' into a new context, and returns
that as result. It assumes that the pl_dependson_s has already
been built. *)

PROCEDURE BuildC(c: M3Context.T; name: TEXT := NIL): M3Context.T RAISES {};
(* First find the main program module in 'c' named 'name', or, if NIL, the 
first one on the list returned by 'M3LMain.Module'. Then compute the 
dependson relation, then call 'Build' to return an exact context. 
If there are no main program modules, return NIL. *)

PROCEDURE BuildPartial(
    c: M3Context.T;
    list: SeqM3AST_AS_Interface.T)
    : M3Context.T
    RAISES {};
(* This is a more general procedure than 'BuildC'. First it
finds those modules that export the interfaces denoted by 'list'.
It then computes the dependson relation for these and then builds
a context from this. If 'list' contained a single entry 'Main', this call
would be equivalent to 'BuildC(c, "Main"); *)

END M3LProgContext.
