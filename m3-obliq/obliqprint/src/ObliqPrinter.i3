
(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

INTERFACE ObliqPrinter;
IMPORT SynWr, ObTree, ObValue, ObLib;

(* Program interface to print Obliq terms and values. *)

  PROCEDURE PackageSetup();
  (* To be called at least once before any other use of the obliqprint 
     package. It calls all the required PackageSetup routines of other packages,
     including Obliq.PackageSetup for obliqrt-ip. *)

  PROCEDURE PrintTerm(term: ObTree.Term; libEnv: ObLib.Env;
     swr: SynWr.T:=NIL; depth:=10);
  (* Prettyprint a term to swr (NIL = standard output). *)

  PROCEDURE PrintVal(val: ObValue.Val; libEnv: ObLib.Env;
     swr: SynWr.T:=NIL; depth:=10);
  (* Prettyprint a value to swr (NIL = standard output). *)

  PROCEDURE PrintText(text: TEXT; swr: SynWr.T:=NIL);
  (* Print a text to swr (NIL = standard output), e.g. "\n". *)

  PROCEDURE PrintFlush(swr: SynWr.T:=NIL);
  (* Flush swr (NIL = standard output). *)

END ObliqPrinter.
