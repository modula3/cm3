(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

INTERFACE ObLibOnline;
IMPORT SynScan;

  PROCEDURE Setup();
  (* To be called once before any other use of this module. *)

  PROCEDURE RegisterScanner(scanner: SynScan.T);
  (* So we can set the prompts interactively. *)

END ObLibOnline.
