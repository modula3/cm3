(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: PSReader.i3,v 1.2 2001-09-19 15:30:31 wagner Exp $ *)

INTERFACE PSReader;
IMPORT Rd;
IMPORT CacheDrawContext;
TYPE
  T = CacheDrawContext.T;

PROCEDURE New(rd: Rd.T := NIL; captureResDPI := 720): T;
  (* the implementation may or may not be a complete postscript
     interpreter. Use at your own risk! *)

END PSReader.
