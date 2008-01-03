(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Sat Jun  4 20:10:46 PDT 1994 by najork                   *)
(*       Created on Tue Jan 20 17:00:00 PST 1994 by najork                   *)


INTERFACE ObMarkerGO;

IMPORT ObProtoLoader;

PROCEDURE SetupPackage ();
PROCEDURE SetupModule (loader : ObProtoLoader.T);

END ObMarkerGO.
