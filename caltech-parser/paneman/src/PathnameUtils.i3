(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: PathnameUtils.i3,v 1.1.1.1 2001-09-19 11:40:10 wagner Exp $ *)

INTERFACE PathnameUtils;
IMPORT Pathname;
TYPE
  T = Pathname.T;

PROCEDURE SlashedPrefix(path: T): T;
PROCEDURE Complete(path: T): T;

END PathnameUtils. 
