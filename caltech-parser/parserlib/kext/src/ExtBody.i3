(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: ExtBody.i3,v 1.1.1.1 2001-09-19 11:40:10 wagner Exp $ *)

INTERFACE ExtBody;
IMPORT LoadSpec;
IMPORT TextSubs;
IMPORT Rd;
TYPE
  T = TextSubs.T;
PROCEDURE Parse(from: Rd.T; READONLY spec: LoadSpec.Info): T;
END ExtBody. 
