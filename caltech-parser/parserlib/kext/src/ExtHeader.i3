(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id$ *)

INTERFACE ExtHeader;
IMPORT TextList;
IMPORT Rd;
TYPE
  T = RECORD
    sources, imports: TextList.T;
  END;
PROCEDURE Parse(from: Rd.T): T;
END ExtHeader.


  
