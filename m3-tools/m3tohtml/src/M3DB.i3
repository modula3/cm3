(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Wed Apr 27 14:13:00 PDT 1994 by kalsow                   *)

INTERFACE M3DB;

IMPORT Rd, TextList;

PROCEDURE Open (path: TEXT);
PROCEDURE Dump (path: TEXT);

PROCEDURE DeleteUnit (path: TEXT);
PROCEDURE AddUnit (rd: Rd.T;  path: TEXT);

(* queries *)
PROCEDURE Imports     (interface: TEXT): TextList.T;
PROCEDURE Exports     (interface: TEXT): TextList.T;
PROCEDURE RevealsTo   (interface: TEXT): TextList.T;
PROCEDURE RevealsType (type: TEXT): TextList.T;
PROCEDURE DefinesType (type: TEXT): TextList.T;
PROCEDURE DefinesProc (proc: TEXT): TextList.T;
PROCEDURE Interface   (nm: TEXT): TextList.T;
PROCEDURE Module      (nm: TEXT): TextList.T;
PROCEDURE GenericIntf (nm: TEXT): TextList.T;
PROCEDURE GenericMod  (nm: TEXT): TextList.T;

END M3DB.


