(* Copyright (C) 1992, Xerox Corporation.                      *)
(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the files COPYRIGHT and COPYRIGHT.extras for details.   *)
(*                                                             *)
(* Last Modified On Tue Nov  1 10:11:04 PST 1994 By kalsow     *)
(* Created by David Goldberg, goldberg@parc.xerox.com, 1992    *)

INTERFACE Tree;

IMPORT Solve;

CONST Brand = "Tree";

TYPE T = Solve.Tree;

PROCEDURE Equal (t, u: T): BOOLEAN;
PROCEDURE Copy (t: T): T;
PROCEDURE Compare (t, u: T): [-1 .. 1];
PROCEDURE Hash (t: T; lessThan: CARDINAL): CARDINAL;

END Tree.
