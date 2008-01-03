(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
 
(* Last modified on Fri Jan 18  9:10:34 PST 1991 by mjordan    *)

INTERFACE MPropertyF;

FROM MProperty IMPORT Set;

(* Without the 1990 revisions, we need to expose the method procedures to 
   subtypes.
*)

PROCEDURE Put(ps: Set; r: REFANY);
PROCEDURE Remove(ps: Set; tc: CARDINAL);
PROCEDURE Get(ps: Set; tc: CARDINAL): REFANY;
PROCEDURE RemoveSub(ps: Set; tc: CARDINAL);
PROCEDURE GetSub(ps: Set; tc: CARDINAL): REFANY;

END MPropertyF.
