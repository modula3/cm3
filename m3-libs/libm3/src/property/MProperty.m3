(* Copyright (C) 1991, 1992, Digital Equipment Corporation                   *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
 
(* Last modified on Mon Jun 29 22:11:47 PDT 1992 by muller                   *)
(*      modified on Fri Jan 18  9:11:13 PST 1991 by mjordan                  *)

MODULE MProperty EXPORTS MProperty, MPropertyF;

IMPORT Thread, PropertyF;

REVEAL
  Set = Set_public BRANDED OBJECT
  OVERRIDES
    put := Put; get := Get; remove := Remove;
  END;

PROCEDURE New(): Set=
  BEGIN
    RETURN NEW(Set, m := NEW(Thread.Mutex));
  END New;

PROCEDURE Put(ps: Set; r: REFANY) =
  BEGIN
    LOCK ps.m DO PropertyF.Put(ps, r); END;
  END Put;

PROCEDURE Remove(ps: Set; tc: CARDINAL) =
  BEGIN
    LOCK ps.m DO PropertyF.Remove(ps, tc) END;
  END Remove;

PROCEDURE Get(ps: Set; tc: CARDINAL): REFANY =
  BEGIN
    LOCK ps.m DO RETURN PropertyF.Get(ps,tc); END;
  END Get;

PROCEDURE RemoveSub(ps: Set; tc: CARDINAL) =
  BEGIN
    LOCK ps.m DO PropertyF.RemoveSub(ps, tc) END;
  END RemoveSub;

PROCEDURE GetSub(ps: Set; tc: CARDINAL): REFANY =
  BEGIN
    LOCK ps.m DO RETURN PropertyF.GetSub(ps,tc); END;
  END GetSub;

BEGIN
END MProperty.
