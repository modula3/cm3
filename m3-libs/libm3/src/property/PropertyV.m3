(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
 
(* Last modified on Fri Jan 18  9:16:52 PST 1991 by mjordan    *)

MODULE PropertyV;

IMPORT Property, PropertyF;


PROCEDURE Put(VAR ps: Set; r: REFANY)=
  BEGIN
    IF ps = NIL THEN ps := Property.New() END;
    PropertyF.Put(ps, r);
  END Put;


PROCEDURE Remove(VAR ps: Set; tc: CARDINAL)=
  BEGIN
    IF ps # NIL THEN
      PropertyF.Remove(ps, tc);
    END;
  END Remove;


PROCEDURE Get(ps: Set; tc: CARDINAL): REFANY=
  BEGIN
    IF ps # NIL THEN
      RETURN PropertyF.Get(ps, tc);
    ELSE
      RETURN NIL;
    END;
  END Get;

PROCEDURE RemoveSub(VAR ps: Set; tc: CARDINAL)=
  BEGIN
    IF ps # NIL THEN
      PropertyF.RemoveSub(ps, tc);
    END;
  END RemoveSub;


PROCEDURE GetSub(ps: Set; tc: CARDINAL): REFANY=
  BEGIN
    IF ps # NIL THEN
      RETURN PropertyF.GetSub(ps, tc);
    ELSE
      RETURN NIL;
    END;
  END GetSub;

BEGIN
END PropertyV.
