(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Jun 11 23:38:17 PDT 1993 by meehan     *)
(*      modified on Thu Feb  4 17:56:39 PST 1993 by mhb        *)
(*      modified on Tue Jun 16 13:09:00 PDT 1992 by muller     *)
(*      modified on Fri Mar 27 02:30:36 1992 by steveg     *)

MODULE ChoiceVBT;

IMPORT BooleanVBT, ButtonVBT, Filter, RefList, VBT;

REVEAL
  T = Public BRANDED OBJECT
        group: Group
      OVERRIDES
        init    := Init;
        discard := Discard
      END;

REVEAL
  Group = MUTEX BRANDED OBJECT
            selection: T         := NIL;
            members  : RefList.T := NIL
          END;

TYPE Link = REF RECORD v: T END;

PROCEDURE Init (v: T; btn: ButtonVBT.T; group: Group): T =
  BEGIN
    EVAL BooleanVBT.T.init (v, btn);
    VBT.PutProp (btn, NEW (Link, v := v));
    btn.action := Action;
    v.group := group;
    LOCK group DO group.members := RefList.Cons (v, group.members) END;
    RETURN v
  END Init;
  
PROCEDURE Discard (v: T) =
  PROCEDURE remove (VAR list: RefList.T) =
    BEGIN
      IF list = NIL THEN         (* skip *)
      ELSIF list.head = v THEN
        list := list.tail
      ELSE
        remove (list.tail)
      END
    END remove;
  BEGIN
    LOCK v.group DO
      remove (v.group.members);
      IF v.group.selection = v THEN v.group.selection := NIL END;
      v.group := NIL
    END;
    Filter.T.discard (v)
  END Discard;

PROCEDURE Action (btn: ButtonVBT.T; READONLY cd: VBT.MouseRec) =
  VAR cl: Link := VBT.GetProp (btn, TYPECODE (Link));
  BEGIN
    Put (cl.v);
    cl.v.callback (cd)
  END Action;

PROCEDURE Get (v: T): T =
  BEGIN
    RETURN Selection (v.group)
  END Get;

PROCEDURE Put (v: T) =
  BEGIN
    Select (v.group, v);
  END Put;

PROCEDURE Clear (v: T) =
  BEGIN
    Select (v.group, NIL);
  END Clear;

PROCEDURE Select (group: Group; new: T) =
  VAR old := Selection (group);
  BEGIN
    IF old # NIL THEN BooleanVBT.Put (old, FALSE) END;
    LOCK group DO group.selection := new END;
    IF new # NIL THEN BooleanVBT.Put (new, TRUE) END
  END Select;

PROCEDURE Selection (group: Group): T =
  BEGIN
    LOCK group DO RETURN group.selection END
  END Selection;

(* This procedure used to be exported, but I couldn't find any
   clients. No surprise. 
PROCEDURE Member (group: Group): T =
  BEGIN
    LOCK group DO
      IF group.memebrs = NIL THEN RETURN NIL END;
      RETURN group.members.head
    END
  END Member;
*)

BEGIN
END ChoiceVBT.
