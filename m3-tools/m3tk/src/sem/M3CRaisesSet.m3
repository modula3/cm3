MODULE M3CRaisesSet;

(***************************************************************************)
(*                      Copyright (C) Olivetti 1989                        *)
(*                          All Rights reserved                            *)
(*                                                                         *)
(* Use and copy of this software and preparation of derivative works based *)
(* upon this software are permitted to any person, provided this same      *)
(* copyright notice and the following Olivetti warranty disclaimer are     *) 
(* included in any copy of the software or any modification thereof or     *)
(* derivative work therefrom made by any person.                           *)
(*                                                                         *)
(* This software is made available AS IS and Olivetti disclaims all        *)
(* warranties with respect to this software, whether expressed or implied  *)
(* under any law, including all implied warranties of merchantibility and  *)
(* fitness for any purpose. In no event shall Olivetti be liable for any   *)
(* damages whatsoever resulting from loss of use, data or profits or       *)
(* otherwise arising out of or in connection with the use or performance   *)
(* of this software.                                                       *)
(***************************************************************************)

(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

IMPORT M3AST_AS,  M3AST_SM;

IMPORT M3AST_AS_F, M3AST_SM_F;

IMPORT SeqM3AST_AS_Qual_used_id;

(* The algorithm used is only suitable for small RAISES lists but then most
RAISES lists are small. It allows duplication in RAISES lists *)

TYPE
  DefIdList = RECORD
    array: ARRAY [0..15] OF RECORD defId: M3AST_AS.DEF_ID; mark: CARDINAL END;
    count: CARDINAL;
    next: REF DefIdList;
  END;


TYPE
  InList = {Nil, Absent, PresentSameMark, PresentOtherMark};

VAR (* CONST *)
  null_raisees_some_g := NEW(M3AST_AS.Raisees_some).init();

PROCEDURE Check(
    defId: M3AST_SM.DEF_ID_UNSET;
    mark: CARDINAL;
    VAR list: DefIdList)
    : InList
    RAISES {}=
  BEGIN
    IF defId = NIL THEN RETURN InList.Nil END;
    FOR i := 0 TO list.count - 1 DO
      WITH entry = list.array[i] DO
        IF entry.defId = defId THEN
          IF entry.mark = mark THEN
            RETURN InList.PresentSameMark;
          ELSE
            entry.mark := mark;
            RETURN InList.PresentOtherMark;
          END;
        END;
      END;
    END;
    IF list.count = NUMBER(list.array) THEN
      IF list.next = NIL THEN 
        list.next := NEW(REF DefIdList, count := 0, next := NIL) 
      END;
      RETURN Check(defId, mark, list.next^);
    ELSE
      WITH entry = list.array[list.count] DO
        entry.defId := defId;
        entry.mark := mark;
      END;
      INC(list.count);
      RETURN InList.Absent;
    END;
  END Check;


PROCEDURE Compare(xr1, xr2: M3AST_AS.RAISEES_NULL): Comparison RAISES {}=
  VAR r1, r2: M3AST_AS.Raisees_some;
  BEGIN
    IF xr1 = NIL THEN r1 := null_raisees_some_g
    ELSIF ISTYPE(xr1, M3AST_AS.Raisees_any) THEN r1 := NIL;
    ELSE r1 := xr1
    END;
    IF xr2 = NIL THEN r2 := null_raisees_some_g
    ELSIF ISTYPE(xr2, M3AST_AS.Raisees_any) THEN r2 := NIL;
    ELSE r2 := xr2
    END;
 
    IF r1 # NIL AND r2 # NIL THEN
      VAR
        dl: DefIdList;
        r: M3AST_AS.Raisees_some;
        iter: SeqM3AST_AS_Qual_used_id.Iter;
        qualId: M3AST_AS.Qual_used_id;
        count1, count2 := 0;
      BEGIN
        dl.next := NIL;
        dl.count := 0;
        FOR i := 1 TO 2 DO
          IF i = 1 THEN r := r1 ELSE r := r2 END;
          iter := SeqM3AST_AS_Qual_used_id.NewIter(r.as_raisees_s);
          WHILE SeqM3AST_AS_Qual_used_id.Next(iter, qualId) DO
            WITH inList = Check(qualId.as_id.sm_def, i, dl) DO
              IF i = 1 THEN
                IF inList = InList.Absent THEN INC(count1) END;
              ELSE
                CASE inList OF
                | InList.Absent =>
                    INC(count2);
                    IF count1 = 0 THEN
                      RETURN Comparison.ProperSubSet;
                    END;
                | InList.PresentOtherMark =>
                    DEC(count1);
                    IF count1 = 0 AND count2 > 0 THEN
                      RETURN Comparison.ProperSubSet
                    END;
                ELSE
                END;
              END;
            END;
          END;
        END;
        IF count1 = 0 THEN
          RETURN Comparison.Equal;
        ELSIF count2 = 0 THEN
          RETURN Comparison.ProperSuperSet;
        ELSE
          RETURN Comparison.Disjoint;
        END;
      END;
    ELSIF r1 = NIL AND r2 = NIL THEN
      (* neither has raises clause *)
      RETURN Comparison.Equal;
    ELSE
      (* only one has raises clause; the one without the raises clause is
       a superset of the other (because no raises clause => raises set of
       all exceptions) *)
      IF r1 = NIL THEN
        RETURN Comparison.ProperSuperSet;
      ELSE
        RETURN Comparison.ProperSubSet;
      END; (* if *)
    END; (* if *)
  END Compare;


BEGIN
END M3CRaisesSet.
