MODULE M3CNormType;

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

IMPORT M3AST, M3AST_AS;

IMPORT M3AST_AS_F, M3AST_SM_F, M3AST_TM_F;

IMPORT SeqM3AST_AS_M3TYPE;



PROCEDURE Set(an: M3AST.NODE) RAISES {}=
  BEGIN
    TYPECASE an OF
    | M3AST_AS.Array_type(at) =>
        IF SeqM3AST_AS_M3TYPE.Length(at.as_indextype_s) <= 1 THEN
          at.sm_norm_type := at;
        ELSE
          VAR
            iter := SeqM3AST_AS_M3TYPE.NewIter(at.as_indextype_s);
            prev: M3AST_AS.Array_type := NIL;
            indextype: M3AST_AS.M3TYPE;
          BEGIN
            WHILE SeqM3AST_AS_M3TYPE.Next(iter, indextype) DO
              VAR new: M3AST_AS.Array_type := NEW(M3AST_AS.Array_type).init();
              BEGIN
                new.tmp_unit_id := at.tmp_unit_id;
                new.sm_norm_type := new;
                SeqM3AST_AS_M3TYPE.AddFront(new.as_indextype_s, indextype);
                IF prev # NIL THEN
                  prev.as_elementtype := new;
                ELSE
                  at.sm_norm_type := new;
                END; (* if *)
                prev := new;
              END;
            END; (* while *)
            prev.as_elementtype := at.as_elementtype;
          END;
        END; (* if *)
    ELSE
    END; (* typecase *)
  END Set;


BEGIN
END M3CNormType.
