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
(**)
(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE M3CBackEnd_C_cc;

IMPORT TextRefTbl;

VAR
 table_g: TextRefTbl.T := NEW(TextRefTbl.Default).init(10);

TYPE R = REF TargetInitProc;

PROCEDURE RegisterTarget(t: TEXT; p: TargetInitProc) RAISES {}=
  VAR r: R;
  BEGIN
    r := NEW(R); r^ := p;
    EVAL table_g.put(t, r);
  END RegisterTarget;

PROCEDURE LookupTarget(
    t: TEXT; 
    VAR (*out*) rp: REF TargetInitProc): BOOLEAN RAISES {}=
  VAR
    id: REFANY;
  BEGIN
    IF table_g.get(t, id) THEN
      rp := NARROW(id, R);
      RETURN TRUE;
    ELSE
      RETURN FALSE
    END; (* if *)
  END LookupTarget;

REVEAL
  Iter = BRANDED REF TextRefTbl.Iterator;

PROCEDURE NewIter(): Iter RAISES {}=
  VAR iter := NEW(Iter);
  BEGIN
    iter^ := table_g.iterate();
    RETURN iter;
  END NewIter;

PROCEDURE Next(iter: Iter; VAR (*out*) t: TEXT; 
    VAR (*out*) rp: REF TargetInitProc): BOOLEAN RAISES {}=
  VAR
    ra: REFANY;
  BEGIN
    IF iter^.next(t, ra) THEN
      rp := NARROW(ra, R); RETURN TRUE;
    ELSE
      RETURN FALSE
    END;
  END Next;

BEGIN

END M3CBackEnd_C_cc.
