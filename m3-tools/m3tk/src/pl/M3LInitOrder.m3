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

MODULE M3LInitOrder;

IMPORT M3Context, M3CUnit, M3AST_AS;

IMPORT M3AST_AS_F, M3AST_SM_F, M3AST_PL_F;

IMPORT SeqM3AST_AS_Module;

TYPE 
  Closure = M3Context.Closure BRANDED OBJECT
    md: MDCallback;
    unitIx, iter_unitIx: INTEGER;
    units: REF ARRAY OF M3AST_AS.Module;
  END;

REVEAL
  T = Public BRANDED OBJECT
    cl: Closure;
  OVERRIDES
    init := Init;
    next := Next;
    reset := Reset;
  END;

CONST
  DoneST = 2; InProgressST = 1; NotDoneST = 0;

PROCEDURE Init(t: T; c: M3Context.T; main: M3AST_AS.Compilation_Unit;
    md: MDCallback := NIL): T=
  VAR
    cl := NEW(Closure, md := md, callback := FillIn);
  BEGIN
    AllocateArray(cl, c);
    <*FATAL ANY*> BEGIN
      M3Context.Apply(c, cl);
    END;
    cl.unitIx := 0;
    AddDepends(cl, main.as_root, NIL);
    t.cl := cl;
    RETURN t;
  END Init;

PROCEDURE AllocateArray(cl: Closure; c: M3Context.T) RAISES {}=
  VAR tcl := NEW(Closure, unitIx := 0, callback := Count);
  BEGIN
    <*FATAL ANY*> BEGIN
      M3Context.Apply(c, tcl);
    END;
    IF tcl.unitIx = 0 THEN RETURN END;
    cl.units := NEW(REF ARRAY OF M3AST_AS.Module, tcl.unitIx);
  END AllocateArray;

PROCEDURE Count(cl: Closure; ut: M3CUnit.Type; <*UNUSED*> name: TEXT; 
    <*UNUSED*> cu: M3AST_AS.Compilation_Unit) RAISES {}=
  BEGIN
    IF ut = M3CUnit.Type.Module THEN INC(cl.unitIx) END;
  END Count;

PROCEDURE FillIn(cl: Closure; ut: M3CUnit.Type; <*UNUSED*> name: TEXT; 
    cu: M3AST_AS.Compilation_Unit) RAISES {}=
  BEGIN
    IF ut = M3CUnit.Type.Module THEN 
      cl.units[cl.unitIx] := cu.as_root;
      INC(cl.unitIx);
      NARROW(cu.as_root, M3AST_AS.Module).pl_tmp_init_status := NotDoneST;
    END;
  END FillIn;

PROCEDURE AddDepends(cl: Closure; m, dm: M3AST_AS.Module) RAISES {}=
  VAR
    iter: SeqM3AST_AS_Module.Iter;
    tm: M3AST_AS.Module;
  BEGIN
    IF InProgress(m) THEN
      (* cycle *)
      IF cl.md # NIL THEN cl.md.callback(dm, m); END; (* if *)
    ELSE
      SetInProgress(m);
      iter := SeqM3AST_AS_Module.NewIter(m.pl_dependson_s);
      WHILE SeqM3AST_AS_Module.Next(iter, tm) DO
        IF NOT Done(tm) THEN
          AddDepends(cl, tm, m);
        END; (* if *)
      END; (* while *)
      cl.units[cl.unitIx] := m; INC(cl.unitIx);
      SetDone(m);
    END; (* if *)
  END AddDepends;

<*INLINE*> PROCEDURE InProgress(m: M3AST_AS.Module): BOOLEAN RAISES {}=
  BEGIN
    RETURN m.pl_tmp_init_status = InProgressST;
  END InProgress;

<*INLINE*> PROCEDURE SetInProgress(m: M3AST_AS.Module) RAISES {}=
  BEGIN
    m.pl_tmp_init_status := InProgressST;
  END SetInProgress;

<*INLINE*> PROCEDURE SetDone(m: M3AST_AS.Module) RAISES {}=
  BEGIN
    m.pl_tmp_init_status := DoneST;
  END SetDone;

<*INLINE*> PROCEDURE Done(m: M3AST_AS.Module): BOOLEAN RAISES {}=
  BEGIN
    RETURN m.pl_tmp_init_status = DoneST;
  END Done;

PROCEDURE Next(
    t: T; 
    VAR (*out*) cu: M3AST_AS.Compilation_Unit
    ): BOOLEAN RAISES {}=
  BEGIN
    IF t.cl.iter_unitIx >= t.cl.unitIx THEN 
      RETURN FALSE
    ELSE
      cu := t.cl.units[t.cl.iter_unitIx].sm_comp_unit;
      INC(t.cl.iter_unitIx);
      RETURN TRUE;
    END;
  END Next;

PROCEDURE Reset(t: T)=
  BEGIN
    t.cl.iter_unitIx := 0; 
  END Reset;

BEGIN
END M3LInitOrder.
