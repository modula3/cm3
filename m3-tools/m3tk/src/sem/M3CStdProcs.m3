MODULE M3CStdProcs;

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

IMPORT Text, TextRefTbl;

IMPORT M3AST_AS;

IMPORT M3AST_AS_F, M3AST_SM_F, M3AST_TM_F;


IMPORT M3Context, M3CId, M3Assert;


VAR
  table_g: TextRefTbl.T;


PROCEDURE IsStandardUnit(unitId: M3AST_AS.UNIT_ID): BOOLEAN RAISES {}=
  BEGIN
    RETURN NARROW(unitId.sm_spec, M3AST_AS.UNIT).sm_comp_unit =
        M3Context.Standard();
  END IsStandardUnit;


PROCEDURE IsStandardCall(
    call: M3AST_AS.Call;
    VAR pf: T)
    : BOOLEAN
    RAISES {}=
  BEGIN
    TYPECASE call.as_callexp OF
    | M3AST_AS.Exp_used_id(expUsedId) =>
        VAR
          defId := expUsedId.vUSED_ID.sm_def;
          hashId: REFANY;
        BEGIN
          IF defId # NIL AND ISTYPE(defId, M3AST_AS.Proc_id) AND
              IsStandardUnit(defId.tmp_unit_id) THEN
            IF table_g.get(M3CId.ToText(defId.lx_symrep), hashId) THEN
              pf := NARROW(hashId, REF T)^;
              RETURN TRUE;
            ELSE
              M3Assert.Fail();
              <*ASSERT FALSE*>
            END; (* if *)
          ELSE
            RETURN FALSE;
          END; (* if *)
        END;
    ELSE
      RETURN FALSE;
    END; (* if *)
  END IsStandardCall;


PROCEDURE IsStandard(id: M3AST_AS.Proc_id; VAR pf: T): BOOLEAN RAISES {}=
  VAR
    hashId: REFANY;
  BEGIN
    IF IsStandardUnit(id.tmp_unit_id) THEN
      IF table_g.get(M3CId.ToText(id.lx_symrep), hashId) THEN
        pf := NARROW(hashId, REF T)^;
        RETURN TRUE;
      ELSE
        M3Assert.Fail();
        <*ASSERT FALSE*>
      END; (* if *)
    ELSE
      RETURN FALSE;
    END; (* if *)
  END IsStandard;


PROCEDURE Enter(s: Text.T; pf: T) RAISES {}=
  VAR
    new := NEW(REF T);
  BEGIN
    new^ := pf;
    EVAL table_g.put(s, new);
  END Enter;


PROCEDURE Initialise() RAISES {}=
  BEGIN
    table_g := NEW(TextRefTbl.Default).init(32);
    Enter("NEW", T.New);
    Enter("INC", T.Inc);
    Enter("DEC", T.Dec);
    Enter("DISPOSE", T.Dispose);
    Enter("ABS", T.Abs);
    Enter("FLOAT", T.Float);
    Enter("FLOOR", T.Floor);
    Enter("CEILING", T.Ceiling);
    Enter("ROUND", T.Round);
    Enter("TRUNC", T.Trunc);
    Enter("MAX", T.Max);
    Enter("MIN", T.Min);
    Enter("ORD", T.Ord);
    Enter("VAL", T.Val);
    Enter("NUMBER", T.Number);
    Enter("FIRST", T.First);
    Enter("LAST", T.Last);
    Enter("TYPECODE", T.TypeCode);
    Enter("NARROW", T.Narrow);
    Enter("ISTYPE", T.IsType);
    Enter("BITSIZE", T.BitSize);
    Enter("BYTESIZE", T.ByteSize);
    Enter("ADRSIZE", T.AdrSize);
    Enter("LOOPHOLE", T.Loophole);
    Enter("ADR", T.Adr);
    Enter("SUBARRAY", T.Subarray);
  END Initialise;


BEGIN
  Initialise();
END M3CStdProcs.
