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


MODULE M3CWordProcs;

IMPORT Text, TextRefTbl;

IMPORT M3AST_AS;

IMPORT M3AST_AS_F, M3AST_TM_F;


IMPORT M3CId, M3Assert, M3CExpsMisc;


PROCEDURE Enter(table: TextRefTbl.T; t: Text.T; w: T) RAISES {}=
  VAR
    new := NEW(REF T);
  BEGIN
    new^ := w;
    M3Assert.Check(NOT table.put(t, new));
  END Enter;


PROCEDURE Initialize(): TextRefTbl.T RAISES {}=
  VAR
    result := NEW(TextRefTbl.Default).init(64);
  BEGIN
    Enter(result, "Plus", T.Plus);
    Enter(result, "Times", T.Times);
    Enter(result, "Minus", T.Minus);
    Enter(result, "Divide", T.Divide);
    Enter(result, "Mod", T.Mod);
    Enter(result, "LT", T.LT);
    Enter(result, "LE", T.LE);
    Enter(result, "GT", T.GT);
    Enter(result, "GE", T.GE);
    Enter(result, "And", T.And);
    Enter(result, "Or", T.Or);
    Enter(result, "Xor", T.Xor);
    Enter(result, "Not", T.Not);
    Enter(result, "Shift", T.Shift);
    Enter(result, "LeftShift", T.Shift);
    Enter(result, "RightShift", T.RightShift);
    Enter(result, "Rotate", T.Rotate);
    Enter(result, "LeftRotate", T.Rotate);
    Enter(result, "RightRotate", T.RightRotate);
    Enter(result, "Extract", T.Extract);
    Enter(result, "Insert", T.Insert);
    RETURN result;
  END Initialize;


VAR
  table_g := Initialize();
  word_g: M3CId.T := NIL;


PROCEDURE IsWordCall(call: M3AST_AS.Call; VAR t: T): BOOLEAN RAISES {}=
  VAR
    defId: M3AST_AS.DEF_ID;
  BEGIN
    IF M3CExpsMisc.IsId(call.as_callexp, defId) AND
        ISTYPE(defId, M3AST_AS.Proc_id) THEN
      VAR
        hId: REFANY;
      BEGIN
        IF word_g = NIL THEN word_g := M3CId.Enter("Word") END;
        IF defId.tmp_unit_id.lx_symrep = word_g THEN
          IF table_g.get(M3CId.ToText(defId.lx_symrep), hId) THEN
            t := NARROW(hId, REF T)^;
            RETURN TRUE;
          ELSE
            RETURN FALSE;
          END; (* if *)
        ELSE
          RETURN FALSE;
        END; (* if *)
      END;
    ELSE
      RETURN FALSE;
    END; (* if *)
  END IsWordCall;


PROCEDURE ArgCount(t: T): CARDINAL RAISES {}=
  BEGIN
    CASE t OF
    | T.Plus => RETURN 2;
    | T.Times => RETURN 2;
    | T.Minus => RETURN 2;
    | T.Divide => RETURN 2;
    | T.Mod => RETURN 2;
    | T.LT => RETURN 2;
    | T.LE => RETURN 2;
    | T.GT => RETURN 2;
    | T.GE => RETURN 2;
    | T.And => RETURN 2;
    | T.Or => RETURN 2;
    | T.Xor => RETURN 2;
    | T.Not => RETURN 1;
    | T.Shift => RETURN 2;
    | T.RightShift => RETURN 2;
    | T.Rotate => RETURN 2;
    | T.RightRotate => RETURN 2;
    | T.Extract => RETURN 3;
    | T.Insert => RETURN 4;
    END;
  END ArgCount;


BEGIN
END M3CWordProcs.
