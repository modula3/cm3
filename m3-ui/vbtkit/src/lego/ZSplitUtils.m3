(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Jun 16 13:07:55 PDT 1992 by muller *)
(*      modified on Sun Jun 14 03:34:47 1992 by mhb *)

MODULE ZSplitUtils;

IMPORT Split, VBT, VBTClass, ZSplit;

PROCEDURE FindZChild (v: VBT.T): VBT.T =
  <* FATAL Split.NotAChild *>
  VAR
    ch := v;
    p  := v.parent;
  BEGIN
    WHILE (p # NIL) AND ((NOT IsZSplit(p)) OR (ch = Split.Pred(p, NIL))) DO
      ch := p;
      p := p.parent;
    END;
    IF p = NIL THEN RETURN NIL ELSE RETURN ch END;
  END FindZChild;

PROCEDURE IsZSplit (v: VBT.T): BOOLEAN =
  BEGIN
    TYPECASE (v) OF ZSplit.T => RETURN TRUE ELSE RETURN FALSE END
  END IsZSplit;

BEGIN
END ZSplitUtils.
