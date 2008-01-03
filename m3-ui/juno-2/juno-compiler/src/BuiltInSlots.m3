(* Copyright (C) 1997, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Jan 27 18:48:22 PST 1997 by heydon                   *)

MODULE BuiltInSlots;

IMPORT JunoAST, JunoScope, Atom, Rd, Wr, Lex, Fmt, FloatMode, Thread;

VAR close, apply := -1;

PROCEDURE SlotFromName(scp: JunoScope.T; nm: TEXT): CARDINAL =
(* Return the index associated with the name "nm" in "scp", which must be
   bound to a procedure. *)
  VAR ent: JunoScope.Proc; BEGIN
    ent := JunoScope.Lookup(scp, Atom.FromText(nm));
    RETURN ent.index
  END SlotFromName;

PROCEDURE Init(scp: JunoScope.T) =
  BEGIN
    close := SlotFromName(scp, "CLOSE");
    apply := SlotFromName(scp, "APPLY")
  END Init;

PROCEDURE IsProcSlot(nm: JunoAST.QId; slot: INTEGER): BOOLEAN =
(* Return TRUE iff "nm" has annotated index "slot" and the annoated type
   corresponding to a user-defined procedure. *)
  BEGIN
    RETURN nm.index = slot AND nm.type = JunoAST.IdType.Proc
  END IsProcSlot;

PROCEDURE IsApplySlot(slot: CARDINAL): BOOLEAN =
  BEGIN RETURN slot = apply END IsApplySlot;

PROCEDURE IsCloseSlot(slot: CARDINAL): BOOLEAN =
  BEGIN RETURN slot = close END IsCloseSlot;

PROCEDURE IsApplyProc(nm: JunoAST.QId): BOOLEAN =
  BEGIN RETURN IsProcSlot(nm, apply) END IsApplyProc;

PROCEDURE IsCloseProc(nm: JunoAST.QId): BOOLEAN =
  BEGIN RETURN IsProcSlot(nm, close) END IsCloseProc;

PROCEDURE Save(wr: Wr.T) =
  <* FATAL Thread.Alerted, Wr.Failure *>
  BEGIN
    Wr.PutText(wr, Fmt.Int(close) & "\n");
    Wr.PutText(wr, Fmt.Int(apply) & "\n")
  END Save;

PROCEDURE Restore(rd: Rd.T) =
  <* FATAL FloatMode.Trap, Lex.Error, Rd.Failure, Thread.Alerted *>
  BEGIN
    close := Lex.Int(rd);
    apply := Lex.Int(rd);
    <* FATAL Rd.EndOfFile *> BEGIN
      IF Rd.GetChar(rd) # '\n' THEN <* ASSERT FALSE *> END
    END
  END Restore;

BEGIN
END BuiltInSlots.
