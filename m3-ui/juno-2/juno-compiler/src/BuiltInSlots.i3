(* Copyright (C) 1997, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Jan 27 18:37:50 PST 1997 by heydon                   *)

INTERFACE BuiltInSlots;

(* Records the slots numbers for the built-in APPLY and CLOSE procedures.
   These are represented as user-defined procedures, but they are treated
   specially by the compiler and assembler. *)

IMPORT JunoAST, JunoScope, Rd, Wr;

PROCEDURE Init(scp: JunoScope.T);
(* Initialize the globals in this module from the scope "scp". This procedure
   must be called before any compilations/assemblies are performed. *)

PROCEDURE IsApplySlot(slot: CARDINAL): BOOLEAN;
PROCEDURE IsCloseSlot(slot: CARDINAL): BOOLEAN;
(* Return TRUE iff "slot" is the slot for the "APPLY" or "CLOSE" procedure,
   respectively. *)

PROCEDURE IsApplyProc(nm: JunoAST.QId): BOOLEAN;
PROCEDURE IsCloseProc(nm: JunoAST.QId): BOOLEAN;
(* Return TRUE iff "nm" is an annotated name for the "APPLY" or "CLOSE"
   procedure, respectively. *)

PROCEDURE Save(wr: Wr.T);
PROCEDURE Restore(rd: Rd.T);
(* Save/restore "close" and "apply" to/from "wr"/"rd". *)

END BuiltInSlots.
