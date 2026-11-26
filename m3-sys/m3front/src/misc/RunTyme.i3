(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* This interface provides handle on the runtime procedures
   that the compiler calls directly. *)

INTERFACE RunTyme;

IMPORT M3, M3ID, Module, Procedure, CG;

TYPE
  Hook = {
    CheckIsType, ScanTypecase,
    RaiseEx, ResumeRaiseEx, PushEFrame, PopEFrame,
    Concat, MultiCat,
    NewTracedObj, NewTracedRef, NewTracedArray,
    NewUntracedObj, NewUntracedRef, NewUntracedArray,
    DisposeRef, DisposeObj,
    Abort, AssertFailed, DebugMsg,
    TextLitInfo, TextLitGetChar, TextLitGetWideChar,
    TextLitGetChars, TextLitGetWideChars,
    CheckLoadTracedRef, CheckLoadTracedBlock, CheckStoreTraced, NoOp
  };

PROCEDURE Import ();
(* Import the standard interfaces containing runtime hooks. *)

PROCEDURE Bind (dest: Module.T;  VAR runtime: Module.T;  VAR id: M3ID.T);
(* bind the runtime interface as an import of 'dest' *)

PROCEDURE LookUpProc (h: Hook): Procedure.T;
(* return a handle on the procedure that implements hook 'h' *)

PROCEDURE Reset ();

PROCEDURE EmitCheckLoadTracedRef ();
PROCEDURE EmitCheckStoreTraced ();

PROCEDURE EmitCheckLoadTracedBlock (t: M3.Type;  n: CG.Val := NIL;
                                    stride: INTEGER := 0);
(* PRE: s0.A is the address of a block that a copy has just filled from
   global, heap or indirect storage.  Pop it, and emit the read barrier
   (RTHooks.CheckLoadTracedBlock) for the traced references in the block:
   one value of type 't' if 'n' is NIL, otherwise 'n' values of type 't',
   'stride' bytes apart.  No call if incremental collection is off or 't'
   holds no traced references. *)

PROCEDURE EmitCopy (t: M3.Type;  size: INTEGER;  overlap, barrier: BOOLEAN);
(* CG.Copy (size, overlap) of a value of type 't', followed, if 'barrier',
   by the read barrier on the destination.  A block copy loads every
   reference in the block, so when the source is global, heap or indirect
   storage each of them needs the barrier, as a single load would. *)

PROCEDURE EmitCopy_n (eltT: M3.Type;  eltPack: INTEGER;
                      overlap, barrier: BOOLEAN);
(* CG.Copy_n (eltPack, overlap) of elements of type 'eltT', followed, if
   'barrier', by the read barrier on the destination.  PRE: s2.A and s1.A
   are ForceStacked. *)

END RunTyme.
