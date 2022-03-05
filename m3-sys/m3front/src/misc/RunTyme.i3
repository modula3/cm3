(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* This interface provides handle on the runtime procedures
   that the compiler calls directly. *)

INTERFACE RunTyme;

IMPORT M3ID, Module, Procedure;

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
    CheckLoadTracedRef, CheckStoreTraced
(*peter*)
    ,LatchEHReg
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

END RunTyme.
