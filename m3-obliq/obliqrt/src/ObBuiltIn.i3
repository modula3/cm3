(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(*                                                                           *)
(* Parts Copyright (C) 1997, Columbia University                             *)
(* All rights reserved.                                                      *)
(*
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Fri Sep 26 12:10:32 1997
 *)

INTERFACE ObBuiltIn;
IMPORT SynWr, SynLocation, ObValue, Thread, WorkerPool, RegEx;

  PROCEDURE Setup();
  (* To be called before any other use of this module. *)

(* ============ "net" package ============ *)

  PROCEDURE NetObjectWho(valObj: ObValue.ValObj; loc: SynLocation.T)
    : ObValue.Val RAISES {ObValue.Exception};

  PROCEDURE NetEngineWho(remObj: ObValue.RemEngine; loc: SynLocation.T)
    : ObValue.Val RAISES {ObValue.Exception};

  PROCEDURE NetExport(name, server: TEXT; valObj: ObValue.ValObj; 
    loc: SynLocation.T) RAISES {ObValue.Exception};

  PROCEDURE NetImport(name, server: TEXT;
    loc: SynLocation.T): ObValue.Val RAISES {ObValue.Exception};

  PROCEDURE NetExportEngine(name, server: TEXT; arg: ObValue.Val; 
    loc: SynLocation.T) RAISES {ObValue.Exception};

  PROCEDURE NetImportEngine(name, server: TEXT;
    loc: SynLocation.T): ObValue.Val RAISES {ObValue.Exception};

(* ============ "replica" package ============ *)

  PROCEDURE ReplicaAcquireLock(valObj: ObValue.ValObj; 
                               loc: SynLocation.T) : ObValue.Val
    RAISES {ObValue.Exception};

  PROCEDURE ReplicaReleaseLock(valObj: ObValue.ValObj; 
                               loc: SynLocation.T) : ObValue.Val
    RAISES {ObValue.Exception};

  PROCEDURE ReplicaSetSiteName(name: TEXT; loc: SynLocation.T) : ObValue.Val
    RAISES {ObValue.Exception};

  PROCEDURE ReplicaSetDefaultSequencer(host, name: TEXT := NIL;
                                       loc: SynLocation.T) : ObValue.Val
    RAISES {ObValue.Exception};

  PROCEDURE ReplicaNotify(valObj: ObValue.Val; 
                          notifyObj: ObValue.ValObj; 
                          loc: SynLocation.T): ObValue.Val
    RAISES {ObValue.Exception};

  PROCEDURE ReplicaCancelNotifier(notifier: ObValue.Val; 
                                  loc: SynLocation.T)
    RAISES {ObValue.Exception};

(* ============ "thread" package ============ *)

  TYPE
    ValMutex =
      ObValue.ValAnything BRANDED "ObBuiltIn.ValMutex" OBJECT
        mutex: Thread.Mutex;
      OVERRIDES Is := IsMutex; Copy := CopyMutex;
      END;
    ValCondition =
      ObValue.ValAnything BRANDED "ObBuiltIn.ValCondition" OBJECT
        condition: Thread.Condition;
      OVERRIDES Is := IsCondition; Copy := CopyCondition;
      END;
    ValThread =
      ObValue.ValAnything BRANDED "ObBuiltIn.ValThread" OBJECT
        thread: Thread.T;
        joinedMu: Thread.Mutex;
        joined: BOOLEAN;
      OVERRIDES Is := IsThread;
      END;
    ValPool = 
      ObValue.ValAnything BRANDED "ObBuiltIn.ValPool" OBJECT
        pool: WorkerPool.T;
      OVERRIDES Is := IsPool;
      END;
  
  PROCEDURE IsMutex(self: ValMutex; other: ObValue.ValAnything): BOOLEAN;
  PROCEDURE IsCondition(self: ValCondition; 
                        other: ObValue.ValAnything): BOOLEAN;
  PROCEDURE IsThread(self: ValThread; other: ObValue.ValAnything): BOOLEAN;
  PROCEDURE IsPool(self: ValPool; other: ObValue.ValAnything): BOOLEAN;

  PROCEDURE CopyMutex(self: ObValue.ValAnything; tbl: ObValue.Tbl;
    loc: SynLocation.T): ObValue.ValAnything RAISES {ObValue.Error};
    (* Creates a new mutex *)
  PROCEDURE CopyCondition(self: ObValue.ValAnything; tbl: ObValue.Tbl;
                          loc: SynLocation.T): ObValue.ValAnything RAISES {ObValue.Error};
    (* Creates a new condition *)

  PROCEDURE ForkThread(fun: ObValue.ValFun; stackSize: INTEGER; 
                       swr: SynWr.T; loc: SynLocation.T): ValThread;
    (* Creates a new thread from a procedure of no arguments. 
       A zero stackSize is converted to a default stackSize. *)

  PROCEDURE JoinThread(threadVal: ValThread; loc: SynLocation.T): ObValue.Val 
    RAISES {ObValue.Error, ObValue.Exception};
    (* Join a thread and report its result. *)

(* ============ "regex" package ============ *)

  TYPE
    ValPattern = ObValue.ValAnything BRANDED "ObBuiltIn.ValPattern" OBJECT
      pattern: RegEx.Pattern;
    OVERRIDES
      Is := IsPattern; 
      Copy := ObValue.CopyId;
    END;
    
  VAR regexError: ObValue.ValException;

  PROCEDURE IsPattern(self: ValPattern; 
                      other: ObValue.ValAnything): BOOLEAN;

(* ============ "reflect" package ============ *)
  
  VAR reflectError: ObValue.ValException;

END ObBuiltIn.
