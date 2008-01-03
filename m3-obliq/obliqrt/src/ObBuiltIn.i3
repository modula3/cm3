(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

INTERFACE ObBuiltIn;
IMPORT SynLocation, ObValue, Thread, WorkerPool;

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

  PROCEDURE ReplicaSetNodeName(name: TEXT; loc: SynLocation.T) : ObValue.Val
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
      ObValue.ValAnything BRANDED OBJECT
        mutex: Thread.Mutex;
      OVERRIDES Is := IsMutex; Copy := CopyMutex;
      END;
    ValCondition =
      ObValue.ValAnything BRANDED OBJECT
        condition: Thread.Condition;
      OVERRIDES Is := IsCondition; Copy := CopyCondition;
      END;
    ValThread =
      ObValue.ValAnything BRANDED OBJECT
        thread: Thread.T;
        joinedMu: Thread.Mutex;
        joined: BOOLEAN;
      OVERRIDES Is := IsThread;
      END;
    ValPool = 
      ObValue.ValAnything BRANDED OBJECT
        pool: WorkerPool.T;
      OVERRIDES Is := IsPool;
      END;
  
  PROCEDURE IsMutex(self: ValMutex; other: ObValue.ValAnything): BOOLEAN;
  PROCEDURE IsCondition(self: ValCondition; other: ObValue.ValAnything): BOOLEAN;
  PROCEDURE IsThread(self: ValThread; other: ObValue.ValAnything): BOOLEAN;
  PROCEDURE IsPool(self: ValPool; other: ObValue.ValAnything): BOOLEAN;

  PROCEDURE CopyMutex(self: ObValue.ValAnything; tbl: ObValue.Tbl;
    loc: SynLocation.T): ObValue.ValAnything RAISES {ObValue.Error};
    (* Creates a new mutex *)
  PROCEDURE CopyCondition(self: ObValue.ValAnything; tbl: ObValue.Tbl;
    loc: SynLocation.T): ObValue.ValAnything RAISES {ObValue.Error};
    (* Creates a new condition *)

  PROCEDURE ForkThread(fun: ObValue.ValFun; stackSize: INTEGER; 
	loc: SynLocation.T): ValThread;
    (* Creates a new thread from a procedure of no arguments. 
       A zero stackSize is converted to a default stackSize. *)

  PROCEDURE JoinThread(threadVal: ValThread; loc: SynLocation.T): ObValue.Val 
    RAISES {ObValue.Error, ObValue.Exception};
    (* Join a thread and report its result. *)

END ObBuiltIn.
