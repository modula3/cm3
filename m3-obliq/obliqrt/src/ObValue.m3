(* Copyright 1991 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(*                                                                           *)
(* Parts Copyright (C) 1997, Columbia University                             *)
(* All rights reserved.                                                      *)
(*
 * Last Modified By: 
 * Last Modified On: Sun Aug 30 20:40:47 1998
 *)

MODULE ObValue EXPORTS ObValue, ObValueRep;

IMPORT Text, Fmt, SynWr, SynLocation, ObTree, AtomList, Atom, ObEval,
       NetObj, Pickle2 AS Pickle, PickleStubs, Rd, Wr, Thread,
       OSError, TextRefTbl, Refany, FileRd, FileWr, OpSys, SharedObj,
       NetObjNotifier, Obliq, ObFieldArraySort, File, Time, FS, Pipe,
       Process, Pathname, M3Config, ObPathSep(*, NetObjF, EventStubLib*);

IMPORT ObValuePickle; 

(* IMPORT Env AS ProcEnv; *)

REVEAL
    ValRemObj = ValRemObjPublic BRANDED "ObValue.ValRemObj" OBJECT OVERRIDES
      Who      := ValRemObjWho;
      Select   := ValRemObjSelect;
      Invoke   := ValRemObjInvoke;
      Update   := ValRemObjUpdate;
      Redirect := ValRemObjRedirect;
      Has      := ValRemObjHas;
      Obtain   := ValRemObjObtain;
      ObtainField := ValRemObjObtainField;
      ObtainDescriptions := ValRemObjObtainDescriptions;
      Describe := ValRemObjDescribe;
    END;

    ValReplObj = ValReplObjPublic BRANDED "ObValue.ValReplObj" OBJECT OVERRIDES
      Who      := ValReplObjWho;
      Select   := ValReplObjSelect;
      Invoke   := ValReplObjInvoke;
      Update   := ValReplObjUpdate;
      Redirect := ValReplObjRedirect;
      Has      := ValReplObjHas;
      Obtain   := ValReplObjObtain;
      ObtainField := ValReplObjObtainField;
      ObtainDescriptions := ValReplObjObtainDescriptions;
      Describe := ValReplObjDescribe;
    END;

    ValSimpleObj = ValSimpleObjPublic BRANDED "ObValue.ValSimpleObj" OBJECT
      OVERRIDES
        Who      := ValSimpleObjWho;
        Select   := ValSimpleObjSelect;
        Invoke   := ValSimpleObjInvoke;
        Update   := ValSimpleObjUpdate;
        Redirect := ValSimpleObjRedirect;
        Has      := ValSimpleObjHas;
        Obtain   := ValSimpleObjObtain;
        ObtainField := ValSimpleObjObtainField;
        ObtainDescriptions := ValSimpleObjObtainDescriptions;
        Describe := ValSimpleObjDescribe;
      END;

    ValRemVar = ValRemVarPublic BRANDED "ObValue.ValRemVar" OBJECT OVERRIDES
      Get        := ValRemVarGet;
      Set        := ValRemVarSet;
    END;

    ValReplVar = ValReplVarPublic BRANDED "ObValue.ValReplVar" OBJECT OVERRIDES
      Get        := ValReplVarGet;
      Set        := ValReplVarSet;
    END;

    ValSimpleVar = ValSimpleVarPublic BRANDED "ObValue.ValSimpleVar" OBJECT OVERRIDES
      Get        := ValSimpleVarGet;
      Set        := ValSimpleVarSet;
    END;

    ValRemArray = ValRemArrayPublic BRANDED "ObValue.ValRemArray" OBJECT OVERRIDES
                     Size   := ValRemArraySize;
                     Get    := ValRemArrayGet;
                     Set    := ValRemArraySet;
                     Sub    := ValRemArraySub;
                     Upd    := ValRemArrayUpd;
                     Obtain := ValRemArrayObtain;
                   END;

    ValReplArray = ValReplArrayPublic BRANDED "ObValue.ValReplArray" OBJECT OVERRIDES
                     Size   := ValReplArraySize;
                     Get    := ValReplArrayGet;
                     Set    := ValReplArraySet;
                     Sub    := ValReplArraySub;
                     Upd    := ValReplArrayUpd;
                     Obtain := ValReplArrayObtain;
                   END;

    ValSimpleArray = ValSimpleArrayPublic BRANDED "ObValue.ValSimpleArray" OBJECT 
                   OVERRIDES
                     Size   := ValSimpleArraySize;
                     Get    := ValSimpleArrayGet;
                     Set    := ValSimpleArraySet;
                     Sub    := ValSimpleArraySub;
                     Upd    := ValSimpleArrayUpd;
                     Obtain := ValSimpleArrayObtain;
                   END;

     RemVarServer = RemVar BRANDED "ObValue.RemVarServer" OBJECT
                   val: Val;
                 OVERRIDES
                   Get := VarGet;
                   Set := VarSet;
                 END;

     RemObjServer = RemObjServerPublic BRANDED "ObValue.RemObjServer" OBJECT
                   self     : ValRemObj;
                   fields   : REF ObjFields;
                   protected: BOOLEAN;
                 OVERRIDES
                   Who      := ObjWho;
                   Select   := ObjSelect;
                   Invoke   := ObjInvoke;
                   Update   := ObjUpdate;
                   Redirect := ObjRedirect;
                   Has      := ObjHas;
                   Obtain   := ObjObtain;
                   ObtainField := ObjObtainField;
                   ObtainDescriptions := ObjObtainDescriptions;
                   Describe := ObjDescribe;
                 END;

    RemArrayServer = RemArray BRANDED "ObValue.RemArrayServer" OBJECT
                     array: REF Vals;
                   OVERRIDES
                     Size   := ArraySize;
                     Get    := ArrayGet;
                     Set    := ArraySet;
                     Sub    := ArraySub;
                     Upd    := ArrayUpd;
                     Obtain := ArrayObtain;
                   END;

    SimpleObj = SimpleObjPublic BRANDED "ObValue.SimpleObj" OBJECT
                self     : ValSimpleObj;
                fields   : REF ObjFields;
                protected: BOOLEAN;

                pickleIn: REF ObjFields := NIL;
                pickleOut: REF ObjFields := NIL;
              OVERRIDES
                Who      := SimpleObjWho;
                Select   := SimpleObjSelect;
                Invoke   := SimpleObjInvoke;
                Update   := SimpleObjUpdate;
                Redirect := SimpleObjRedirect;
                Has      := SimpleObjHas;
                Obtain   := SimpleObjObtain;
                ObtainField := SimpleObjObtainField;
                ObtainDescriptions := SimpleObjObtainDescriptions;
                Describe := SimpleObjDescribe;
              END;

    SimpleVar = SimpleVarPublic BRANDED "ObValue.SimpleVar" OBJECT
                   val: Val;
                 OVERRIDES
                   Get := SimpleVarGet;
                   Set := SimpleVarSet;
                 END;

    SimpleArray = SimpleArrayPublic BRANDED "ObValue.SimpleArray" OBJECT
                     array: REF Vals;
                   OVERRIDES
                     Size   := SimpleArraySize;
                     Get    := SimpleArrayGet;
                     Set    := SimpleArraySet;
                     Sub    := SimpleArraySub;
                     Upd    := SimpleArrayUpd;
                     Obtain := SimpleArrayObtain;
                   END;

    RemFileSystemServer = RemFileSystem BRANDED "ObValue.RemFileSystemServer" OBJECT
                          readOnly: BOOLEAN;
                        OVERRIDES
                          OpenRead   := FileSystemOpenRead;
                          OpenWrite  := FileSystemOpenWrite;
                          OpenAppend := FileSystemOpenAppend;
                          GetAbsolutePathname := FileSystemGetAbsolutePathname;
                          CreateDirectory := FileSystemCreateDirectory;
                          DeleteDirectory := FileSystemDeleteDirectory;
                          DeleteFile := FileSystemDeleteFile;
                          Rename := FileSystemRename;
                          Iterate := FileSystemIterate;
                          Status := FileSystemStatus;
                          SetModificationTime := FileSystemSetModificationTime;
                          (* path support *)
                          PathSep := FileSystemPathSep;
                          PathSearchSep := FileSystemPathSearchSep;
                          PathCurrent := FileSystemPathCurrent;
                          PathParent := FileSystemPathParent;
                          PathValid := FileSystemPathValid;
                          PathDecompose := FileSystemPathDecompose;
                          PathCompose := FileSystemPathCompose;
                          PathAbsolute := FileSystemPathAbsolute;
                          PathPrefix := FileSystemPathPrefix;
                          PathLast := FileSystemPathLast;
                          PathBase := FileSystemPathBase;
                          PathJoin := FileSystemPathJoin;
                          PathLastBase := FileSystemPathLastBase;
                          PathLastExt := FileSystemPathLastExt;
                          PathReplaceExt := FileSystemPathReplaceExt;
                        END;

    RemIteratorServer = RemIterator BRANDED "ObValue.RemIteratorServer" OBJECT
                          iterator: FS.Iterator;
                        OVERRIDES
                          Next := IteratorNext;
                          NextWithStatus := IteratorNextWithStatus;
                          Close := IteratorClose;
                        END;

    RemProcessorServer = RemProcessor BRANDED "ObValue.RemProcessorServer" OBJECT
                        OVERRIDES
                          CreateProcess := ProcessorCreateProcess;
                          GetWorkingDirectory := ProcessorGetWorkingDirectory;
                          SetWorkingDirectory := ProcessorSetWorkingDirectory;
                        END;

    RemProcessServer = RemProcess BRANDED "ObValue.RemProcessServer" OBJECT
                          proc: Process.T;
                        OVERRIDES
                          Wait := ProcessWait;
                          GetID := ProcessGetID;
                        END;

    NonRemValHookServer = NonRemValHook BRANDED "ObValue.NonRemValHookServer" OBJECT
      val: Val;
    OVERRIDES
      init := NonRemValHookInit;
      Get := NonRemValHookGet;
    END;

VAR sysCallTable: TextRefTbl.Default;

PROCEDURE ThisMachine (): TEXT =
  BEGIN
    TRY
      RETURN OpSys.GetHostName();
    EXCEPT
    | OpSys.Error => RETURN "<unknown>";
    END;
  END ThisMachine;

PROCEDURE Setup () =
  BEGIN
    valOk := NEW(ValOk);
    
    netException := NEW(ValException, name := "net_failure");
    sharedException := NEW(ValException, name := "replica_failure");
    sharedFatal := NEW(ValException, name := "replica_fatal");
    threadAlerted := NEW(ValException, name := "thread_alerted");
    osError := NEW(ValException, name := "os_error");
    machineAddress := ThisMachine();

    sysCallTable := NEW(TextRefTbl.Default).init();
    sysCallFailure := NEW(ValException, name := "sys_callFailure");
    showNetObjMsgs := FALSE;

    localProcessor := NewProcessor();
  END Setup;

PROCEDURE RaiseError (msg: TEXT; location: SynLocation.T) RAISES {Error} =
  BEGIN
    RAISE Error(NEW(ErrorPacket, msg := msg, location := location));
  END RaiseError;

PROCEDURE RaiseServerError (msg: TEXT) RAISES {ServerError} =
  BEGIN
    RAISE ServerError(msg);
  END RaiseServerError;

PROCEDURE SameException (exc1, exc2: ValException): BOOLEAN =
  BEGIN
    RETURN Text.Equal(exc1.name, exc2.name);
  END SameException;

PROCEDURE RaiseException (exception: ValException;
                          msg      : TEXT;
                          loc      : SynLocation.T ) RAISES {Exception} =
  BEGIN
    RAISE Exception(NEW(ExceptionPacket, msg := msg, location := loc,
                        exception := exception, data := NIL));
  END RaiseException;

PROCEDURE AtomsToText(atoms: AtomList.T): TEXT =
  VAR msg := "";
  BEGIN
    IF atoms # NIL THEN
      msg := Atom.ToText(atoms.head);
      atoms := atoms.tail;
    END;
    WHILE atoms # NIL DO
      msg := msg & " " & Atom.ToText(atoms.head);
      atoms := atoms.tail;
    END;
    RETURN msg;
  END AtomsToText; 

PROCEDURE RaiseNetException (msg  : TEXT;
                             atoms: AtomList.T;
                             loc  : SynLocation.T) RAISES {Exception} =
  BEGIN
    IF showNetObjMsgs THEN
      msg := msg & " (NetObj says: " & AtomsToText(atoms) & ")";
    END;
    RaiseException(netException, msg, loc);
  END RaiseNetException;

PROCEDURE RaiseSharedException (msg  : TEXT;
                                atoms: AtomList.T;
                                loc  : SynLocation.T) RAISES {Exception} =
  BEGIN
    IF showNetObjMsgs THEN
      msg := msg & " (SharedObj says: " & AtomsToText(atoms) & ")";
    END;
    RaiseException(sharedException, msg, loc);
  END RaiseSharedException;

PROCEDURE ErrorMsg (swr: SynWr.T; packet: ErrorPacket) =
  BEGIN
    Msg(swr, "Execution error ", packet.msg, packet.location);
  END ErrorMsg;

PROCEDURE ExceptionMsg (swr: SynWr.T; packet: ExceptionPacket) =
  VAR name: TEXT;
  BEGIN
    name := packet.exception.name;
    IF NOT Text.Empty(packet.msg) THEN
      name := name & " (" & packet.msg & ")";
    END;
    Msg(swr, "Uncaught exception ", name, packet.location);
  END ExceptionMsg;

PROCEDURE Msg (swr           : SynWr.T;
               msgKind, msg  : TEXT;
               sourceLocation: SynLocation.T) =
  BEGIN
    SynWr.Beg(swr, 2, loud := TRUE);
    SynWr.Text(swr, msgKind, loud := TRUE);
    SynLocation.PrintLocation(swr, sourceLocation);
    SynWr.End(swr, loud := TRUE);
    SynWr.NewLine(swr, loud := TRUE);

    SynWr.Text(swr, msg, loud := TRUE);
    SynWr.NewLine(swr, loud := TRUE);
    SynWr.Flush(swr, loud := TRUE);
  END Msg;

PROCEDURE BadOp (pkg, op: TEXT; location: SynLocation.T) RAISES {Error} =
  BEGIN
    RaiseError("Unknown operation: " & pkg & "_" & op, location);
  END BadOp;

PROCEDURE BadArgType (argNo            : INTEGER;
                      expected, pkg, op: TEXT;
                      location         : SynLocation.T) RAISES {Error} =
  BEGIN
    RaiseError("Argument " & Fmt.Int(argNo) & " of " & pkg & "_" & op
                 & " must have type " & expected, location);
  END BadArgType;

PROCEDURE BadArgVal (argNo            : INTEGER;
                     expected, pkg, op: TEXT;
                     location         : SynLocation.T) RAISES {Error} =
  BEGIN
    RaiseError("Argument " & Fmt.Int(argNo) & " of " & pkg & "_" & op
                 & " must be " & expected, location);
  END BadArgVal;

PROCEDURE NewEnv (name: ObTree.IdeName; env: Env): Env =
  BEGIN
    RETURN NEW(LocalEnv, name := name, val := NIL, rest := env);
  END NewEnv;

PROCEDURE ExtendEnv (binders: ObTree.IdeList; env: Env): Env =
  BEGIN
    IF binders = NIL THEN
      RETURN env;
    ELSE
      RETURN ExtendEnv(binders.rest, NewEnv(binders.first, env));
    END;
  END ExtendEnv;

PROCEDURE PrintWhat (self: ValAnything): TEXT =
  BEGIN
    RETURN self.what;
  END PrintWhat;

PROCEDURE IsSelfOther (self, other: ValAnything): BOOLEAN =
  BEGIN
    RETURN self = other;
  END IsSelfOther;

PROCEDURE Is (v1, v2: Val; location: SynLocation.T): BOOLEAN =
  BEGIN
    (* handle NILs explicitely *)
    IF v1 = NIL OR v2 = NIL THEN RETURN v1 = v2 END;

    TYPECASE v1 OF
    | ValOk =>
        TYPECASE v2 OF | ValOk => RETURN TRUE; ELSE RETURN FALSE; END;
    | ValBool (node1) =>
        TYPECASE v2 OF
        | ValBool (node2) => RETURN node1.bool = node2.bool;
        ELSE
          RETURN FALSE;
        END;
    | ValChar (node1) =>
        TYPECASE v2 OF
        | ValChar (node2) => RETURN node1.char = node2.char;
        ELSE
          RETURN FALSE;
        END;
    | ValText (node1) =>
        TYPECASE v2 OF
        | ValText (node2) => RETURN Text.Equal(node1.text, node2.text);
        ELSE
          RETURN FALSE;
        END;
    | ValException (node1) =>
        TYPECASE v2 OF
        | ValException (node2) =>
            RETURN Text.Equal(node1.name, node2.name);
        ELSE
          RETURN FALSE;
        END;
    | ValInt (node1) =>
        TYPECASE v2 OF
        | ValInt (node2) => RETURN node1.int = node2.int;
        ELSE
          RETURN FALSE;
        END;
    | ValReal (node1) =>
        TYPECASE v2 OF
        | ValReal (node2) => RETURN node1.real = node2.real;
        ELSE
          RETURN FALSE;
        END;
    | ValRemArray (node1) =>
        TYPECASE v2 OF
        | ValRemArray (node2) => RETURN node1.remote = node2.remote;
        ELSE
          RETURN FALSE;
        END;
    | ValReplArray (node1) =>
        TYPECASE v2 OF
        | ValReplArray (node2) => RETURN node1.replica = node2.replica;
        ELSE
          RETURN FALSE;
        END;
    | ValSimpleArray (node1) =>
        TYPECASE v2 OF
        | ValSimpleArray (node2) => RETURN node1.simple = node2.simple;
        ELSE
          RETURN FALSE;
        END;
    | ValAnything (node1) =>
        TYPECASE v2 OF
        | ValAnything (node2) => RETURN node1.Is(node2);
        ELSE
          RETURN FALSE;
        END;
    | ValOption (node1) =>
        TYPECASE v2 OF
        | ValOption (node2) => 
          RETURN Text.Equal(node1.tag, node2.tag) AND
                 Is(node1.val, node2.val, location);
        ELSE
          RETURN FALSE;
        END;
    | ValFun (node1) =>
        TYPECASE v2 OF
        | ValFun (node2) => RETURN node1 = node2;
        ELSE
          RETURN FALSE;
        END;
    | ValMeth (node1) =>
        TYPECASE v2 OF
        | ValMeth (node2) => RETURN node1 = node2;
        ELSE
          RETURN FALSE;
        END;
    | ValRemObj (node1) =>
        TYPECASE v2 OF
        | ValRemObj (node2) => RETURN node1.remote = node2.remote;
        ELSE
          RETURN FALSE;
        END;
    | ValReplObj (node1) =>
        TYPECASE v2 OF
        | ValReplObj (node2) => RETURN node1.replica = node2.replica;
        ELSE
          RETURN FALSE;
        END;
    | ValSimpleObj (node1) =>
        TYPECASE v2 OF
        | ValSimpleObj (node2) => RETURN node1.simple = node2.simple;
        ELSE
          RETURN FALSE;
        END;
    | ValAlias (node1) =>
        TYPECASE v2 OF
        | ValAlias (node2) => RETURN node1 = node2;
        ELSE
          RETURN FALSE;
        END;
    | ValEngine (node1) =>
        TYPECASE v2 OF
        | ValEngine (node2) => RETURN node1.remote = node2.remote;
        ELSE
          RETURN FALSE;
        END;
    ELSE                         <*ASSERT FALSE*>
    END;
  END Is;

PROCEDURE BadArgsNoMsg (desired, found          : INTEGER;
                        routineKind, routineName: TEXT     ): TEXT =
  VAR msg: TEXT;
  BEGIN
    msg := "Expecting " & Fmt.Int(desired);
    IF desired = 1 THEN
      msg := msg & " argument";
    ELSE
      msg := msg & " arguments";
    END;
    msg := msg & ", not " & Fmt.Int(found);
    IF NOT Text.Empty(routineKind) THEN
      msg := msg & ", for " & routineKind & ": " & routineName;
    END;
    RETURN msg;
  END BadArgsNoMsg;

PROCEDURE NonRemValHookGet (self: NonRemValHookServer): Val =
  BEGIN
    RETURN self.val;
  END NonRemValHookGet; 

PROCEDURE NonRemValHookInit (self: NonRemValHookServer; val: Val):
  NonRemValHook =
  BEGIN
    self.val := val;
    RETURN self;
  END NonRemValHookInit; 

(****************************************************************
 * The New Routines
 ****************************************************************)
PROCEDURE NewText (text: TEXT): Val =
  BEGIN
    IF text = NIL THEN text := "" END;
    RETURN NEW(ValText, text := text);
  END NewText;

PROCEDURE NewAlias (obj: ValObj; label: TEXT; location: SynLocation.T):
  ValAlias RAISES {Error, Exception} =
  VAR
    hint    : INTEGER;
    hasLabel          := FALSE;
  BEGIN
    TRY
      hasLabel := obj.Has(label, (*var*) hint);
      IF hasLabel THEN
        RETURN NEW(ValAlias, label := label, labelIndexHint := hint,
                   obj := obj);
      ELSE
        RaiseError("Field not found in object: " & label, location);
        <*ASSERT FALSE*>
      END;
    EXCEPT
    | NetObj.Error (atoms) =>
        RaiseNetException("on remote object access", atoms, location);
      <*ASSERT FALSE*>
    | SharedObj.Error (atoms) =>
        RaiseSharedException(
          "on replicated object access", atoms, location);
      <*ASSERT FALSE*>
    | Thread.Alerted =>
        RaiseException(threadAlerted, "on remote object access", location);
      <*ASSERT FALSE*>
    END;
  END NewAlias;

PROCEDURE NewArray (READONLY vals: Vals): ValArray =
  VAR newVals: REF Vals;
  BEGIN
    newVals := NEW(REF Vals, NUMBER(vals));
    newVals^ := vals;
    RETURN NewArrayFromVals(newVals);
  END NewArray;

PROCEDURE NewArrayFromVals (vals: REF Vals): ValArray =
  BEGIN
    RETURN NEW(ValRemArray, remote := NEW(RemArrayServer, array := vals));
  END NewArrayFromVals;

PROCEDURE NewVar (val: Val): ValVar =
  BEGIN
    RETURN NEW(ValRemVar, remote := NEW(RemVarServer, val := val));
  END NewVar;

PROCEDURE NewObject (READONLY fields   : ObjFields;
                              who      : TEXT        := "";
                              protected: BOOLEAN     := FALSE;
                              sync     : Sync        := NIL    ): ValObj =
  VAR remFields: REF ObjFields;
  BEGIN
    remFields := NEW(REF ObjFields, NUMBER(fields));
    remFields^ := fields;
    RETURN NewObjectFromFields(remFields, who, protected, sync);
  END NewObject;

PROCEDURE NewObjectFromFields (fields   : REF ObjFields;
                               who      : TEXT;
                               protected: BOOLEAN;
                               sync     : Sync           ): ValObj =
  VAR remObjServ: RemObjServer;
  BEGIN
    IF NUMBER(fields^) > 1 THEN ObFieldArraySort.Sort(fields^) END;
    remObjServ :=
      NEW(RemObjServer, who := who, self := NEW(ValRemObj, remote := NIL),
          fields := fields, protected := protected, sync := sync);
    remObjServ.self.remote := remObjServ;
    RETURN remObjServ.self;
  END NewObjectFromFields;

PROCEDURE NewReplArray (READONLY vals: Vals): ValArray 
  RAISES {SharedObj.Error} =
  VAR newVals: REF Vals;
  BEGIN
    newVals := NEW(REF Vals, NUMBER(vals));
    newVals^ := vals;
    RETURN NewReplArrayFromVals(newVals);
  END NewReplArray; 

PROCEDURE NewReplArrayFromVals (vals: REF Vals): ValArray 
  RAISES {SharedObj.Error} =
  BEGIN
    WITH rep = NEW(ValReplArray, replica := NEW(ReplArrayStd, 
                                                array:=vals).init()) DO
      RETURN rep;
    END;
  END NewReplArrayFromVals;

PROCEDURE NewReplVar (val: Val): ValVar RAISES {SharedObj.Error} =
  BEGIN
    WITH rep = NEW(ValReplVar, replica := NEW(ReplVarStd, 
                                              val := val).init()) DO
      RETURN rep;
    END;
  END NewReplVar; 

PROCEDURE NewReplObject (READONLY fields   : ObjFields;
                         who      : TEXT        := "";
                         protected: BOOLEAN     := FALSE): ValObj
    RAISES {SharedObj.Error} =
  VAR replFields: REF ObjFields;
  BEGIN
    replFields := NEW(REF ObjFields, NUMBER(fields));
    replFields^ := fields;
    RETURN NewReplObjectFromFields(replFields, who, protected);
  END NewReplObject;

PROCEDURE NewReplObjectFromFields (fields   : REF ObjFields;
                                   who      : TEXT;
                                   protected: BOOLEAN): ValObj 
    RAISES {SharedObj.Error} =
  VAR replObjServ: ReplObjStd;
  BEGIN
    (*
    FOR i := FIRST(fields^) TO LAST(fields^) DO
      TYPECASE fields[i].field OF
      | ValAlias => 
        RaiseServerError("Cannot have alias fields in a replicated object");
      ELSE
      END;
    END;
    *)

    IF NUMBER(fields^) > 1 THEN ObFieldArraySort.Sort(fields^) END;
    replObjServ := NEW(ReplObjStd, who := who, 
                      self := NEW(ValReplObj, replica := NIL),
                      protected := protected,
                      fields := fields).init();
    replObjServ.self.replica := replObjServ;

    RETURN replObjServ.self;
  END NewReplObjectFromFields;

PROCEDURE NewSimpleArray (READONLY vals: Vals): ValArray =
  VAR newVals: REF Vals;
  BEGIN
    newVals := NEW(REF Vals, NUMBER(vals));
    newVals^ := vals;
    RETURN NewSimpleArrayFromVals(newVals);
  END NewSimpleArray; 

PROCEDURE NewSimpleArrayFromVals (vals: REF Vals): ValArray =
  BEGIN
    RETURN NEW(ValSimpleArray, simple := NEW(SimpleArray, array := vals));
  END NewSimpleArrayFromVals; 

PROCEDURE NewSimpleVar (val: Val): ValVar =
  BEGIN
    RETURN NEW(ValSimpleVar, simple := NEW(SimpleVar, val := val));
  END NewSimpleVar; 

PROCEDURE NewSimpleObject (READONLY fields   : ObjFields;
                           who      : TEXT        := "";
                           protected: BOOLEAN     := FALSE;
                           sync     : Sync        := NIL    ): ValObj =
  VAR simpleFields: REF ObjFields;
  BEGIN
    simpleFields := NEW(REF ObjFields, NUMBER(fields));
    simpleFields^ := fields;
    RETURN NewSimpleObjectFromFields(simpleFields, who, protected, sync);
  END NewSimpleObject; 

PROCEDURE NewSimpleObjectFromFields (fields   : REF ObjFields;
                                     who      : TEXT;
                                     protected: BOOLEAN;
                                     sync     : Sync           ): ValObj =
  VAR simpleObj: SimpleObj;
  BEGIN
    IF NUMBER(fields^) > 1 THEN ObFieldArraySort.Sort(fields^) END;
    simpleObj := NEW(SimpleObj, who := who,
                     self := NEW(ValSimpleObj, simple := NIL), 
                     fields := fields,
                     protected := protected, sync := sync);
    simpleObj.self.simple := simpleObj;
    RETURN simpleObj.self;
  END NewSimpleObjectFromFields; 

(***************************
 object conversion routines
 ***************************)
PROCEDURE CloneObjData(valObj: ValObj; mySelf: ValObj;
                       VAR resWho: TEXT;
                       VAR resFields: REF ObjFields;
                       VAR protected: BOOLEAN;
                       VAR sync: Sync) 
  RAISES {ServerError, NetObj.Error, SharedObj.Error, Thread.Alerted} =
  VAR
    who: TEXT;
    fieldsOf1: REF ObjFields;
    resSize  : INTEGER;
    serialized: BOOLEAN;
  BEGIN
    who := valObj.Who( (*out*)protected, (*out*) serialized);
    IF Text.Empty(who) THEN who := "someone" END;
    resWho := "clone of " & who;
    fieldsOf1 := valObj.Obtain(ObjEqual(valObj, mySelf));
    resSize := NUMBER(fieldsOf1^);
    resFields := NEW(REF ObjFields, resSize);
    resFields^ := fieldsOf1^;

    FOR i := FIRST(resFields^) TO LAST(resFields^) DO
      TYPECASE resFields^[i].field OF
      | ValMeth (node) => 
        node.meth := CopyMeth(node.meth);
        node.meth.update := FALSE;
      ELSE
      END;
    END;

    IF serialized THEN
      sync := NEW(Sync, mutex := NEW(Thread.Mutex))
    ELSE
      sync := NIL
    END;
  END CloneObjData;

PROCEDURE ToSimpleObj (READONLY obj: ValObj; mySelf: ValObj): ValObj
  RAISES {ServerError, NetObj.Error, SharedObj.Error, Thread.Alerted} =
  VAR
    resWho: TEXT;
    resFields: REF ObjFields;
    protected: BOOLEAN;
    sync                 : Sync;
  BEGIN
    CloneObjData(obj, mySelf, resWho, resFields, protected, sync);
    
    WITH res = NEW(SimpleObj, who := "",
                   self := NEW(ValSimpleObj, simple := NIL), 
                   fields := resFields,
                   protected := protected, sync := sync) DO
      res.self.simple := res;
      RETURN res.self;
    END;
  END ToSimpleObj;

PROCEDURE CopyMeth (meth: ObTree.TermMeth): ObTree.TermMeth =
  BEGIN
    RETURN NEW(ObTree.TermMeth,
               location := meth.location,
               binders := meth.binders,
               bindersNo := meth.bindersNo,
               body := meth.body,
               globals := meth.globals,
               globalsNo := meth.globalsNo,
               update := meth.update);
  END CopyMeth; 

PROCEDURE ToReplObj (READONLY obj: ValObj; mySelf: ValObj;
                     READONLY updateMethods: ARRAY OF TEXT): ValObj
  RAISES {ServerError, NetObj.Error, SharedObj.Error, Thread.Alerted} =
  VAR
    resWho: TEXT;
    resFields: REF ObjFields;
    protected: BOOLEAN;
    sync                 : Sync;
    j  : INTEGER;
  BEGIN
    CloneObjData(obj, mySelf, resWho, resFields, protected, sync);
    (* why bother failing?  What's the big deal!?   It becomes
       serialized, that's all.
    IF sync = NIL THEN
      RaiseServerError("Replicated Objects must be serialized");
    END;
    *)

    FOR i := FIRST(updateMethods) TO LAST(updateMethods) DO
      j := FIRST(resFields^);
      WHILE j <= LAST(resFields^) DO
        IF Text.Equal(updateMethods[i], resFields^[j].label) THEN
          TYPECASE resFields^[j].field OF
          | ValMeth(node) => node.meth.update := TRUE; EXIT;
          ELSE
            RaiseServerError("field '" & updateMethods[i] & 
              "' is not a method");
          END;
        END;
        INC(j);
      END;
      IF j > LAST(resFields^) THEN
        RaiseServerError("update method '" & updateMethods[i] & 
          "' does not exist");
      END;
    END;
    WITH res = NEW(ReplObjStd, who := "", protected := protected,
                   self := NEW(ValReplObj, replica := NIL), 
                   fields := resFields).init() DO
      res.self.replica := res;
      RETURN res.self;
    END;
  END ToReplObj;

PROCEDURE ToRemObj (READONLY obj: ValObj; mySelf: ValObj): ValObj
  RAISES {ServerError, NetObj.Error, SharedObj.Error, Thread.Alerted} =
  VAR
    resWho: TEXT;
    resFields: REF ObjFields;
    protected: BOOLEAN;
    sync                 : Sync;
  BEGIN
    CloneObjData(obj, mySelf, resWho, resFields, protected, sync);

    WITH res = NEW(RemObjServer, who := resWho,
                   self := NEW(ValRemObj, remote := NIL), 
                   fields := resFields,
                   protected := protected, sync := sync) DO
      res.self.remote := res;
      RETURN res.self;
    END;
  END ToRemObj;


(***************************
 ValVar object wrapper functions
 ***************************)
PROCEDURE ValRemVarGet (self: ValRemVar): Val 
  RAISES {NetObj.Error, Thread.Alerted} =
  BEGIN
    RETURN self.remote.Get();
  END ValRemVarGet;

PROCEDURE ValRemVarSet (self: ValRemVar; val: Val)
  RAISES {NetObj.Error, Thread.Alerted} =
  BEGIN
    self.remote.Set(val);
  END ValRemVarSet; 

PROCEDURE ValReplVarGet (self: ValReplVar): Val 
  RAISES {SharedObj.Error} =
  BEGIN
    RETURN self.replica.Get();
  END ValReplVarGet;

PROCEDURE ValReplVarSet (self: ValReplVar; val: Val)
  RAISES {SharedObj.Error} =
  BEGIN
    self.replica.Set(val);
  END ValReplVarSet; 

PROCEDURE ValSimpleVarGet (self: ValSimpleVar): Val 
  RAISES {NetObj.Error, Thread.Alerted, SharedObj.Error} =
  BEGIN
    RETURN self.simple.Get();
  END ValSimpleVarGet;

PROCEDURE ValSimpleVarSet (self: ValSimpleVar; val: Val)
  RAISES {NetObj.Error, Thread.Alerted, SharedObj.Error} =
  BEGIN
    self.simple.Set(val);
  END ValSimpleVarSet; 

(***************************
 ValArray object wrapper functions
 ***************************)
PROCEDURE ValRemArraySize (arr: ValRemArray): INTEGER 
  RAISES {NetObj.Error, Thread.Alerted} =
  BEGIN
    RETURN arr.remote.Size();
  END ValRemArraySize; 

PROCEDURE ValRemArrayGet (self: ValRemArray; i: INTEGER): Val
  RAISES {ServerError, NetObj.Error, Thread.Alerted} =
  BEGIN
    RETURN self.remote.Get(i);
  END ValRemArrayGet;

PROCEDURE ValRemArraySet (self: ValRemArray; i: INTEGER; val: Val)
  RAISES {ServerError, NetObj.Error, Thread.Alerted} =
  BEGIN
    self.remote.Set(i,val);
  END ValRemArraySet;

PROCEDURE ValRemArraySub (self: ValRemArray; start, size: INTEGER): ValArray
  RAISES {ServerError, NetObj.Error, Thread.Alerted} =
  BEGIN
    RETURN self.remote.Sub(start,size);
  END ValRemArraySub;

PROCEDURE ValRemArrayUpd (   self       : ValRemArray;
                             start, size: INTEGER;
                    READONLY otherArr   : REF Vals        )
  RAISES {ServerError, NetObj.Error, Thread.Alerted} =
  BEGIN
    self.remote.Upd(start,size,otherArr);
  END ValRemArrayUpd;

PROCEDURE ValRemArrayObtain (self: ValRemArray): REF Vals 
  RAISES {NetObj.Error, Thread.Alerted} =
  BEGIN
    RETURN self.remote.Obtain();
  END ValRemArrayObtain; 

PROCEDURE ValReplArraySize (arr: ValReplArray): INTEGER 
  RAISES {SharedObj.Error} =
  BEGIN
    RETURN arr.replica.Size();
  END ValReplArraySize; 

PROCEDURE ValReplArrayGet (self: ValReplArray; i: INTEGER): Val
  RAISES {SharedObj.Error, ServerError} =
  BEGIN
    RETURN self.replica.Get(i);
  END ValReplArrayGet;

PROCEDURE ValReplArraySet (self: ValReplArray; i: INTEGER; val: Val)
  RAISES {SharedObj.Error, ServerError} =
  BEGIN
    self.replica.Set(i,val);
  END ValReplArraySet;

PROCEDURE ValReplArraySub (self: ValReplArray; start, size: INTEGER): ValArray
  RAISES {SharedObj.Error, ServerError} =
  BEGIN
    RETURN self.replica.Sub(start,size);
  END ValReplArraySub;

PROCEDURE ValReplArrayUpd (   self       : ValReplArray;
                             start, size: INTEGER;
                    READONLY otherArr   : REF Vals        )
  RAISES {SharedObj.Error, ServerError} =
  BEGIN
    self.replica.Upd(start,size,otherArr);
  END ValReplArrayUpd;

PROCEDURE ValReplArrayObtain (self: ValReplArray): REF Vals 
  RAISES {SharedObj.Error} =
  BEGIN
    RETURN self.replica.Obtain();
  END ValReplArrayObtain; 

PROCEDURE ValSimpleArraySize (arr: ValSimpleArray): INTEGER 
  RAISES {SharedObj.Error, NetObj.Error, Thread.Alerted} =
  BEGIN
    RETURN arr.simple.Size();
  END ValSimpleArraySize; 

PROCEDURE ValSimpleArrayGet (self: ValSimpleArray; i: INTEGER): Val
  RAISES {SharedObj.Error, ServerError, NetObj.Error, Thread.Alerted} =
  BEGIN
    RETURN self.simple.Get(i);
  END ValSimpleArrayGet;

PROCEDURE ValSimpleArraySet (self: ValSimpleArray; i: INTEGER; val: Val)
  RAISES {SharedObj.Error, ServerError, NetObj.Error, Thread.Alerted} =
  BEGIN
    self.simple.Set(i,val);
  END ValSimpleArraySet;

PROCEDURE ValSimpleArraySub (self: ValSimpleArray; 
                             start, size: INTEGER): ValArray
  RAISES {SharedObj.Error, ServerError, NetObj.Error, Thread.Alerted} =
  BEGIN
    RETURN self.simple.Sub(start,size);
  END ValSimpleArraySub;

PROCEDURE ValSimpleArrayUpd (   self       : ValSimpleArray;
                             start, size: INTEGER;
                    READONLY otherArr   : REF Vals        )
  RAISES {SharedObj.Error, ServerError, NetObj.Error, Thread.Alerted} =
  BEGIN
    self.simple.Upd(start,size,otherArr);
  END ValSimpleArrayUpd;

PROCEDURE ValSimpleArrayObtain (self: ValSimpleArray): REF Vals 
  RAISES {SharedObj.Error, NetObj.Error, Thread.Alerted} =
  BEGIN
    RETURN self.simple.Obtain();
  END ValSimpleArrayObtain; 

(***************************
 ValObj object wrapper functions
 ***************************)
PROCEDURE ValRemObjWho(self: ValRemObj; 
                    VAR(*out*) protected, serialized: BOOLEAN): TEXT 
          RAISES {NetObj.Error, Thread.Alerted} =
  BEGIN
    RETURN self.remote.Who(protected, serialized);
  END ValRemObjWho;

PROCEDURE ValRemObjSelect (    self    : ValRemObj;
                               swr     : SynWr.T;
                               label   : TEXT;
                               internal: BOOLEAN;
                           VAR hint    : INTEGER    ): Val
  RAISES {Error, Exception, ServerError, SharedObj.Error, NetObj.Error,
          Thread.Alerted} =
  BEGIN
    RETURN self.remote.Select(swr, label, internal, hint);
  END ValRemObjSelect;

PROCEDURE ValRemObjInvoke (         self    : ValRemObj;
                                    swr     : SynWr.T;
                                    label   : TEXT;
                                    argNo   : INTEGER;
                           READONLY args    : Vals;
                                    internal: BOOLEAN;
                           VAR      hint    : INTEGER    ): Val
  RAISES {Error, Exception, ServerError, SharedObj.Error, NetObj.Error,
          Thread.Alerted} =
  BEGIN
    RETURN self.remote.Invoke(swr, label, argNo, args, internal, hint);
  END ValRemObjInvoke;

PROCEDURE ValRemObjUpdate (    self    : ValRemObj;
                               label   : TEXT;
                               val     : Val;
                               internal: BOOLEAN;
                           VAR hint    : INTEGER    )
  RAISES {ServerError, SharedObj.Error, NetObj.Error, Thread.Alerted} =
  BEGIN
    self.remote.Update(label, val, internal, hint);
  END ValRemObjUpdate;

PROCEDURE ValRemObjRedirect (self: ValRemObj; val: Val; internal: BOOLEAN)
  RAISES {ServerError, SharedObj.Error, NetObj.Error, Thread.Alerted} =
  BEGIN
    self.remote.Redirect(val, internal);
  END ValRemObjRedirect;

PROCEDURE ValRemObjHas (self: ValRemObj; label: TEXT; VAR hint: INTEGER):
  BOOLEAN RAISES {NetObj.Error, Thread.Alerted} =
  BEGIN
    RETURN self.remote.Has(label, hint);
  END ValRemObjHas;

PROCEDURE ValRemObjObtain (self: ValRemObj; internal: BOOLEAN):
  REF ObjFields RAISES {ServerError, NetObj.Error, Thread.Alerted} =
  BEGIN
    RETURN self.remote.Obtain(internal);
  END ValRemObjObtain;

PROCEDURE ValRemObjObtainField(self: ValRemObj; label: TEXT; 
                               internal: BOOLEAN): Val
  RAISES {ServerError, NetObj.Error, Thread.Alerted} =
  BEGIN
    RETURN self.remote.ObtainField(label,internal);
  END ValRemObjObtainField;

PROCEDURE ValRemObjObtainDescriptions(self: ValRemObj): REF ObjFieldTypes 
  RAISES {ServerError, SharedObj.Error, NetObj.Error, Thread.Alerted} =
  BEGIN
    RETURN self.remote.ObtainDescriptions();
  END ValRemObjObtainDescriptions;

PROCEDURE ValRemObjDescribe(self: ValRemObj; label: TEXT): TEXT
  RAISES {ServerError, SharedObj.Error, NetObj.Error, Thread.Alerted} =
  BEGIN
    RETURN self.remote.Describe(label);
  END ValRemObjDescribe;

PROCEDURE ValReplObjWho (            self                 : ValReplObj;
                         VAR (*out*) protected, serialized: BOOLEAN     ):
  TEXT RAISES {SharedObj.Error} =
  BEGIN
    serialized := TRUE;
    RETURN self.replica.Who(protected);
  END ValReplObjWho;

PROCEDURE ValReplObjSelect (               self    : ValReplObj;
                                           swr     : SynWr.T;
                                           label   : TEXT;
                            <*UNUSED*>     internal: BOOLEAN;
                                       VAR hint    : INTEGER     ): Val
  RAISES {Error, Exception, ServerError, SharedObj.Error} =
  BEGIN
    RETURN self.replica.Select(swr, label, hint);
  END ValReplObjSelect;

PROCEDURE ValReplObjInvoke (                    self    : ValReplObj;
                                                swr     : SynWr.T;
                                                label   : TEXT;
                                                argNo   : INTEGER;
                                       READONLY args    : Vals;
                            <*UNUSED*>          internal: BOOLEAN;
                                       VAR      hint    : INTEGER     ):
  Val RAISES {Error, Exception, ServerError, SharedObj.Error} =
  BEGIN
    RETURN self.replica.Invoke(swr, label, argNo, args, hint);
  END ValReplObjInvoke;

PROCEDURE ValReplObjUpdate (    self    : ValReplObj;
                                label   : TEXT;
                                val     : Val;
                                internal: BOOLEAN;
                            VAR hint    : INTEGER     )
  RAISES {ServerError, SharedObj.Error} =
  BEGIN
    self.replica.Update(label, val, internal, hint);
  END ValReplObjUpdate;

PROCEDURE ValReplObjRedirect (self    : ValReplObj;
                              val     : Val;
                              internal: BOOLEAN     )
  RAISES {ServerError, SharedObj.Error} =
  BEGIN
    self.replica.Redirect(val, internal);
    (*
    RaiseServerError("Cannot Redirect Replicated Object Fields");
    *)
  END ValReplObjRedirect;

PROCEDURE ValReplObjHas (self: ValReplObj; label: TEXT; VAR hint: INTEGER):
  BOOLEAN RAISES {SharedObj.Error} =
  BEGIN
    RETURN self.replica.Has(label, hint);
  END ValReplObjHas;

PROCEDURE ValReplObjObtain (self: ValReplObj; internal: BOOLEAN):
  REF ObjFields RAISES {ServerError, SharedObj.Error} =
  BEGIN
    RETURN self.replica.Obtain(internal);
  END ValReplObjObtain;

PROCEDURE ValReplObjObtainField(self: ValReplObj; label: TEXT; 
                               internal: BOOLEAN): Val
  RAISES {ServerError, SharedObj.Error} =
  BEGIN
    RETURN self.replica.ObtainField(label,internal);
  END ValReplObjObtainField;

PROCEDURE ValReplObjObtainDescriptions(self: ValReplObj): REF ObjFieldTypes 
  RAISES {ServerError, SharedObj.Error} =
  BEGIN
    RETURN self.replica.ObtainDescriptions();
  END ValReplObjObtainDescriptions;

PROCEDURE ValReplObjDescribe(self: ValReplObj; label: TEXT): TEXT
  RAISES {ServerError, SharedObj.Error} =
  BEGIN
    RETURN self.replica.Describe(label);
  END ValReplObjDescribe;

PROCEDURE ValSimpleObjWho (self: ValSimpleObj;
                           VAR (*out*) protected, serialized: BOOLEAN):
  TEXT =
  BEGIN
    RETURN self.simple.Who(protected, serialized);
  END ValSimpleObjWho;

PROCEDURE ValSimpleObjSelect (    self    : ValSimpleObj;
                                  swr     : SynWr.T;
                                  label   : TEXT;
                                  internal: BOOLEAN;
                              VAR hint    : INTEGER       ): Val
  RAISES {Error, Exception, ServerError, SharedObj.Error, NetObj.Error,
          Thread.Alerted} =
  BEGIN
    RETURN self.simple.Select(swr, label, internal, hint);
  END ValSimpleObjSelect;

PROCEDURE ValSimpleObjInvoke (         self    : ValSimpleObj;
                                       swr     : SynWr.T;
                                       label   : TEXT;
                                       argNo   : INTEGER;
                              READONLY args    : Vals;
                                       internal: BOOLEAN;
                              VAR      hint    : INTEGER       ): Val
  RAISES {Error, Exception, ServerError, SharedObj.Error, NetObj.Error,
          Thread.Alerted} =
  BEGIN
    RETURN self.simple.Invoke(swr, label, argNo, args, internal, hint);
  END ValSimpleObjInvoke;

PROCEDURE ValSimpleObjUpdate (    self    : ValSimpleObj;
                                  label   : TEXT;
                                  val     : Val;
                                  internal: BOOLEAN;
                              VAR hint    : INTEGER       )
  RAISES {ServerError, SharedObj.Error, NetObj.Error, Thread.Alerted} =
  BEGIN
    self.simple.Update(label, val, internal, hint);
  END ValSimpleObjUpdate;

PROCEDURE ValSimpleObjRedirect (self    : ValSimpleObj;
                                val     : Val;
                                internal: BOOLEAN       )
  RAISES {ServerError, SharedObj.Error, NetObj.Error, Thread.Alerted} =
  BEGIN
    self.simple.Redirect(val, internal);
  END ValSimpleObjRedirect;

PROCEDURE ValSimpleObjHas (    self : ValSimpleObj;
                               label: TEXT;
                           VAR hint : INTEGER       ): BOOLEAN =
  BEGIN
    RETURN self.simple.Has(label, hint);
  END ValSimpleObjHas;

PROCEDURE ValSimpleObjObtain (self: ValSimpleObj; internal: BOOLEAN):
  REF ObjFields RAISES {ServerError} =
  BEGIN
    RETURN self.simple.Obtain(internal);
  END ValSimpleObjObtain;

PROCEDURE ValSimpleObjObtainField(self: ValSimpleObj; label: TEXT; 
                                  internal: BOOLEAN): Val
  RAISES {ServerError, SharedObj.Error, NetObj.Error, Thread.Alerted} =
  BEGIN
    RETURN self.simple.ObtainField(label,internal);
  END ValSimpleObjObtainField;

PROCEDURE ValSimpleObjObtainDescriptions(self: ValSimpleObj): REF ObjFieldTypes
  RAISES {ServerError, SharedObj.Error, NetObj.Error, Thread.Alerted} =
  BEGIN
    RETURN self.simple.ObtainDescriptions();
  END ValSimpleObjObtainDescriptions;

PROCEDURE ValSimpleObjDescribe(self: ValSimpleObj; label: TEXT): TEXT
  RAISES {ServerError, SharedObj.Error, NetObj.Error, Thread.Alerted} =
  BEGIN
    RETURN self.simple.Describe(label);
  END ValSimpleObjDescribe;

(***************************
 variable fields
 ***************************)

PROCEDURE VarGet (self: RemVarServer): Val =
  BEGIN
    RETURN self.val;
  END VarGet;

PROCEDURE ReplVarGet (self: ReplVar): Val =
  BEGIN
    RETURN self.val;
  END ReplVarGet; 

PROCEDURE SimpleVarGet (self: SimpleVar): Val =
  BEGIN
    RETURN self.val;
  END SimpleVarGet; 

PROCEDURE VarSet (self: RemVarServer; val: Val) =
  BEGIN
    self.val := val;
  END VarSet;

PROCEDURE ReplVarSet (self: ReplVar; val: Val) =
  BEGIN
    self.val := val;
  END ReplVarSet; 

PROCEDURE SimpleVarSet (self: SimpleVar; val: Val) =
  BEGIN
    self.val := val;
  END SimpleVarSet; 

PROCEDURE ReplVarInit (self: ReplVar): ReplVar =
  BEGIN
    RETURN self;
  END ReplVarInit; 

(***************************
 array fields
 ***************************)
PROCEDURE ArraySize (arr: RemArrayServer): INTEGER =
  BEGIN
    RETURN NUMBER(arr.array^);
  END ArraySize;

PROCEDURE ReplArraySize (arr: ReplArray): INTEGER =
  BEGIN
    RETURN NUMBER(arr.array^);
  END ReplArraySize; 

PROCEDURE SimpleArraySize (arr: SimpleArray): INTEGER =
  BEGIN
    RETURN NUMBER(arr.array^);
  END SimpleArraySize; 

PROCEDURE ArrayGet (self: RemArrayServer; i: INTEGER): Val
  RAISES {ServerError} =
  BEGIN
    IF (i < 0) OR (i >= NUMBER(self.array^)) THEN
      RaiseServerError("arg not in range")
    END;
    RETURN self.array^[i];
  END ArrayGet;

PROCEDURE ReplArrayGet (self: ReplArray; i: INTEGER): Val
  RAISES {ServerError} =
  BEGIN
    IF (i < 0) OR (i >= NUMBER(self.array^)) THEN
      RaiseServerError("arg not in range")
    END;
    RETURN self.array^[i];
  END ReplArrayGet; 

PROCEDURE SimpleArrayGet (self: SimpleArray; i: INTEGER): Val
  RAISES {ServerError} =
  BEGIN
    IF (i < 0) OR (i >= NUMBER(self.array^)) THEN
      RaiseServerError("arg not in range")
    END;
    RETURN self.array^[i];
  END SimpleArrayGet;

PROCEDURE ArraySet (self: RemArrayServer; i: INTEGER; val: Val)
  RAISES {ServerError} =
  BEGIN
    IF (i < 0) OR (i >= NUMBER(self.array^)) THEN
      RaiseServerError("arg 1 not in range");
    END;
    self.array^[i] := val;
  END ArraySet;

PROCEDURE ReplArraySet (self: ReplArray; i: INTEGER; val: Val)
  RAISES {ServerError} =
  BEGIN
    IF (i < 0) OR (i >= NUMBER(self.array^)) THEN
      RaiseServerError("arg 1 not in range");
    END;
    self.array^[i] := val;
  END ReplArraySet; 

PROCEDURE SimpleArraySet (self: SimpleArray; i: INTEGER; val: Val)
  RAISES {ServerError} =
  BEGIN
    IF (i < 0) OR (i >= NUMBER(self.array^)) THEN
      RaiseServerError("arg 1 not in range");
    END;
    self.array^[i] := val;
  END SimpleArraySet;

PROCEDURE ArraySub (self: RemArrayServer; start, size: INTEGER): ValArray
  RAISES {ServerError} =
  VAR
    len : INTEGER;
    vals: REF Vals;
  BEGIN
    len := NUMBER(self.array^);
    IF (start < 0) OR (start > len) THEN
      RaiseServerError("arg 2 not in range");
    END;
    IF (size < 0) OR (start + size > len) THEN
      RaiseServerError("arg 3 not in range");
    END;
    vals := NEW(REF Vals, size);
    FOR i := 0 TO size - 1 DO vals^[i] := self.array^[start + i]; END;
    RETURN NEW(ValRemArray, remote := NEW(RemArrayServer, array := vals));
  END ArraySub;

PROCEDURE ReplArraySub (self: ReplArray; start, size: INTEGER): ValArray
  RAISES {SharedObj.Error, ServerError} =
  VAR
    len : INTEGER;
    vals: REF Vals;
  BEGIN
    len := NUMBER(self.array^);
    IF (start < 0) OR (start > len) THEN
      RaiseServerError("arg 2 not in range");
    END;
    IF (size < 0) OR (start + size > len) THEN
      RaiseServerError("arg 3 not in range");
    END;
    vals := NEW(REF Vals, size);
    FOR i := 0 TO size - 1 DO vals^[i] := self.array^[start + i]; END;
    WITH rep = NEW(ValReplArray, replica := NEW(ReplArrayStd, 
                                                array:=vals).init()) DO
      RETURN rep;
    END;
  END ReplArraySub; 

PROCEDURE SimpleArraySub (self: SimpleArray; start, size: INTEGER): ValArray
  RAISES {ServerError} =
  VAR
    len : INTEGER;
    vals: REF Vals;
  BEGIN
    len := NUMBER(self.array^);
    IF (start < 0) OR (start > len) THEN
      RaiseServerError("arg 2 not in range");
    END;
    IF (size < 0) OR (start + size > len) THEN
      RaiseServerError("arg 3 not in range");
    END;
    vals := NEW(REF Vals, size);
    FOR i := 0 TO size - 1 DO vals^[i] := self.array^[start + i]; END;
    RETURN NEW(ValSimpleArray, simple := NEW(SimpleArray, array := vals));
  END SimpleArraySub;
 
PROCEDURE ArrayUpd (         self       : RemArrayServer;
                             start, size: INTEGER;
                    READONLY otherArr   : REF Vals        )
  RAISES {ServerError} =
  VAR
    selfLen, otherLen: INTEGER;
    selfArr          : REF Vals;
  BEGIN
    selfArr := self.array;
    selfLen := NUMBER(selfArr^);
    IF (start < 0) OR (start > selfLen) THEN
      RaiseServerError("arg 2 not in range");
    END;
    IF (size < 0) OR (start + size > selfLen) THEN
      RaiseServerError("arg 3 not in range of arg 1");
    END;
    otherLen := NUMBER(otherArr^);
    IF size > otherLen THEN
      RaiseServerError("arg 3 not in range of arg 4");
    END;
    FOR i := size - 1 TO 0 BY -1 DO
      selfArr^[start + i] := otherArr^[i];
    END;
  END ArrayUpd;

PROCEDURE ReplArrayUpd (     self       : ReplArray;
                             start, size: INTEGER;
                    READONLY otherArr   : REF Vals        )
  RAISES {ServerError} =
  VAR
    selfLen, otherLen: INTEGER;
    selfArr          : REF Vals;
  BEGIN
    selfArr := self.array;
    selfLen := NUMBER(selfArr^);
    IF (start < 0) OR (start > selfLen) THEN
      RaiseServerError("arg 2 not in range");
    END;
    IF (size < 0) OR (start + size > selfLen) THEN
      RaiseServerError("arg 3 not in range of arg 1");
    END;
    otherLen := NUMBER(otherArr^);
    IF size > otherLen THEN
      RaiseServerError("arg 3 not in range of arg 4");
    END;
    FOR i := size - 1 TO 0 BY -1 DO
      selfArr^[start + i] := otherArr^[i];
    END;
  END ReplArrayUpd;

PROCEDURE SimpleArrayUpd (   self       : SimpleArray;
                             start, size: INTEGER;
                    READONLY otherArr   : REF Vals        )
  RAISES {ServerError} =
  VAR
    selfLen, otherLen: INTEGER;
    selfArr          : REF Vals;
  BEGIN
    selfArr := self.array;
    selfLen := NUMBER(selfArr^);
    IF (start < 0) OR (start > selfLen) THEN
      RaiseServerError("arg 2 not in range");
    END;
    IF (size < 0) OR (start + size > selfLen) THEN
      RaiseServerError("arg 3 not in range of arg 1");
    END;
    otherLen := NUMBER(otherArr^);
    IF size > otherLen THEN
      RaiseServerError("arg 3 not in range of arg 4");
    END;
    FOR i := size - 1 TO 0 BY -1 DO
      selfArr^[start + i] := otherArr^[i];
    END;
  END SimpleArrayUpd; 

PROCEDURE ArrayObtain (self: RemArrayServer): REF Vals RAISES {} =
  BEGIN
    RETURN self.array;
  END ArrayObtain;

PROCEDURE ReplArrayObtain (self: ReplArray): REF Vals =
  BEGIN
    RETURN self.array;
  END ReplArrayObtain; 

PROCEDURE SimpleArrayObtain (self: SimpleArray): REF Vals =
  BEGIN
    RETURN self.array;
  END SimpleArrayObtain;

PROCEDURE ArrayCat (vals1, vals2: REF Vals): Val =
  VAR
    len1, len2: INTEGER;
    vals      : REF Vals;
  BEGIN
    len1 := NUMBER(vals1^);
    len2 := NUMBER(vals2^);
    vals := NEW(REF Vals, len1 + len2);
    FOR i := 0 TO len1 - 1 DO vals^[i] := vals1^[i]; END;
    FOR i := 0 TO len2 - 1 DO vals^[len1 + i] := vals2^[i]; END;
    RETURN NEW(ValRemArray, remote := NEW(RemArrayServer, array := vals));
  END ArrayCat;

PROCEDURE ReplArrayCat (vals1, vals2: REF Vals): Val RAISES {SharedObj.Error} =
  VAR
    len1, len2: INTEGER;
    vals      : REF Vals;
  BEGIN
    len1 := NUMBER(vals1^);
    len2 := NUMBER(vals2^);
    vals := NEW(REF Vals, len1 + len2);
    FOR i := 0 TO len1 - 1 DO vals^[i] := vals1^[i]; END;
    FOR i := 0 TO len2 - 1 DO vals^[len1 + i] := vals2^[i]; END;
    WITH rep = NEW(ValReplArray, replica := NEW(ReplArrayStd, 
                                                array:=vals).init()) DO
      RETURN rep;
    END;
  END ReplArrayCat; 

PROCEDURE SimpleArrayCat (vals1, vals2: REF Vals): Val =
  VAR
    len1, len2: INTEGER;
    vals      : REF Vals;
  BEGIN
    len1 := NUMBER(vals1^);
    len2 := NUMBER(vals2^);
    vals := NEW(REF Vals, len1 + len2);
    FOR i := 0 TO len1 - 1 DO vals^[i] := vals1^[i]; END;
    FOR i := 0 TO len2 - 1 DO vals^[len1 + i] := vals2^[i]; END;
    RETURN NEW(ValSimpleArray, simple := NEW(SimpleArray, array:=vals));
  END SimpleArrayCat; 

PROCEDURE ReplArrayInit (self: ReplArray): ReplArray =
  BEGIN
    RETURN self;
  END ReplArrayInit; 

(***************************
 object fields
 ***************************)
PROCEDURE ObjWho (            self                 : RemObjServer;
                  VAR (*out*) protected, serialized: BOOLEAN       ):
  TEXT =
  BEGIN
    protected := self.protected;
    serialized := self.sync # NIL;
    RETURN self.who;
  END ObjWho;

PROCEDURE ReplObjWho (self: ReplObj; VAR (*out*) protected: BOOLEAN):
  TEXT =
  BEGIN
    protected := self.protected;
    RETURN self.who;
  END ReplObjWho;

PROCEDURE SimpleObjWho (            self                 : SimpleObj;
                        VAR (*out*) protected, serialized: BOOLEAN    ):
  TEXT =
  BEGIN
    protected := self.protected;
    serialized := self.sync # NIL;
    RETURN self.who;
  END SimpleObjWho;

PROCEDURE ObjEqual (v1, v2: ValObj): BOOLEAN =
  BEGIN
    IF v1 = NIL OR v2 = NIL THEN RETURN v1 = v2 END;

    TYPECASE v1 OF
    | ValRemObj (node1) =>
        TYPECASE v2 OF
        | ValRemObj (node2) => RETURN node1.remote = node2.remote;
        ELSE
          RETURN FALSE;
        END;
    | ValReplObj (node1) =>
        TYPECASE v2 OF
        | ValReplObj (node2) => RETURN node1.replica = node2.replica;
        ELSE
          RETURN FALSE;
        END;
    | ValSimpleObj (node1) =>
        TYPECASE v2 OF
        | ValSimpleObj (node2) => RETURN node1.simple = node2.simple;
        ELSE
          RETURN FALSE;
        END;
    ELSE
      RETURN FALSE;
    END;
  END ObjEqual;

PROCEDURE ObjClone1 (valObj: ValObj; mySelf: ValObj): ValObj
  RAISES {ServerError, NetObj.Error, SharedObj.Error, Thread.Alerted} =
  VAR who: TEXT;
  VAR fieldsOf1: REF ObjFields;
  VAR
    resSize  : INTEGER;
    resFields: REF ObjFields;
  VAR
    protected, serialized: BOOLEAN;
    sync                 : Sync;
  BEGIN
    who := valObj.Who( (*out*)protected, (*out*) serialized);
    IF Text.Empty(who) THEN who := "someone" END;
    fieldsOf1 := valObj.Obtain(ObjEqual(valObj, mySelf));
    resSize := NUMBER(fieldsOf1^);
    resFields := NEW(REF ObjFields, resSize);
    resFields^ := fieldsOf1^;
    IF serialized THEN
      sync := NEW(Sync, mutex := NEW(Thread.Mutex))
    ELSE
      sync := NIL
    END;
    TYPECASE valObj OF
    | ValRemObj =>
        WITH res = NEW(RemObjServer, who := "clone of " & who,
                       self := NEW(ValRemObj, remote := NIL),
                       fields := resFields, protected := protected,
                       sync := sync) DO
          res.self.remote := res;
          RETURN res.self;
        END;
    | ValReplObj =>
        WITH res = NEW(ReplObjStd, who := "", protected := protected,
                       self := NEW(ValReplObj, replica := NIL),
                       fields := resFields).init() DO
          res.self.replica := res;
          RETURN res.self;
        END;
    | ValSimpleObj =>
        WITH res = NEW(SimpleObj, who := "", 
                       self := NEW(ValSimpleObj, simple := NIL),
                       fields := resFields, protected := protected,
                       sync := sync) DO
          res.self.simple := res;
          RETURN res.self;
        END;
    ELSE                         <*ASSERT FALSE*>
    END;
  END ObjClone1;

PROCEDURE ObjClone (READONLY valObjs: ARRAY OF ValObj; mySelf: ValObj):
  ValObj
  RAISES {ServerError, NetObj.Error, Thread.Alerted, SharedObj.Error} =
  VAR resWho := "";
  VAR remWho: TEXT;
  VAR someunnamed := FALSE;
  VAR fieldsOfN: REF ARRAY OF REF ObjFields;
  VAR setWho := FALSE;
  VAR
    resSize, k          : INTEGER;
    ithFields, resFields: REF ObjFields;
  VAR
    protected, protected1, serialized, serialized1: BOOLEAN;
    sync                                          : Sync;
  BEGIN
    (* First, check to make sure they are all the same type *)
    TYPECASE valObjs[0] OF
    | ValRemObj =>
        FOR i := 1 TO NUMBER(valObjs) - 1 DO
          TYPECASE valObjs[i] OF
            ValRemObj =>         (* ok *)
          ELSE
            RaiseServerError(
              "Objects to be cloned must be of the same type");
          END;
        END;
        setWho := TRUE;
    | ValReplObj =>
        FOR i := 1 TO NUMBER(valObjs) - 1 DO
          TYPECASE valObjs[i] OF
            ValReplObj =>        (* ok *)
          ELSE
            RaiseServerError(
              "Objects to be cloned must be of the same type");
          END;
        END;
    | ValSimpleObj =>
        FOR i := 1 TO NUMBER(valObjs) - 1 DO
          TYPECASE valObjs[i] OF
            ValSimpleObj =>      (* ok *)
          ELSE
            RaiseServerError(
              "Objects to be cloned must be of the same type");
          END;
        END;
    ELSE
      RaiseServerError("Arguments of clone must be objects");
    END;

    IF setWho THEN resWho := "clone of" END;
    protected := FALSE;
    serialized := FALSE;
    fieldsOfN := NEW(REF ARRAY OF REF ObjFields, NUMBER(valObjs));
    FOR i := 0 TO NUMBER(valObjs) - 1 DO
      remWho := valObjs[i].Who( (*out*)protected1, (*out*) serialized1);
      IF i = 0 THEN
        protected := protected1;
        serialized := serialized1;
      END;
      IF setWho THEN
        IF Text.Empty(remWho) THEN 
          someunnamed := TRUE;
        ELSE
          resWho := resWho & " " & remWho;
        END;
      END;
      fieldsOfN^[i] := valObjs[i].Obtain(ObjEqual(valObjs[i], mySelf));
    END;
    IF setWho AND someunnamed THEN resWho := resWho & " someone" END;
    resSize := 0;
    FOR i := 0 TO NUMBER(fieldsOfN^) - 1 DO
      ithFields := fieldsOfN^[i];
      INC(resSize, NUMBER(ithFields^));
    END;
    resFields := NEW(REF ObjFields, resSize);
    k := 0;
    FOR i := 0 TO NUMBER(fieldsOfN^) - 1 DO
      ithFields := fieldsOfN^[i];
      FOR j := 0 TO NUMBER(ithFields^) - 1 DO
        resFields^[k] := ithFields^[j];
        INC(k);
      END;
    END;
    IF NUMBER(fieldsOfN^) > 1 THEN
      ObFieldArraySort.Sort(resFields^);
      (* Since they are sorted, we can just see if any pair are the same!
      FOR i := 0 TO resSize - 1 DO
        FOR j := i + 1 TO resSize - 1 DO
          IF Text.Equal(resFields^[i].label, resFields^[j].label) THEN
            RaiseServerError(
              "duplicated field on cloning: " & resFields^[i].label);
          END;
        END;
      END;
      *)
      FOR i := 0 TO resSize - 2 DO
        IF Text.Equal(resFields^[i].label, resFields^[i+1].label) THEN
          RaiseServerError(
              "duplicated field on cloning: " & resFields^[i].label);
        END;
      END;
    END;

    IF serialized THEN
      sync := NEW(Sync, mutex := NEW(Thread.Mutex))
    ELSE
      sync := NIL
    END;
    TYPECASE valObjs[0] OF
    | ValRemObj =>
        WITH res = NEW(RemObjServer, who := resWho,
                       self := NEW(ValRemObj, remote := NIL),
                       fields := resFields, protected := protected,
                       sync := sync) DO
          res.self.remote := res;
          RETURN res.self;
        END;
    | ValReplObj =>
        WITH res = NEW(ReplObjStd, who := resWho, protected := protected,
                       self := NEW(ValReplObj, replica := NIL),
                       fields := resFields).init() DO
          res.self.replica := res;
          RETURN res.self;
        END;
    | ValSimpleObj =>
        WITH res = NEW(SimpleObj, who := resWho,
                       self := NEW(ValSimpleObj, simple := NIL),
                       fields := resFields, protected := protected,
                       sync := sync) DO
          res.self.simple := res;
          RETURN res.self;
        END;
    ELSE                         <*ASSERT FALSE*>
    END;
  END ObjClone;

<*INLINE*> 
PROCEDURE FindField (label: TEXT; fields: REF ObjFields; VAR hint: INTEGER):
  Val RAISES {ServerError} =
  VAR fieldIndex := -1;
  BEGIN
    WITH fieldsNo = NUMBER(fields^) DO
      IF (hint >= 0) AND (hint < fieldsNo)
           AND Text.Equal(label, fields^[hint].label) THEN
        (* use hint as is *)
      ELSE
        FOR i := 0 TO fieldsNo - 1 DO
          IF Text.Equal(label, fields^[i].label) THEN
            fieldIndex := i;
            EXIT;
          END;
        END;
        IF fieldIndex = -1 THEN
          RaiseServerError("Field not found in object: " & label);
        END;
        hint := fieldIndex;
      END;
    END;
    RETURN fields^[hint].field;
  END FindField;

PROCEDURE ObjSelect (               self    : RemObjServer;
                                    swr     : SynWr.T;
                                    label   : TEXT;
                                    internal: BOOLEAN;
                     VAR (*in-out*) hint    : INTEGER       ): Val
  RAISES {ServerError, Error, Exception, SharedObj.Error, NetObj.Error,
          Thread.Alerted} =
  VAR
    lock    : BOOLEAN;
    fields  : REF ObjFields;
    newEnv  : Env;
    fieldVal: Val;
    objMu   : Thread.Mutex;
  BEGIN
    lock := (NOT internal) AND (self.sync # NIL);
    IF lock THEN objMu := self.sync.mutex; Thread.Acquire(objMu) END;
    TRY

      fields := self.fields;
      fieldVal := FindField(label, fields, hint);
      TYPECASE fieldVal OF
      | ValMeth (meth) =>
          (* Consider a method with zero parameters as a field. *)
          IF meth.meth.bindersNo - 1 # 0 THEN
            RaiseServerError(
              BadArgsNoMsg(meth.meth.bindersNo - 1, 0, "method", label));
          END;
          newEnv := NEW(LocalEnv, name := meth.meth.binders.first,
                        val := self.self, rest := NIL);
          RETURN ObEval.Term(swr, meth.meth.body, (*in-out*) newEnv,
                             meth.global, self.self);
      | ValAlias (alias) =>
          RETURN
            alias.obj.Select(swr, alias.label, ObjEqual(alias.obj, self.self),
                             (*var*) alias.labelIndexHint);
      ELSE
        RETURN fieldVal;
      END;

    FINALLY
      IF lock THEN Thread.Release(objMu) END;
    END;
  END ObjSelect;

PROCEDURE SimpleObjSelect (    self    : SimpleObj;
                               swr     : SynWr.T;
                               label   : TEXT;
                               internal: BOOLEAN;
                           VAR hint    : INTEGER    ): Val
  RAISES {ServerError, Error, Exception, SharedObj.Error, NetObj.Error,
          Thread.Alerted} =
  VAR
    lock    : BOOLEAN;
    fields  : REF ObjFields;
    newEnv  : Env;
    fieldVal: Val;
    objMu   : Thread.Mutex;
  BEGIN
    lock := (NOT internal) AND (self.sync # NIL);
    IF lock THEN objMu := self.sync.mutex; Thread.Acquire(objMu) END;
    TRY

      fields := self.fields;
      fieldVal := FindField(label, fields, hint);
      TYPECASE fieldVal OF
      | ValMeth (meth) =>
          (* Consider a method with zero parameters as a field. *)
          IF meth.meth.bindersNo - 1 # 0 THEN
            RaiseServerError(
              BadArgsNoMsg(meth.meth.bindersNo - 1, 0, "method", label));
          END;
          newEnv := NEW(LocalEnv, name := meth.meth.binders.first,
                        val := self.self, rest := NIL);
          RETURN ObEval.Term(swr, meth.meth.body, (*in-out*) newEnv,
                             meth.global, self.self);
      | ValAlias (alias) =>
          RETURN alias.obj.Select(
                   swr, alias.label, ObjEqual(alias.obj, self.self),
                   (*var*) alias.labelIndexHint);
      ELSE
        RETURN fieldVal;
      END;

    FINALLY
      IF lock THEN Thread.Release(objMu) END;
    END;
  END SimpleObjSelect;

PROCEDURE ReplObjSelect (    self : ReplObj;
                             swr  : SynWr.T;
                             label: TEXT;
                         VAR hint : INTEGER  ): Val
  RAISES {Error, Exception, ServerError, SharedObj.Error} =
  VAR
    fields        := self.fields;
    newEnv  : Env;
    fieldVal: Val;
  BEGIN
    fieldVal := FindField(label, fields, hint);
    TYPECASE fieldVal OF
    | ValMeth (meth) =>
        (* Consider a method with zero parameters as a field. *)
        IF meth.meth.bindersNo - 1 # 0 THEN
          RaiseServerError(
            BadArgsNoMsg(meth.meth.bindersNo - 1, 0, "method", label));
        END;
        (* If it is not an update method, we can execute it here.  If it is
           an update method, we must call InvokeUpdate *)
        IF meth.meth.update THEN
          VAR args := ARRAY [0 .. 0] OF Val{NIL};
          BEGIN
            RETURN self.InvokeUpdate(swr, label, 0, args, hint);
          END;
        ELSE
          newEnv := NEW(LocalEnv, name := meth.meth.binders.first,
                        val := self.self, rest := NIL);
          RETURN ObEval.Term(swr, meth.meth.body, (*in-out*) newEnv,
                             meth.global, self.self);
        END;
    | ValAlias (alias) =>
      TRY
        RETURN alias.obj.Select(
                         swr, alias.label, ObjEqual(alias.obj, self.self),
                         (*var*) alias.labelIndexHint);
      EXCEPT
      | NetObj.Error, SharedObj.Error, Thread.Alerted =>
        RaiseServerError("on remote object access through alias");
        <*ASSERT FALSE*>
      END;
      (*
        RaiseServerError(
          "Unexpected Alias field in replicated object: " & label);
          <*ASSERT FALSE*>
      *)
    ELSE
      RETURN fieldVal;
    END;
  END ReplObjSelect;

(*
PROCEDURE LinearFieldsHave (fields: REF ObjFields; label: TEXT; 
                            i, len: INTEGER; VAR hint: INTEGER):
  BOOLEAN =
  BEGIN
    WHILE len > 0 DO
      IF Text.Equal(label, fields^[i].label) THEN
        hint := i;
        RETURN TRUE;
      END;
      INC(i);
      DEC(len);
    END;
    RETURN FALSE;
  END LinearFieldsHave; 
*)

(* use binary search *)
PROCEDURE FieldsHave (fields: REF ObjFields; label: TEXT; VAR hint: INTEGER):
  BOOLEAN =
  VAR left := 0;
      size := NUMBER(fields^);
      rightSize, middle: INTEGER;
  BEGIN
    LOOP
      IF size <= 1 THEN
        IF size = 0 THEN RETURN FALSE END;
        IF Text.Equal(label, fields^[left].label) THEN
          hint := left;
          RETURN TRUE;
        END;
        RETURN FALSE;
      END;

      IF size MOD 2 = 0 THEN
        (* if size is even, then rightsize is (size DIV 2)-1, which
           represents the number of elements to the right of middle. *)
        size := size DIV 2;
        rightSize := size - 1;
      ELSE
        (* if size is odd, then rightsize is (size DIV 2), since there
           is the same number of elements on both sides of middle. *)
        size := size DIV 2;
        rightSize := size;
      END;
      middle :=  size + left;
      CASE Text.Compare(label, fields^[middle].label) OF
      | -1 => (* do nothing *)
      | 0 => hint := middle; RETURN TRUE;
      | 1 => left := middle + 1; size := rightSize 
      ELSE <*ASSERT FALSE*> END;
    END;
  END FieldsHave;

PROCEDURE ObjHas (self: RemObjServer; label: TEXT; VAR hint: INTEGER):
  BOOLEAN =
  BEGIN
    RETURN FieldsHave(self.fields, label, hint);
  END ObjHas;

PROCEDURE ReplObjHas (self: ReplObj; label: TEXT; VAR hint: INTEGER):
  BOOLEAN =
  BEGIN
    RETURN FieldsHave(self.fields, label, hint);
  END ReplObjHas;

PROCEDURE SimpleObjHas (self: SimpleObj; label: TEXT; VAR hint: INTEGER):
  BOOLEAN =
  BEGIN
    RETURN FieldsHave(self.fields, label, hint);
  END SimpleObjHas;

PROCEDURE ObjInvoke (               self    : RemObjServer;
                                    swr     : SynWr.T;
                                    label   : TEXT;
                                    argsNo  : INTEGER;
                     READONLY       args    : Vals;
                                    internal: BOOLEAN;
                     VAR (*in-out*) hint    : INTEGER       ): Val
  RAISES {ServerError, Error, Exception, SharedObj.Error, NetObj.Error,
          Thread.Alerted} =
  VAR
    lock      : BOOLEAN;
    fields    : REF ObjFields;
    binderList: ObTree.IdeList;
    newEnv    : Env;
    fieldVal  : Val;
    objMu     : Thread.Mutex;
  BEGIN
    lock := (NOT internal) AND (self.sync # NIL);
    IF lock THEN objMu := self.sync.mutex; Thread.Acquire(objMu) END;
    TRY

      fields := self.fields;
      fieldVal := FindField(label, fields, hint);
      TYPECASE fieldVal OF
      | ValMeth (meth) =>
          IF meth.meth.bindersNo - 1 # argsNo THEN
            RaiseServerError(BadArgsNoMsg(meth.meth.bindersNo - 1, argsNo,
                                          "method", label));
          END;
          binderList := meth.meth.binders;
          newEnv := NEW(LocalEnv, name := binderList.first,
                        val := self.self, rest := NIL);
          binderList := binderList.rest;
          FOR i := 0 TO argsNo - 1 DO
            newEnv := NEW(LocalEnv, name := binderList.first,
                          val := args[i], rest := newEnv);
            binderList := binderList.rest;
          END;
          RETURN ObEval.Term(swr, meth.meth.body, (*in-out*) newEnv,
                             meth.global, self.self);
      | ValAlias (alias) =>
          RETURN alias.obj.Invoke(swr, alias.label, argsNo, args,
                                  ObjEqual(alias.obj, self.self),
                                  (*var*) alias.labelIndexHint);
      ELSE
        RaiseServerError("Field used as a method: " & label);
        <*ASSERT FALSE*>
      END;
    FINALLY
      IF lock THEN Thread.Release(objMu) END;
    END;
  END ObjInvoke;

PROCEDURE SimpleObjInvoke (         self    : SimpleObj;
                                    swr     : SynWr.T;
                                    label   : TEXT;
                                    argsNo  : INTEGER;
                           READONLY args    : Vals;
                                    internal: BOOLEAN;
                           VAR      hint    : INTEGER    ): Val
  RAISES {ServerError, Error, Exception, SharedObj.Error, NetObj.Error,
          Thread.Alerted} =
  VAR
    lock      : BOOLEAN;
    fields    : REF ObjFields;
    binderList: ObTree.IdeList;
    newEnv    : Env;
    fieldVal  : Val;
    objMu     : Thread.Mutex;
  BEGIN
    lock := (NOT internal) AND (self.sync # NIL);
    IF lock THEN objMu := self.sync.mutex; Thread.Acquire(objMu) END;
    TRY

      fields := self.fields;
      fieldVal := FindField(label, fields, hint);
      TYPECASE fieldVal OF
      | ValMeth (meth) =>
          IF meth.meth.bindersNo - 1 # argsNo THEN
            RaiseServerError(BadArgsNoMsg(meth.meth.bindersNo - 1, argsNo,
                                          "method", label));
          END;
          binderList := meth.meth.binders;
          newEnv := NEW(LocalEnv, name := binderList.first,
                        val := self.self, rest := NIL);
          binderList := binderList.rest;
          FOR i := 0 TO argsNo - 1 DO
            newEnv := NEW(LocalEnv, name := binderList.first,
                          val := args[i], rest := newEnv);
            binderList := binderList.rest;
          END;
          RETURN ObEval.Term(swr, meth.meth.body, (*in-out*) newEnv,
                             meth.global, self.self);
      | ValAlias (alias) =>
          RETURN alias.obj.Invoke(swr, alias.label, argsNo, args,
                                  ObjEqual(alias.obj, self.self),
                                  (*var*) alias.labelIndexHint);
      ELSE
        RaiseServerError("Field used as a method: " & label);
        <*ASSERT FALSE*>
      END;
    FINALLY
      IF lock THEN Thread.Release(objMu) END;
    END;
  END SimpleObjInvoke;

PROCEDURE ReplObjInvoke (         self  : ReplObj;
                                  swr   : SynWr.T;
                                  label : TEXT;
                                  argsNo: INTEGER;
                         READONLY args  : Vals;
                         VAR      hint  : INTEGER  ): Val
  RAISES {Error, Exception, ServerError, SharedObj.Error} =
  VAR
    fields    : REF ObjFields;
    binderList: ObTree.IdeList;
    newEnv    : Env;
    fieldVal  : Val;
  BEGIN
    fields := self.fields;
    fieldVal := FindField(label, fields, hint);

    TYPECASE fieldVal OF
    | ValMeth (meth) =>
        (* If it's an update method, do perform the update instead *)
        IF meth.meth.update THEN
          RETURN self.InvokeUpdate(swr, label, argsNo, args, hint);
        END;

        IF meth.meth.bindersNo - 1 # argsNo THEN
          RaiseServerError(
            BadArgsNoMsg(meth.meth.bindersNo - 1, argsNo, "method", label));
        END;
        binderList := meth.meth.binders;
        newEnv := NEW(LocalEnv, name := binderList.first, val := self.self,
                      rest := NIL);
        binderList := binderList.rest;
        FOR i := 0 TO argsNo - 1 DO
          newEnv := NEW(LocalEnv, name := binderList.first, val := args[i],
                        rest := newEnv);
          binderList := binderList.rest;
        END;
        RETURN ObEval.Term(swr, meth.meth.body, (*in-out*) newEnv,
                           meth.global, self.self);
    | ValAlias (alias) =>
      TRY
        RETURN alias.obj.Invoke(swr, alias.label, argsNo, args,
                                ObjEqual(alias.obj, self.self),
                                (*var*) alias.labelIndexHint);
      EXCEPT
      | NetObj.Error, SharedObj.Error, Thread.Alerted =>
        RaiseServerError("on object invocation through alias");
        <*ASSERT FALSE*>
      END;
      (*
        RaiseServerError(
          "Unexpected Alias field in replicated object: " & label);
        <*ASSERT FALSE*>
      *)
    ELSE
      RaiseServerError("Field used as a method: " & label);
      <*ASSERT FALSE*>
    END;
  END ReplObjInvoke;

PROCEDURE ReplObjInvokeUpdate (         self  : ReplObj;
                                        swr   : SynWr.T;
                                        label : TEXT;
                                        argsNo: INTEGER;
                               READONLY args  : Vals;
                               VAR      hint  : INTEGER  ): Val
  RAISES {Error, Exception, ServerError} =
  VAR
    fields    : REF ObjFields;
    binderList: ObTree.IdeList;
    newEnv    : Env;
    fieldVal  : Val;
  BEGIN
    fields := self.fields;
    fieldVal := FindField(label, fields, hint);

    TYPECASE fieldVal OF
    | ValMeth (meth) =>
        IF meth.meth.bindersNo - 1 # argsNo THEN
          RaiseServerError(
            BadArgsNoMsg(meth.meth.bindersNo - 1, argsNo, "method", label));
        END;
        binderList := meth.meth.binders;
        newEnv := NEW(LocalEnv, name := binderList.first, val := self.self,
                      rest := NIL);
        binderList := binderList.rest;
        FOR i := 0 TO argsNo - 1 DO
          newEnv := NEW(LocalEnv, name := binderList.first, val := args[i],
                        rest := newEnv);
          binderList := binderList.rest;
        END;
        RETURN ObEval.Term(swr, meth.meth.body, (*in-out*) newEnv,
                           meth.global, self.self);
    | ValAlias =>
        RaiseServerError(
          "Unexpected Alias field in replicated object: " & label);
      <*ASSERT FALSE*>(* should never happen *)
    ELSE
      RaiseServerError("Field used as a method: " & label);
      <*ASSERT FALSE*>
    END;
  END ReplObjInvokeUpdate;

PROCEDURE ObjUpdate (               self    : RemObjServer;
                                    label   : TEXT;
                                    val     : Val;
                                    internal: BOOLEAN;
                     VAR (*in-out*) hint    : INTEGER       )
  RAISES {ServerError, SharedObj.Error, NetObj.Error, Thread.Alerted} =
  VAR
    lock  : BOOLEAN;
    fields: REF ObjFields;
    objMu : Thread.Mutex;
  BEGIN
    lock := (NOT internal) AND (self.sync # NIL);
    IF lock THEN objMu := self.sync.mutex; Thread.Acquire(objMu) END;
    TRY

      IF self.protected AND (NOT internal) THEN
        RaiseServerError("Cannot update protected object");
      END;
      fields := self.fields;
      EVAL FindField(label, fields, hint);

      TYPECASE fields^[hint].field OF
      | ValAlias (alias) =>
          TYPECASE val OF
          | ValAlias => fields^[hint].field := val
          ELSE
            alias.obj.Update(
              alias.label, val, ObjEqual(alias.obj, self.self),
              (*var*) alias.labelIndexHint);
          END;
      ELSE
        fields^[hint].field := val;
      END;

    FINALLY
      IF lock THEN Thread.Release(objMu) END;
    END;
  END ObjUpdate;

PROCEDURE SimpleObjUpdate (    self    : SimpleObj;
                               label   : TEXT;
                               val     : Val;
                               internal: BOOLEAN;
                           VAR hint    : INTEGER    )
  RAISES {ServerError, SharedObj.Error, NetObj.Error, Thread.Alerted} =
  VAR
    lock  : BOOLEAN;
    fields: REF ObjFields;
    objMu : Thread.Mutex;
  BEGIN
    lock := (NOT internal) AND (self.sync # NIL);
    IF lock THEN objMu := self.sync.mutex; Thread.Acquire(objMu) END;
    TRY

      IF self.protected AND (NOT internal) THEN
        RaiseServerError("Cannot update protected object");
      END;
      fields := self.fields;
      EVAL FindField(label, fields, hint);

      TYPECASE fields^[hint].field OF
      | ValAlias (alias) =>
          TYPECASE val OF
          | ValAlias => fields^[hint].field := val
          ELSE
            alias.obj.Update(
              alias.label, val, ObjEqual(alias.obj, self.self),
              (*var*) alias.labelIndexHint);
          END;
      ELSE
        fields^[hint].field := val;
      END;

    FINALLY
      IF lock THEN Thread.Release(objMu) END;
    END;
  END SimpleObjUpdate;

PROCEDURE ReplObjUpdate (    self    : ReplObj;
                             label   : TEXT;
                             val     : Val;
                             internal: BOOLEAN;
                         VAR hint    : INTEGER  ) 
  RAISES {ServerError} =
  VAR fields: REF ObjFields;
  BEGIN
    IF self.protected AND (NOT internal) THEN
      RaiseServerError("Cannot update protected object");
    END;

    (*
    TYPECASE val OF
    | ValAlias =>
        RaiseServerError("Cannot alias fields in a replicated object");
    ELSE
    END;
    *)
    fields := self.fields;
    EVAL FindField(label, fields, hint);

    TYPECASE fields^[hint].field OF
    | ValAlias(alias) =>
        TYPECASE val OF
        | ValAlias => fields^[hint].field := val
        ELSE
          TRY
            alias.obj.Update(
                      alias.label, val, ObjEqual(alias.obj, self.self),
                      (*var*) alias.labelIndexHint);
          EXCEPT
          | NetObj.Error, SharedObj.Error, Thread.Alerted =>
            RaiseServerError("on object update through alias");
            <*ASSERT FALSE*>
          END;
        END;
        (*
        RaiseServerError(
          "Unexpected Alias field in replicated object: " & label);
          <* ASSERT FALSE *>(* should be impossible *)
        *)
    ELSE
      fields^[hint].field := val;
    END;
  END ReplObjUpdate;

PROCEDURE ObjRedirect (self: RemObjServer; val: Val; internal: BOOLEAN)
  RAISES {ServerError, SharedObj.Error, NetObj.Error, Thread.Alerted} =
  VAR
    lock             : BOOLEAN;
    fields, newFields: REF ObjFields;
    fieldsNo         : INTEGER;
    label            : TEXT;
    hint             : INTEGER;
    objMu            : Thread.Mutex;
    valObj           : ValObj;
  BEGIN
    TYPECASE val OF
      ValObj (vo) => valObj := vo
    ELSE
      RaiseServerError("Redirection target must be an object");
    END;

    lock := (NOT internal) AND (self.sync # NIL);
    IF lock THEN objMu := self.sync.mutex; Thread.Acquire(objMu) END;
    TRY
      IF self.protected AND (NOT internal) THEN
        RaiseServerError("Cannot redirect protected object");
      END;
      fields := self.fields;
      fieldsNo := NUMBER(fields^);
      newFields := NEW(REF ObjFields, fieldsNo);
      FOR i := 0 TO fieldsNo - 1 DO
        label := fields^[i].label;
        newFields^[i].label := label;
        IF valObj.Has(label, (*in-out*) hint) THEN
          newFields^[i].field :=
            NEW(ValAlias, label := label, labelIndexHint := hint,
                obj := valObj);
        ELSE
          RaiseServerError(
            "Field not found in object on redirection: " & label);
        END;
        self.fields := newFields; (* atomic swap *)
      END;

    FINALLY
      IF lock THEN Thread.Release(objMu) END;
    END;
  END ObjRedirect;

PROCEDURE ReplObjRedirect (self: ReplObj; val: Val; internal: BOOLEAN)
  RAISES {ServerError} =
  VAR
    fields, newFields: REF ObjFields;
    fieldsNo         : INTEGER;
    label            : TEXT;
    hint             : INTEGER;
    valObj           : ValObj;
  BEGIN
    TYPECASE val OF
      ValObj (vo) => valObj := vo
    ELSE
      RaiseServerError("Redirection target must be an object");
    END;

    IF self.protected AND (NOT internal) THEN
      RaiseServerError("Cannot redirect protected object");
    END;
    fields := self.fields;
    fieldsNo := NUMBER(fields^);
    newFields := NEW(REF ObjFields, fieldsNo);
    FOR i := 0 TO fieldsNo - 1 DO
      label := fields^[i].label;
      newFields^[i].label := label;
      TRY
        IF valObj.Has(label, (*in-out*) hint) THEN
          newFields^[i].field :=
              NEW(ValAlias, label := label, labelIndexHint := hint,
                  obj := valObj);
        ELSE
          RaiseServerError(
              "Field not found in object on redirection: " & label);
        END;
        self.RedirectFields(newFields);
      EXCEPT
      | NetObj.Error, SharedObj.Error, Thread.Alerted =>
        RaiseServerError("on object access through alias");
        <*ASSERT FALSE*>
      END;
    END;
  END ReplObjRedirect;

PROCEDURE ReplObjRedirectFields (self: ReplObj; newFields: REF ObjFields) =
  BEGIN
    self.fields := newFields; (* atomic swap *)
  END ReplObjRedirectFields;
    
PROCEDURE SimpleObjRedirect (self: SimpleObj; val: Val; internal: BOOLEAN)
  RAISES {ServerError, SharedObj.Error, NetObj.Error, Thread.Alerted} =
  VAR
    lock             : BOOLEAN;
    fields, newFields: REF ObjFields;
    fieldsNo         : INTEGER;
    label            : TEXT;
    hint             : INTEGER;
    objMu            : Thread.Mutex;
    valObj           : ValObj;
  BEGIN
    TYPECASE val OF
      ValObj (vo) => valObj := vo
    ELSE
      RaiseServerError("Redirection target must be an object");
    END;

    lock := (NOT internal) AND (self.sync # NIL);
    IF lock THEN objMu := self.sync.mutex; Thread.Acquire(objMu) END;
    TRY
      IF self.protected AND (NOT internal) THEN
        RaiseServerError("Cannot redirect protected object");
      END;
      fields := self.fields;
      fieldsNo := NUMBER(fields^);
      newFields := NEW(REF ObjFields, fieldsNo);
      FOR i := 0 TO fieldsNo - 1 DO
        label := fields^[i].label;
        newFields^[i].label := label;
        IF valObj.Has(label, (*in-out*) hint) THEN
          newFields^[i].field :=
            NEW(ValAlias, label := label, labelIndexHint := hint,
                obj := valObj);
        ELSE
          RaiseServerError(
            "Field not found in object on redirection: " & label);
        END;
        self.fields := newFields; (* atomic swap *)
      END;

    FINALLY
      IF lock THEN Thread.Release(objMu) END;
    END;
  END SimpleObjRedirect;

PROCEDURE ObjObtain (self: RemObjServer; internal: BOOLEAN): REF ObjFields
  RAISES {ServerError} =
  VAR
    lock : BOOLEAN;
    objMu: Thread.Mutex;
  BEGIN
    lock := (NOT internal) AND (self.sync # NIL);
    IF lock THEN objMu := self.sync.mutex; Thread.Acquire(objMu) END;
    TRY
      IF self.protected AND (NOT internal) THEN
        RaiseServerError("Cannot obtain protected object");
      END;
      RETURN self.fields;

    FINALLY
      IF lock THEN Thread.Release(objMu) END;
    END;
  END ObjObtain;

PROCEDURE ReplObjObtain (self: ReplObj; internal: BOOLEAN): REF ObjFields
  RAISES {ServerError} =
  BEGIN
    IF self.protected AND (NOT internal) THEN
      RaiseServerError("Cannot obtain protected object");
    END;
    RETURN self.fields;
  END ReplObjObtain;

PROCEDURE SimpleObjObtain (self: SimpleObj; internal: BOOLEAN):
  REF ObjFields RAISES {ServerError} =
  VAR
    lock : BOOLEAN;
    objMu: Thread.Mutex;
  BEGIN
    lock := (NOT internal) AND (self.sync # NIL);
    IF lock THEN objMu := self.sync.mutex; Thread.Acquire(objMu) END;
    TRY
      IF self.protected AND (NOT internal) THEN
        RaiseServerError("Cannot obtain protected object");
      END;
      RETURN self.fields;

    FINALLY
      IF lock THEN Thread.Release(objMu) END;
    END;
  END SimpleObjObtain;

PROCEDURE ObjObtainField(self: RemObjServer; label: TEXT; 
                            internal: BOOLEAN): Val
  RAISES {ServerError} =
  VAR
    lock    : BOOLEAN;
    objMu   : Thread.Mutex;
    hint := -1;
  BEGIN
    lock := (NOT internal) AND (self.sync # NIL);
    IF lock THEN objMu := self.sync.mutex; Thread.Acquire(objMu) END;
    TRY
      IF self.protected AND (NOT internal) THEN
        RaiseServerError("Cannot obtain fields of protected object");
      END;
      RETURN FindField(label, self.fields, hint);
    FINALLY
      IF lock THEN Thread.Release(objMu) END;
    END;
  END ObjObtainField;

PROCEDURE ReplObjObtainField(self: ReplObj; label: TEXT; 
                             internal: BOOLEAN): Val
  RAISES {ServerError} =
  VAR hint := -1;
  BEGIN
    IF self.protected AND (NOT internal) THEN
      RaiseServerError("Cannot obtain fields of protected object");
    END;
    RETURN FindField(label, self.fields, hint);
  END ReplObjObtainField;
  
PROCEDURE SimpleObjObtainField(self: SimpleObj; label: TEXT; 
                               internal: BOOLEAN): Val
  RAISES {ServerError} =
  VAR
    lock    : BOOLEAN;
    objMu   : Thread.Mutex;
    hint := -1;
  BEGIN
    lock := (NOT internal) AND (self.sync # NIL);
    IF lock THEN objMu := self.sync.mutex; Thread.Acquire(objMu) END;
    TRY
      IF self.protected AND (NOT internal) THEN
        RaiseServerError("Cannot obtain fields of protected object");
      END;
      RETURN FindField(label, self.fields, hint);
    FINALLY
      IF lock THEN Thread.Release(objMu) END;
    END;
  END SimpleObjObtainField;
  
PROCEDURE ObjDescribe(self: RemObjServer; label: TEXT): TEXT
  RAISES {ServerError, SharedObj.Error, NetObj.Error, Thread.Alerted} =
  VAR
    lock    : BOOLEAN;
    fieldVal: Val;
    objMu   : Thread.Mutex;
    hint    := -1;
  BEGIN
    lock := self.sync # NIL;
    IF lock THEN objMu := self.sync.mutex; Thread.Acquire(objMu) END;
    TRY
      fieldVal := FindField(label, self.fields, hint);
      TYPECASE fieldVal OF
      | ValAlias (alias) => RETURN alias.obj.Describe(alias.label);
      ELSE
        RETURN GetTypeString(fieldVal);
      END;
    FINALLY
      IF lock THEN Thread.Release(objMu) END;
    END;
  END ObjDescribe; 

PROCEDURE ReplObjDescribe(self: ReplObj;
                          label: TEXT): TEXT
  RAISES {ServerError} =
  VAR
    fieldVal: Val;
    hint    := -1;
  BEGIN
    fieldVal := FindField(label, self.fields, hint);
    TYPECASE fieldVal OF
    | ValAlias (alias) => 
      TRY
        RETURN alias.obj.Describe(alias.label);
      EXCEPT
      | NetObj.Error, SharedObj.Error, Thread.Alerted =>
        RaiseServerError("on object access through alias");
        <*ASSERT FALSE*>
      END;
      (*RaiseServerError(
          "Unexpected Alias field in replicated object: " & label);
      <*ASSERT FALSE*>(* should never happen *)*)
    ELSE
      RETURN GetTypeString(fieldVal);
    END;
  END ReplObjDescribe;

PROCEDURE SimpleObjDescribe(self: SimpleObj;
                          label: TEXT): TEXT
  RAISES {ServerError, SharedObj.Error, NetObj.Error, Thread.Alerted} =
  VAR
    lock    : BOOLEAN;
    fieldVal: Val;
    objMu   : Thread.Mutex;
    hint    := -1;
  BEGIN
    lock := self.sync # NIL;
    IF lock THEN objMu := self.sync.mutex; Thread.Acquire(objMu) END;
    TRY
      fieldVal := FindField(label, self.fields, hint);
      TYPECASE fieldVal OF
      | ValAlias (alias) => RETURN alias.obj.Describe(alias.label);
      ELSE
        RETURN GetTypeString(fieldVal);
      END;
    FINALLY
      IF lock THEN Thread.Release(objMu) END;
    END;
  END SimpleObjDescribe;

PROCEDURE ObjObtainDescriptions(self: RemObjServer): REF ObjFieldTypes 
  RAISES {ServerError, SharedObj.Error, NetObj.Error, Thread.Alerted} =
  VAR
    lock             : BOOLEAN;
    fields           : REF ObjFields;
    fieldsNo         : INTEGER;
    objMu            : Thread.Mutex;
    desc             : REF ObjFieldTypes := NIL;
  BEGIN
    lock := self.sync # NIL;
    IF lock THEN objMu := self.sync.mutex; Thread.Acquire(objMu) END;
    TRY
      fields := self.fields;
      fieldsNo := NUMBER(fields^);
      desc := NEW(REF ObjFieldTypes, fieldsNo);
      FOR i := 0 TO fieldsNo - 1 DO
        desc[i].label := fields^[i].label;
        TYPECASE fields[i].field OF
        | ValAlias (alias) => desc[i].type := alias.obj.Describe(alias.label);
        ELSE
          desc[i].type := GetTypeString(fields[i].field);
        END;
      END;
      RETURN desc;
    FINALLY
      IF lock THEN Thread.Release(objMu) END;
    END;
  END ObjObtainDescriptions;

PROCEDURE ReplObjObtainDescriptions(self: ReplObj): REF ObjFieldTypes
  RAISES {ServerError} =
  VAR
    fields           : REF ObjFields;
    fieldsNo         : INTEGER;
    desc             : REF ObjFieldTypes := NIL;
  BEGIN
    fields := self.fields;
    fieldsNo := NUMBER(fields^);
    desc := NEW(REF ObjFieldTypes, fieldsNo);
    FOR i := 0 TO fieldsNo - 1 DO
      desc[i].label := fields^[i].label;
      TYPECASE fields[i].field OF
      | ValAlias (alias) => 
        TRY
          desc[i].type := alias.obj.Describe(alias.label);
        EXCEPT
        | NetObj.Error, SharedObj.Error, Thread.Alerted =>
          RaiseServerError("on object access through alias");
          <*ASSERT FALSE*>
        END;
        (*
        RaiseServerError(
            "Unexpected Alias field in replicated object: " & fields[i].label);
        <*ASSERT FALSE*>(* should never happen *)
        *)
      ELSE
        desc[i].type := GetTypeString(fields[i].field);
      END;
    END;
    RETURN desc;
  END ReplObjObtainDescriptions;

PROCEDURE SimpleObjObtainDescriptions(self: SimpleObj): REF ObjFieldTypes
  RAISES {ServerError, SharedObj.Error, NetObj.Error, Thread.Alerted} =
  VAR
    lock             : BOOLEAN;
    fields           : REF ObjFields;
    fieldsNo         : INTEGER;
    objMu            : Thread.Mutex;
    desc             : REF ObjFieldTypes := NIL;
  BEGIN
    lock := self.sync # NIL;
    IF lock THEN objMu := self.sync.mutex; Thread.Acquire(objMu) END;
    TRY
      fields := self.fields;
      fieldsNo := NUMBER(fields^);
      desc := NEW(REF ObjFieldTypes, fieldsNo);
      FOR i := 0 TO fieldsNo - 1 DO
        desc[i].label := fields^[i].label;
        TYPECASE fields[i].field OF
        | ValAlias (alias) => desc[i].type := alias.obj.Describe(alias.label);
        ELSE
          desc[i].type := GetTypeString(fields[i].field);
        END;
      END;
      RETURN desc;
    FINALLY
      IF lock THEN Thread.Release(objMu) END;
    END;
  END SimpleObjObtainDescriptions;

PROCEDURE ReplObjInit (self: ReplObj): ReplObj =
  BEGIN
    RETURN self;
  END ReplObjInit;

(*****************************************************
 * miscellaneous routines
 *****************************************************)
PROCEDURE SetObjPickler (obj       : ValObj;
                         picklerIn : ValSimpleObj;
                         picklerOut: ValSimpleObj;
                         mySelf    : ValObj        )
  RAISES {ServerError, NetObj.Error, SharedObj.Error, Thread.Alerted} =
  VAR
    objFields    := obj.Obtain(ObjEqual(obj, mySelf));
    pklInFields  := picklerIn.Obtain(ObjEqual(obj, mySelf));
    pklOutFields := picklerOut.Obtain(ObjEqual(obj, mySelf));

    inFields           := NEW(REF ObjFields, NUMBER(objFields^));
    outFields          := NEW(REF ObjFields, NUMBER(objFields^));
    numFields          := 0;
  BEGIN
    TYPECASE obj OF
    | ValSimpleObj, ValReplObj => (*ok*)
    ELSE
      RaiseServerError(
        "Can only set picklers for simple or " & "replicated objects");
    END;

    IF NUMBER(pklInFields^) # NUMBER(pklOutFields^) THEN
      RaiseServerError(
        "in and out pickler objects must have the same " & "set of fields");
    END;
    IF NUMBER(objFields^) < NUMBER(pklInFields^) THEN
      RaiseServerError("pickler objects have extra fields");
    END;

    (* the fields should all be sorted, which makes life easier *)
    FOR i := 0 TO NUMBER(objFields^) - 1 DO
      TYPECASE objFields[i].field OF
      | ValMeth, ValAlias =>               (* ignore *)
          IF numFields < NUMBER(pklInFields^) AND 
            (Text.Equal(pklInFields[numFields].label, objFields[i].label) OR
             Text.Equal(pklOutFields[numFields].label, objFields[i].label))THEN
            RaiseServerError(
              "field in pickle object corresponds to "
                & "method or alias field in object: " & objFields[i].label);
          END;
          (* put some dummy values for simplicity *)
          inFields[i].label := objFields[i].label;
          inFields[i].field := NIL;
          outFields[i].label := objFields[i].label;
          outFields[i].field := NIL;
      ELSE
        IF numFields >= NUMBER(pklInFields^) OR
          NOT Text.Equal(pklInFields[numFields].label,objFields[i].label) THEN
          RaiseServerError(
              "pickler 'in' object missing field: " & objFields[i].label);
        END;
        inFields[i].label := objFields[i].label;
        TYPECASE pklInFields[numFields].field OF
        | ValMeth (meth) =>
            IF meth.meth.bindersNo # 2 THEN
              RaiseServerError(
                BadArgsNoMsg(2, meth.meth.bindersNo, "pickle 'in' method",
                             objFields[i].label));
            END;
            inFields[i].field := meth;
        ELSE
          RaiseServerError(
            "pickler 'in' field must be a method: " & objFields[i].label);
        END;
        IF numFields >= NUMBER(pklOutFields^) OR
          NOT Text.Equal(pklOutFields[numFields].label,objFields[i].label) THEN
          RaiseServerError(
            "pickler 'out' object missing field: " & objFields[i].label);
        END;
        outFields[i].label := objFields[i].label;
        TYPECASE pklOutFields[numFields].field OF
        | ValMeth (meth) =>
            IF meth.meth.bindersNo # 2 THEN
              RaiseServerError(
                BadArgsNoMsg(2, meth.meth.bindersNo,
                             "pickler 'out' method", objFields[i].label));
            END;
            outFields[i].field := meth;
        ELSE
          RaiseServerError(
            "pickler 'out' field must be a method: " & objFields[i].label);
        END;
        INC(numFields);
      END;
    END;

    IF numFields # NUMBER(pklOutFields^) THEN
      RaiseServerError("extra fields in pickler objects");
    END;

    TYPECASE obj OF
    | ValSimpleObj (simple) =>
        simple.simple.pickleIn := inFields;
        simple.simple.pickleOut := outFields;
    | ValReplObj (repl) =>
        repl.replica.pickleIn := inFields;
        repl.replica.pickleOut := outFields;
    ELSE
      <*ASSERT FALSE*>
    END;
  END SetObjPickler;

PROCEDURE EngineWho (self: RemEngineServer): TEXT RAISES {} =
  BEGIN
    RETURN self.who;
  END EngineWho;

PROCEDURE EngineEval (self: RemEngineServer; proc: Val; mySelf: ValObj):
  Val RAISES {Error, Exception, ServerError} =
  VAR
    newEnv : Env;
    newGlob: GlobalEnv;
  BEGIN
    TYPECASE proc OF
    | ValFun (clos) =>
        IF 1 # clos.fun.bindersNo THEN
          RaiseServerError(
            "Engine needs a procedure of 1 argument as argument");
        END;
        newGlob := clos.global;
        newEnv := NEW(LocalEnv, name := clos.fun.binders.first,
                      val := self.arg, rest := NIL);
        RETURN ObEval.Term(Obliq.Console(), clos.fun.body, (*in-out*)
                           newEnv, newGlob, mySelf);
    ELSE
      RaiseServerError("Engine needs a procedure as argument");
      <*ASSERT FALSE*>
    END;
  END EngineEval;

PROCEDURE NewFileSystem (readOnly: BOOLEAN): ValFileSystem =
  BEGIN
    RETURN NEW(ValFileSystem, picklable := FALSE,
               what := "<FileSystem at " & machineAddress & ">",
               tag := "FileSystem",
               remote := NEW(RemFileSystemServer, readOnly := readOnly));
  END NewFileSystem;

PROCEDURE FileSystemIs (self: ValFileSystem; other: ValAnything): BOOLEAN =
  BEGIN
    TYPECASE other OF
    | ValFileSystem (oth) => RETURN self.remote = oth.remote;
    ELSE
      RETURN FALSE;
    END;
  END FileSystemIs;

PROCEDURE IteratorIs(self: ValIterator; other: ValAnything): BOOLEAN =
  BEGIN
    TYPECASE other OF
    | ValIterator (oth) => RETURN self.remote = oth.remote;
    ELSE
      RETURN FALSE;
    END;
  END IteratorIs; 

PROCEDURE FileSystemOpenRead (<*UNUSED*> self    : RemFileSystemServer;
                                         fileName: TEXT                 ):
  Rd.T RAISES {ServerError} =
  BEGIN
    TRY
      RETURN FileRd.Open(fileName);
    EXCEPT
    | OSError.E => RaiseServerError("FileSystemOpenRead");
      <*ASSERT FALSE*>
    END;
  END FileSystemOpenRead;

PROCEDURE FileSystemOpenWrite (self: RemFileSystemServer; fileName: TEXT):
  Wr.T RAISES {ServerError} =
  BEGIN
    IF self.readOnly THEN RaiseServerError("FileSystemOpenWrite") END;
    TRY
      RETURN FileWr.Open(fileName);
    EXCEPT
    | OSError.E => RaiseServerError("FileSystemOpenWrite");
      <*ASSERT FALSE*>
    END;
  END FileSystemOpenWrite;

PROCEDURE FileSystemOpenAppend (self: RemFileSystemServer; fileName: TEXT):
  Wr.T RAISES {ServerError} =
  BEGIN
    IF self.readOnly THEN RaiseServerError("FileSystemOpenAppend") END;
    TRY
      RETURN FileWr.OpenAppend(fileName);
    EXCEPT
    | OSError.E => RaiseServerError("FileSystemOpenAppend");
      <*ASSERT FALSE*>
    END;
  END FileSystemOpenAppend;

PROCEDURE FileSystemGetAbsolutePathname (<*UNUSED*>self: RemFileSystemServer; 
                                         fileName: TEXT):
  TEXT RAISES {ServerError} =
  BEGIN
    TRY
      RETURN FS.GetAbsolutePathname(fileName);
    EXCEPT
    | OSError.E => RaiseServerError("GetAbsolutePathname");
      <*ASSERT FALSE*>
    END;
  END FileSystemGetAbsolutePathname;

PROCEDURE FileSystemCreateDirectory (self: RemFileSystemServer; fileName: TEXT)
  RAISES {ServerError} =
  BEGIN
    IF self.readOnly THEN RaiseServerError("CreateDirectory") END;
    TRY
      FS.CreateDirectory(fileName);
    EXCEPT
    | OSError.E => RaiseServerError("CreateDirectory");
      <*ASSERT FALSE*>
    END;
  END FileSystemCreateDirectory;

PROCEDURE FileSystemDeleteDirectory (self: RemFileSystemServer; fileName: TEXT)
  RAISES {ServerError} =
  BEGIN
    IF self.readOnly THEN RaiseServerError("DeleteDirectory") END;
    TRY
      FS.DeleteDirectory(fileName);
    EXCEPT
    | OSError.E => RaiseServerError("DeleteDirectory");
      <*ASSERT FALSE*>
    END;
  END FileSystemDeleteDirectory;

PROCEDURE FileSystemDeleteFile (self: RemFileSystemServer; fileName: TEXT)
  RAISES {ServerError} =
  BEGIN
    IF self.readOnly THEN RaiseServerError("DeleteFile") END;
    TRY
      FS.DeleteFile(fileName);
    EXCEPT
    | OSError.E => RaiseServerError("DeleteFile");
      <*ASSERT FALSE*>
    END;
  END FileSystemDeleteFile;

PROCEDURE FileSystemRename (self: RemFileSystemServer; fileName1,fileName2: TEXT)
  RAISES {ServerError} =
  BEGIN
    IF self.readOnly THEN RaiseServerError("Rename") END;
    TRY
      FS.Rename(fileName1, fileName2);
    EXCEPT
    | OSError.E => RaiseServerError("Rename");
      <*ASSERT FALSE*>
    END;
  END FileSystemRename;

PROCEDURE FileSystemIterate (<*UNUSED*>self: RemFileSystemServer; 
                             fileName: TEXT): RemIterator RAISES {ServerError} =
  BEGIN
    TRY
      WITH it = FS.Iterate(fileName) DO
        RETURN NEW(RemIteratorServer, iterator := it);
      END;
    EXCEPT
    | OSError.E => RaiseServerError("Iterate");
      <*ASSERT FALSE*>
    END;
  END FileSystemIterate;

PROCEDURE FileSystemStatus (<*UNUSED*>self: RemFileSystemServer; 
                            fileName: TEXT) : File.Status RAISES {ServerError} =
  BEGIN
    TRY
      RETURN FS.Status(fileName);
    EXCEPT
    | OSError.E => RaiseServerError("Status");
      <*ASSERT FALSE*>
    END;
  END FileSystemStatus; 

PROCEDURE FileSystemSetModificationTime (self: RemFileSystemServer; 
                                         fileName: TEXT; time: Time.T)
  RAISES {ServerError} =
  BEGIN
    IF self.readOnly THEN RaiseServerError("SetModificationTime") END;
    TRY
      FS.SetModificationTime(fileName,time);
    EXCEPT
    | OSError.E => RaiseServerError("SetModificationTime");
      <*ASSERT FALSE*>
    END;
  END FileSystemSetModificationTime; 

PROCEDURE FileSystemPathSep(<*UNUSED*> self: RemFileSystemServer): TEXT =
  BEGIN
    RETURN M3Config.PATH_SEP;
  END FileSystemPathSep;

PROCEDURE FileSystemPathSearchSep(<*UNUSED*> self: RemFileSystemServer): TEXT =
  BEGIN
    RETURN Text.FromChar(ObPathSep.SearchPathSeparator);
  END FileSystemPathSearchSep;

PROCEDURE FileSystemPathCurrent(<*UNUSED*> self: RemFileSystemServer): TEXT =
  BEGIN
    RETURN Pathname.Current;
  END FileSystemPathCurrent;

PROCEDURE FileSystemPathParent(<*UNUSED*> self: RemFileSystemServer): TEXT =
  BEGIN
    RETURN Pathname.Parent;
  END FileSystemPathParent;

PROCEDURE FileSystemPathValid(<*UNUSED*> self: RemFileSystemServer; pn: Pathname.T): BOOLEAN =
  BEGIN
    RETURN Pathname.Valid(pn);
  END FileSystemPathValid;

PROCEDURE FileSystemPathDecompose(<*UNUSED*> self: RemFileSystemServer;
                                  pn: Pathname.T): Pathname.Arcs 
  RAISES{Pathname.Invalid} =
  BEGIN
    RETURN Pathname.Decompose(pn);
  END FileSystemPathDecompose;

PROCEDURE FileSystemPathCompose(<*UNUSED*> self: RemFileSystemServer;
                                a: Pathname.Arcs): Pathname.T
  RAISES{Pathname.Invalid} = 
  BEGIN
    RETURN Pathname.Compose(a);
  END FileSystemPathCompose;

PROCEDURE FileSystemPathAbsolute(<*UNUSED*> self: RemFileSystemServer; 
                                 pn: Pathname.T): BOOLEAN =
  BEGIN
    RETURN Pathname.Absolute(pn);
  END FileSystemPathAbsolute;

PROCEDURE FileSystemPathPrefix(<*UNUSED*> self: RemFileSystemServer; 
                               pn: Pathname.T): Pathname.T =
  BEGIN
    RETURN Pathname.Prefix(pn);
  END FileSystemPathPrefix;

PROCEDURE FileSystemPathLast(<*UNUSED*> self: RemFileSystemServer; 
                             pn: Pathname.T): Pathname.T =
  BEGIN
    RETURN Pathname.Last(pn);
  END FileSystemPathLast;

PROCEDURE FileSystemPathBase(<*UNUSED*> self: RemFileSystemServer; 
                             pn: Pathname.T): Pathname.T =
  BEGIN
    RETURN Pathname.Base(pn);
  END FileSystemPathBase;

PROCEDURE FileSystemPathJoin(<*UNUSED*> self: RemFileSystemServer; 
                             pn, base: Pathname.T; ext: TEXT): Pathname.T=
  BEGIN
    RETURN Pathname.Join(pn,base,ext);
  END FileSystemPathJoin;

PROCEDURE FileSystemPathLastBase(<*UNUSED*> self: RemFileSystemServer; 
                                 pn: Pathname.T): Pathname.T =
  BEGIN
    RETURN Pathname.LastBase(pn);
  END FileSystemPathLastBase;

PROCEDURE FileSystemPathLastExt(<*UNUSED*> self: RemFileSystemServer; 
                                pn: Pathname.T): TEXT=
  BEGIN
    RETURN Pathname.LastExt(pn);
  END FileSystemPathLastExt;

PROCEDURE FileSystemPathReplaceExt(<*UNUSED*> self: RemFileSystemServer; 
                                   pn: Pathname.T; ext: TEXT): Pathname.T =
  BEGIN
    RETURN Pathname.ReplaceExt(pn,ext);
  END FileSystemPathReplaceExt;

PROCEDURE IteratorNext (self: RemIteratorServer; 
                        VAR name: TEXT) : BOOLEAN =
  BEGIN
    RETURN self.iterator.next(name);
  END IteratorNext; 

PROCEDURE IteratorClose (self: RemIteratorServer) =
  BEGIN
    self.iterator.close();
  END IteratorClose; 

PROCEDURE IteratorNextWithStatus (self: RemIteratorServer; 
                                  VAR name: TEXT; 
                                  VAR stat: File.Status) : BOOLEAN 
  RAISES {ServerError} =
  BEGIN
    TRY
      RETURN self.iterator.nextWithStatus(name,stat);
    EXCEPT
    | OSError.E => RaiseServerError("NextWithStatus");
      <*ASSERT FALSE*>
    END;
  END IteratorNextWithStatus; 

PROCEDURE NewProcessor (): ValProcessor =
  BEGIN
    RETURN NEW(ValProcessor, picklable := TRUE,
               tag:="Processor", what := "<Processor at " & machineAddress & ">",
               remote := NEW(RemProcessorServer));
  END NewProcessor;

PROCEDURE ProcessorIs (self: ValProcessor; other: ValAnything): BOOLEAN =
  BEGIN
    TYPECASE other OF
      ValProcessor (oth) => RETURN self.remote = oth.remote;
    ELSE
      RETURN FALSE
    END;
  END ProcessorIs;

PROCEDURE NewProcess (proc: Process.T): RemProcess =
  BEGIN
    RETURN NEW(RemProcessServer, proc := proc);
  END NewProcess;

PROCEDURE ProcessIs (self: ValProcess; other: ValAnything): BOOLEAN =
  BEGIN
    TYPECASE other OF
      ValProcess (oth) => RETURN self.remote = oth.remote;
    ELSE
      RETURN FALSE
    END;
  END ProcessIs; 

PROCEDURE NewRd(rd: Rd.T; what: TEXT := "<a reader>"): ValRd =
  BEGIN
    RETURN NEW(ValRd, what := what, tag := "Reader",
               picklable := FALSE, rd := rd);
  END NewRd;

PROCEDURE RdIs(self: ValRd; other: ValAnything): BOOLEAN =
  BEGIN
    TYPECASE other OF
      ValRd (oth) => RETURN self.rd = oth.rd;
    ELSE
      RETURN FALSE
    END;
  END RdIs;

PROCEDURE NewWr(wr: Wr.T; what: TEXT := "<a writer>"): ValWr =
  BEGIN
    RETURN NEW(ValWr, what := what, tag := "Writer",
               picklable := FALSE, wr := wr);
  END NewWr;

PROCEDURE WrIs(self: ValWr; other: ValAnything): BOOLEAN =
  BEGIN
    TYPECASE other OF
      ValWr (oth) => RETURN self.wr = oth.wr;
    ELSE
      RETURN FALSE
    END;
  END WrIs;

PROCEDURE ProcessorCreateProcess(<*UNUSED*> self: RemProcessorServer;
                                 cmd: TEXT; 
                                 READONLY params: ARRAY OF TEXT;
                                 env: REF ARRAY OF TEXT := NIL;
                                 wd: TEXT := NIL; 
                                 mergeOut: BOOLEAN;
                                 VAR (*out*) stdin: Wr.T;
                                 VAR (*out*) stdout: Rd.T;
                                 VAR (*out*) stderr: Rd.T): RemProcess
  RAISES {ServerError} =
  VAR
    stdinR, stdinW, stdoutR, stdoutW, stderrR, stderrW: Pipe.T := NIL;
  BEGIN
    TRY
      Pipe.Open( (*out*)stdinR, (*out*) stdinW);
      Pipe.Open( (*out*)stdoutR, (*out*) stdoutW);
      IF mergeOut THEN
        stderrW := stdoutW;
        stderr := NIL;
      ELSE
        Pipe.Open( (*out*)stderrR, (*out*) stderrW);
      END;
      WITH proc = NEW(RemProcessServer, 
                      proc := Process.Create(cmd, params, env, wd, 
                                             stdinR, stdoutW, stderrW)) DO
        stdinR.close();
        stdoutW.close();
        stdin := NEW(FileWr.T).init(stdinW);
        stdout := NEW(FileRd.T).init(stdoutR);
        IF mergeOut THEN 
          stderr := NIL
        ELSE
          stderrW.close(); 
          stderr := NEW(FileRd.T).init(stderrR);
        END;
        RETURN proc;
      END;
    EXCEPT
    | OSError.E => 
      TRY IF stdinR # NIL THEN stdinR.close() END EXCEPT OSError.E =>END;
      TRY IF stdoutR # NIL THEN stdoutR.close() END EXCEPT OSError.E =>END;
      TRY IF stderrR # NIL THEN stderrR.close() END EXCEPT OSError.E =>END;
      TRY IF stdinW # NIL THEN stdinW.close() END EXCEPT OSError.E =>END;
      TRY IF stdoutW # NIL THEN stdoutW.close() END EXCEPT OSError.E =>END;
      TRY IF stderrW # NIL THEN stderrW.close() END EXCEPT OSError.E =>END;
      RaiseServerError("CreateProcess");
      <*ASSERT FALSE*>
    END;
  END ProcessorCreateProcess;

PROCEDURE ProcessWait(self: RemProcessServer): Process.ExitCode =
  BEGIN
    RETURN Process.Wait(self.proc);
  END ProcessWait;

PROCEDURE ProcessorGetWorkingDirectory(<*UNUSED*> self: RemProcessorServer): TEXT
  RAISES {ServerError} =
  BEGIN
    TRY
      RETURN Process.GetWorkingDirectory();
    EXCEPT
    | OSError.E => RaiseServerError("GetWorkingDirectory");
      <*ASSERT FALSE*>
    END;
  END ProcessorGetWorkingDirectory;

PROCEDURE ProcessorSetWorkingDirectory(<*UNUSED*> self: RemProcessorServer; 
                                       path: TEXT)
  RAISES {ServerError} =
  BEGIN
    TRY
      Process.SetWorkingDirectory(path);
    EXCEPT
    | OSError.E => RaiseServerError("GetWorkingDirectory");
      <*ASSERT FALSE*>
    END;
  END ProcessorSetWorkingDirectory;

PROCEDURE ProcessGetID(self: RemProcessServer): Process.ID =
  BEGIN
    RETURN Process.GetID(self.proc);
  END ProcessGetID;

PROCEDURE RegisterSysCall (name: TEXT; clos: SysCallClosure) =
  VAR v: Refany.T;
  BEGIN
    IF clos = NIL THEN
      EVAL sysCallTable.delete(name, (*out*) v);
    ELSE
      EVAL sysCallTable.put(name, clos);
    END;
  END RegisterSysCall;

PROCEDURE FetchSysCall (name: TEXT; VAR (*out*) clos: SysCallClosure):
  BOOLEAN =
  VAR
    v    : Refany.T;
    found: BOOLEAN;
  BEGIN
    found := sysCallTable.get(name, (*out*) v);
    clos := NARROW(v, SysCallClosure);
    RETURN found;
  END FetchSysCall;

(* === notification for remote object disappearance === *)

TYPE
  ObNotifierClosure = NetObjNotifier.NotifierClosure OBJECT 
    swr  : SynWr.T;
    proc : ValFun;
  OVERRIDES
    notify := ObNotifyMethod;
  END;

PROCEDURE ObNotifyMethod(self: ObNotifierClosure;
                         obj: NetObj.T; st: NetObjNotifier.OwnerState) =
  VAR args   : ARRAY [0..1] OF Val;
  BEGIN
    TYPECASE obj OF
    | RemVar(var) => args[0] := NEW(ValRemVar, remote := var);
    | RemArray(var) => args[0] := NEW(ValRemArray, remote := var);
    | RemObj(var) => args[0] := NEW(ValRemObj, remote := var);
    | RemEngine(var) => args[0] := NEW(ValEngine, remote := var);
    | RemFileSystem(var) => args[0] := NEW(ValFileSystem, remote := var);
    | RemIterator(var) => args[0] := NEW(ValIterator, remote := var);
    ELSE <* ASSERT FALSE *> (* Shouldn't get here! *)
    END;
    CASE st OF
    | NetObjNotifier.OwnerState.Dead =>
      args[1] := NewText("Dead");
    | NetObjNotifier.OwnerState.Failed =>
      args[1] := NewText("Failed");
    END;
    TRY
      EVAL ObEval.Call(self.proc, args, self.swr);
    EXCEPT
    | Error (packet) => ErrorMsg(self.swr, packet);
    | Exception (packet) => ExceptionMsg(self.swr, packet);
    END;
  END ObNotifyMethod;

PROCEDURE ObjNotify(val: Val; notifyProc: ValFun; swr: SynWr.T) =
  BEGIN
    WITH notifier = NEW(ObNotifierClosure, swr := swr, proc := notifyProc) DO
      TYPECASE val OF
      | ValRemVar(var) =>     NetObjNotifier.AddNotifier(var.remote, notifier);
      | ValRemArray(var) =>   NetObjNotifier.AddNotifier(var.remote, notifier);
      | ValRemObj(var) =>     NetObjNotifier.AddNotifier(var.remote, notifier);
      | ValEngine(var) =>     NetObjNotifier.AddNotifier(var.remote, notifier);
      | ValFileSystem(var) => NetObjNotifier.AddNotifier(var.remote, notifier);
      | ValIterator(var) =>   NetObjNotifier.AddNotifier(var.remote, notifier);
      ELSE (* do nothing for other objects *)
      END;
    END;
  END ObjNotify;
  
(* === GC-safe hash table of refanys :-) === *)

TYPE TblArr = ARRAY OF RECORD old, new: REFANY END;

REVEAL
  Tbl = BRANDED "ObValue.Tbl" OBJECT
          a  : REF TblArr;
          top: INTEGER      := 0;
        METHODS
          Get (old: REFANY; VAR (*out*) new: REFANY): BOOLEAN := TblGet;
          Put (old, new: REFANY)                              := TblPut;
        END;

PROCEDURE NewTbl (): Tbl =
  BEGIN
    RETURN NEW(Tbl, a := NEW(REF TblArr, 256), top := 0);
  END NewTbl;

PROCEDURE TblGet (self: Tbl; old: REFANY; VAR (*out*) new: REFANY):
  BOOLEAN =
  BEGIN
    FOR i := self.top - 1 TO 0 BY -1 DO
      IF self.a^[i].old = old THEN new := self.a^[i].new; RETURN TRUE END;
    END;
    RETURN FALSE;
  END TblGet;

PROCEDURE TblPut (self: Tbl; old, new: REFANY) =
  VAR newArr: REF TblArr;
  BEGIN
    self.a^[self.top].old := old;
    self.a^[self.top].new := new;
    INC(self.top);
    IF self.top >= NUMBER(self.a^) THEN
      newArr := NEW(REF TblArr, 2 * NUMBER(self.a^));
      SUBARRAY(newArr^, 0, NUMBER(self.a^)) := self.a^;
      self.a := newArr;
    END;
  END TblPut;

(* === Copy === *)

TYPE CopyStyle = {ValToVal, ValToLocal, LocalToVal};

TYPE OrigObjType = {Remote, Replicated, Simple};
TYPE ValVarLocal = Val BRANDED "ObValue.ValVarLocal" OBJECT 
                  type                 : OrigObjType;
                  val: Val;  
END;

TYPE
  ValArrayLocal = Val BRANDED "ObValue.ValArrayLocal" OBJECT 
                  type                 : OrigObjType;
                  array: REF Vals;  
  END;

TYPE
  ValObjLocal = Val BRANDED "ObValue.ValObjLocal" OBJECT
                  who                  : TEXT;
                  fields               : REF ObjFields;
                  protected, serialized: BOOLEAN;
                  type                 : OrigObjType;
                END;

PROCEDURE CopyVal (val: Val; tbl: Tbl; loc: SynLocation.T): Val
  RAISES {Error, SharedObj.Error, NetObj.Error, Thread.Alerted} =
  BEGIN
    RETURN Copy(val, tbl, loc, CopyStyle.ValToVal);
  END CopyVal;

PROCEDURE CopyValToLocal (val: Val; tbl: Tbl; loc: SynLocation.T): Val
  RAISES {Error, SharedObj.Error, NetObj.Error, Thread.Alerted} =
  BEGIN
    RETURN Copy(val, tbl, loc, CopyStyle.ValToLocal);
  END CopyValToLocal;

PROCEDURE CopyLocalToVal (val: Val; tbl: Tbl; loc: SynLocation.T): Val
  RAISES {Error, SharedObj.Error, NetObj.Error, Thread.Alerted} =
  BEGIN
    RETURN Copy(val, tbl, loc, CopyStyle.LocalToVal);
  END CopyLocalToVal;

PROCEDURE Copy (val: Val; tbl: Tbl; loc: SynLocation.T; style: CopyStyle):
  Val RAISES {Error, SharedObj.Error, NetObj.Error, Thread.Alerted} =
  VAR cache: REFANY;

  PROCEDURE CopyFields (fields, newFields: REF ObjFields)
    RAISES {Error, SharedObj.Error, NetObj.Error, Thread.Alerted} =
    BEGIN
      FOR i := 0 TO NUMBER(fields^) - 1 DO
        newFields^[i].label := fields^[i].label;
        newFields^[i].field := Copy(fields^[i].field, tbl, loc, style);
      END;
    END CopyFields;

  BEGIN
    TYPECASE val OF
    | ValRemVar (node) =>
        VAR
          newVar     : ValRemVar;
          newVarLocal: ValVarLocal;
        BEGIN
          IF tbl.Get(node.remote, (*out*) cache) THEN RETURN cache END;
          CASE style OF
          | CopyStyle.ValToVal =>
              newVar := NEW(ValRemVar, remote := NIL);
              tbl.Put(node.remote, newVar);
              newVar.remote :=
                NEW(RemVarServer,
                    val := Copy(node.remote.Get(), tbl, loc, style));
              RETURN newVar;
          | CopyStyle.ValToLocal =>
              newVarLocal := NEW(ValVarLocal, val := NIL, 
                                 type := OrigObjType.Remote);
              tbl.Put(node.remote, newVarLocal);
              newVarLocal.val := Copy(node.remote.Get(), tbl, loc, style);
              RETURN newVarLocal;
          ELSE                   <*ASSERT FALSE*>
          END;
        END;
    | ValReplVar (node) =>
        VAR
          newVar     : ValReplVar;
          newVarLocal: ValVarLocal;
        BEGIN
          IF tbl.Get(node.replica, (*out*) cache) THEN RETURN cache END;
          CASE style OF
          | CopyStyle.ValToVal =>
              newVar := NEW(ValReplVar, replica := NIL);
              tbl.Put(node.replica, newVar);
              newVar.replica :=
                NEW(ReplVarStd,
                    val := Copy(node.replica.Get(), tbl, loc, style));
              RETURN newVar;
          | CopyStyle.ValToLocal =>
              newVarLocal := NEW(ValVarLocal, val := NIL, 
                                 type := OrigObjType.Replicated);
              tbl.Put(node.replica, newVarLocal);
              newVarLocal.val := Copy(node.replica.Get(), tbl, loc, style);
              RETURN newVarLocal;
          ELSE                   <*ASSERT FALSE*>
          END;
        END;
    | ValSimpleVar (node) =>
        VAR
          newVar     : ValSimpleVar;
          newVarLocal: ValVarLocal;
        BEGIN
          IF tbl.Get(node.simple, (*out*) cache) THEN RETURN cache END;
          CASE style OF
          | CopyStyle.ValToVal =>
              newVar := NEW(ValSimpleVar, simple := NIL);
              tbl.Put(node.simple, newVar);
              newVar.simple :=
                NEW(SimpleVar,
                    val := Copy(node.simple.Get(), tbl, loc, style));
              RETURN newVar;
          | CopyStyle.ValToLocal =>
              newVarLocal := NEW(ValVarLocal, val := NIL, 
                                 type := OrigObjType.Simple);
              tbl.Put(node.simple, newVarLocal);
              newVarLocal.val := Copy(node.simple.Get(), tbl, loc, style);
              RETURN newVarLocal;
          ELSE                   <*ASSERT FALSE*>
          END;
        END;
    | ValVarLocal (node) =>
        BEGIN
          IF tbl.Get(node, (*out*) cache) THEN RETURN cache END;
          CASE style OF
          | CopyStyle.LocalToVal =>
              CASE node.type OF
              | OrigObjType.Remote =>
                WITH newVar = NEW(ValRemVar, remote := NIL) DO
                  tbl.Put(node, newVar);
                  newVar.remote :=
                      NEW(RemVarServer, 
                          val := Copy(node.val, tbl, loc, style));
                  RETURN newVar;
                END;
              | OrigObjType.Replicated =>
                WITH newVar = NEW(ValReplVar, replica := NIL) DO
                  tbl.Put(node, newVar);
                  newVar.replica :=
                      NEW(ReplVarStd, val := Copy(node.val, tbl, loc, style));
                  RETURN newVar;
                END;
              | OrigObjType.Simple =>
                WITH newVar = NEW(ValSimpleVar, simple := NIL) DO
                  tbl.Put(node, newVar);
                  newVar.simple :=
                      NEW(SimpleVar, val := Copy(node.val, tbl, loc, style));
                  RETURN newVar;
                END;
              END;
          ELSE                   <*ASSERT FALSE*>
          END;
        END;
    | ValOk, ValBool, ValChar, ValText, ValInt, ValReal, ValException,
        ValEngine =>
        RETURN val;
    | ValOption (node) =>
        VAR newOpt: ValOption;
        BEGIN
          IF tbl.Get(node, (*out*) cache) THEN RETURN cache END;
          newOpt := NEW(ValOption, tag := node.tag, val := NIL);
          tbl.Put(node, newOpt);
          newOpt.val := Copy(node.val, tbl, loc, style);
          RETURN newOpt;
        END;
    | ValAlias (node) =>
        VAR newAlias: ValAlias;
        BEGIN
          IF tbl.Get(node, (*out*) cache) THEN RETURN cache END;
          newAlias :=
            NEW(ValAlias, label := node.label,
                labelIndexHint := node.labelIndexHint, obj := NIL);
          tbl.Put(node, newAlias);
          newAlias.obj := Copy(node.obj, tbl, loc, style);
          RETURN newAlias;
        END;
    | ValRemArray (node) =>
        VAR
          vals, newVals: REF Vals;
          newArr       : ValRemArray;
          newArrLocal  : ValArrayLocal;
        BEGIN
          IF tbl.Get(node.remote, (*out*) cache) THEN RETURN cache END;
          vals := node.remote.Obtain();
          newVals := NEW(REF Vals, NUMBER(vals^));
          CASE style OF
          | CopyStyle.ValToVal =>
              newArr := NEW(ValRemArray, remote := NIL);
              tbl.Put(node.remote, newArr);
              FOR i := 0 TO NUMBER(vals^) - 1 DO
                newVals^[i] := Copy(vals^[i], tbl, loc, style);
              END;
              newArr.remote := NEW(RemArrayServer, array := newVals);
              RETURN newArr;
          | CopyStyle.ValToLocal =>
              newArrLocal := NEW(ValArrayLocal, array := NIL,
                                 type := OrigObjType.Remote);
              tbl.Put(node.remote, newArrLocal);
              FOR i := 0 TO NUMBER(vals^) - 1 DO
                newVals^[i] := Copy(vals^[i], tbl, loc, style);
              END;
              newArrLocal.array := newVals;
              RETURN newArrLocal;
          ELSE                   <*ASSERT FALSE*>
          END;
        END;
    | ValReplArray (node) =>
        VAR
          vals, newVals: REF Vals;
          newArr       : ValReplArray;
          newArrLocal  : ValArrayLocal;
        BEGIN
          IF tbl.Get(node.replica, (*out*) cache) THEN RETURN cache END;
          vals := node.replica.Obtain();
          newVals := NEW(REF Vals, NUMBER(vals^));
          CASE style OF
          | CopyStyle.ValToVal =>
              newArr := NEW(ValReplArray, replica := NIL);
              tbl.Put(node.replica, newArr);
              FOR i := 0 TO NUMBER(vals^) - 1 DO
                newVals^[i] := Copy(vals^[i], tbl, loc, style);
              END;
              newArr.replica := NEW(ReplArrayStd, array := newVals).init();
              RETURN newArr;
          | CopyStyle.ValToLocal =>
              newArrLocal := NEW(ValArrayLocal, array := NIL,
                                 type := OrigObjType.Replicated);
              tbl.Put(node.replica, newArrLocal);
              FOR i := 0 TO NUMBER(vals^) - 1 DO
                newVals^[i] := Copy(vals^[i], tbl, loc, style);
              END;
              newArrLocal.array := newVals;
              RETURN newArrLocal;
          ELSE                   <*ASSERT FALSE*>
          END;
        END;
    | ValSimpleArray (node) =>
        VAR
          vals, newVals: REF Vals;
          newArr       : ValSimpleArray;
          newArrLocal  : ValArrayLocal;
        BEGIN
          IF tbl.Get(node.simple, (*out*) cache) THEN RETURN cache END;
          vals := node.simple.Obtain();
          newVals := NEW(REF Vals, NUMBER(vals^));
          CASE style OF
          | CopyStyle.ValToVal =>
              newArr := NEW(ValSimpleArray, simple := NIL);
              tbl.Put(node.simple, newArr);
              FOR i := 0 TO NUMBER(vals^) - 1 DO
                newVals^[i] := Copy(vals^[i], tbl, loc, style);
              END;
              newArr.simple := NEW(SimpleArray, array := newVals);
              RETURN newArr;
          | CopyStyle.ValToLocal =>
              newArrLocal := NEW(ValArrayLocal, array := NIL,
                                 type := OrigObjType.Simple);
              tbl.Put(node.simple, newArrLocal);
              FOR i := 0 TO NUMBER(vals^) - 1 DO
                newVals^[i] := Copy(vals^[i], tbl, loc, style);
              END;
              newArrLocal.array := newVals;
              RETURN newArrLocal;
          ELSE                   <*ASSERT FALSE*>
          END;
        END;
    | ValArrayLocal (node) =>
        VAR
          vals, newVals: REF Vals;
        BEGIN
          IF tbl.Get(node, (*out*) cache) THEN RETURN cache END;
          vals := node.array;
          newVals := NEW(REF Vals, NUMBER(vals^));
          CASE style OF
          | CopyStyle.LocalToVal =>
            CASE node.type OF
            | OrigObjType.Remote =>
              WITH newArr = NEW(ValRemArray, remote := NIL) DO
                tbl.Put(node, newArr);
                FOR i := 0 TO NUMBER(vals^) - 1 DO
                  newVals^[i] := Copy(vals^[i], tbl, loc, style);
                END;
                newArr.remote := NEW(RemArrayServer, array := newVals);
                RETURN newArr;
              END;
            | OrigObjType.Replicated =>
              WITH newArr = NEW(ValReplArray, replica := NIL) DO
                tbl.Put(node, newArr);
                FOR i := 0 TO NUMBER(vals^) - 1 DO
                  newVals^[i] := Copy(vals^[i], tbl, loc, style);
                END;
                newArr.replica := NEW(ReplArrayStd, array := newVals).init();
                RETURN newArr;
              END;
            | OrigObjType.Simple =>
              WITH newArr = NEW(ValSimpleArray, simple := NIL) DO
                tbl.Put(node, newArr);
                FOR i := 0 TO NUMBER(vals^) - 1 DO
                  newVals^[i] := Copy(vals^[i], tbl, loc, style);
                END;
                newArr.simple := NEW(SimpleArray, array := newVals);
                RETURN newArr;
              END;
            END;
          ELSE                   <*ASSERT FALSE*>
          END;
        END;
    | ValAnything (node) =>
        CASE style OF
        | CopyStyle.ValToVal => RETURN node.Copy(tbl, loc);
        | CopyStyle.ValToLocal, CopyStyle.LocalToVal =>
            IF node.picklable THEN
              RETURN node
            ELSE
              RaiseError("Cannot pickle: " & node.what, loc);
              <*ASSERT FALSE*>
            END;
        ELSE                     <*ASSERT FALSE*>
        END;
    | ValFun (node) =>
        VAR newProc: ValFun;
        BEGIN
          IF tbl.Get(node, (*out*) cache) THEN RETURN cache END;
          newProc := NEW(ValFun, fun := node.fun,
                         global := NEW(REF Vals, NUMBER(node.global^)));
          tbl.Put(node, newProc);
          FOR i := 0 TO NUMBER(node.global^) - 1 DO
            newProc.global^[i] := Copy(node.global^[i], tbl, loc, style);
          END;
          RETURN newProc;
        END;
    | ValMeth (node) =>
        VAR newMeth: ValMeth;
        BEGIN
          IF tbl.Get(node, (*out*) cache) THEN RETURN cache END;
          newMeth := NEW(ValMeth, meth := node.meth,
                         global := NEW(REF Vals, NUMBER(node.global^)));
          tbl.Put(node, newMeth);
          FOR i := 0 TO NUMBER(node.global^) - 1 DO
            newMeth.global^[i] := Copy(node.global^[i], tbl, loc, style);
          END;
          RETURN newMeth;
        END;
    | ValRemObj (node) =>
        VAR
          fields, newFields    : REF ObjFields;
          who                  : TEXT;
          protected, serialized: BOOLEAN;
          sync                 : Sync;
        BEGIN
          IF tbl.Get(node.remote, (*out*) cache) THEN RETURN cache END;
          TRY
            who := node.remote.Who( (*out*)protected, (*out*) serialized);
            fields := node.remote.Obtain(FALSE);
            newFields := NEW(REF ObjFields, NUMBER(fields^));
          EXCEPT
            ServerError (msg) => RaiseError(msg, loc);
          END;
          IF serialized THEN
            sync := NEW(Sync, mutex := NEW(Thread.Mutex))
          ELSE
            sync := NIL
          END;
          CASE style OF
          | CopyStyle.ValToVal =>
              WITH newObj = NEW(ValRemObj, remote := NIL) DO
                tbl.Put(node.remote, newObj);
                CopyFields(fields, newFields);
                WITH newObjServ = NEW(RemObjServer, who := who,
                                      self := newObj, fields := newFields,
                                      protected := protected, sync := sync) DO
                  newObj.remote := newObjServ;
                END;
                RETURN newObj;
              END;
          | CopyStyle.ValToLocal =>
              WITH newObjLocal = NEW(ValObjLocal, who := who,
                                     fields := NIL, protected := protected,
                                     serialized := serialized,
                                     type := OrigObjType.Remote) DO
                tbl.Put(node.remote, newObjLocal);
                CopyFields(fields, newFields);
                newObjLocal.fields := newFields;
                RETURN newObjLocal;
              END;
          ELSE
            <*ASSERT FALSE*>
          END;
        END;
    | ValReplObj (node) =>
        VAR
          fields, newFields: REF ObjFields;
          protected        : BOOLEAN;
          who              : TEXT;
        BEGIN
          IF tbl.Get(node.replica, (*out*) cache) THEN RETURN cache END;
          TRY
            who := node.replica.Who( (*out*)protected);
            fields := node.replica.Obtain(FALSE);
            newFields := NEW(REF ObjFields, NUMBER(fields^));
          EXCEPT
            ServerError (msg) => RaiseError(msg, loc);
          END;
          CASE style OF
          | CopyStyle.ValToVal =>
              WITH newObj = NEW(ValReplObj, replica := NIL) DO
                tbl.Put(node.replica, newObj);
                CopyFields(fields, newFields);
                WITH newObjServ = NEW(
                                    ReplObjStd, who := who, self := newObj,
                                    protected := protected,
                                    fields := newFields).init() DO
                  newObj.replica := newObjServ;
                END;
                RETURN newObj;
              END;
          | CopyStyle.ValToLocal =>
              WITH newObjLocal = NEW(ValObjLocal, who := who,
                                     fields := NIL, protected := protected,
                                     serialized := FALSE,
                                     type := OrigObjType.Replicated) DO
                tbl.Put(node.replica, newObjLocal);
                CopyFields(fields, newFields);
                newObjLocal.fields := newFields;
                RETURN newObjLocal;
              END;
          ELSE
            <*ASSERT FALSE*>
          END;
        END;
    | ValSimpleObj (node) =>
        VAR
          fields, newFields    : REF ObjFields;
          who                  : TEXT;
          protected, serialized: BOOLEAN;
          sync                 : Sync;
        BEGIN
          IF tbl.Get(node.simple, (*out*) cache) THEN RETURN cache END;
          TRY
            who := node.simple.Who( (*out*)protected, (*out*) serialized);
            fields := node.simple.Obtain(FALSE);
            newFields := NEW(REF ObjFields, NUMBER(fields^));
          EXCEPT
            ServerError (msg) => RaiseError(msg, loc);
          END;
          IF serialized THEN
            sync := NEW(Sync, mutex := NEW(Thread.Mutex))
          ELSE
            sync := NIL
          END;
          CASE style OF
          | CopyStyle.ValToVal =>
              WITH newObj = NEW(ValSimpleObj, simple := NIL) DO
                tbl.Put(node.simple, newObj);
                CopyFields(fields, newFields);
                WITH newObjServ = NEW(SimpleObj, who := who,
                                      self := newObj, fields := newFields,
                                      protected := protected, sync := sync) DO
                  newObj.simple := newObjServ;
                END;
                RETURN newObj;
              END;
          | CopyStyle.ValToLocal =>
              WITH newObjLocal = NEW(ValObjLocal, who := who,
                                     fields := NIL, protected := protected,
                                     serialized := serialized,
                                     type := OrigObjType.Simple) DO
                tbl.Put(node.simple, newObjLocal);
                CopyFields(fields, newFields);
                newObjLocal.fields := newFields;
                RETURN newObjLocal;
              END;
          ELSE
            <*ASSERT FALSE*>
          END;
        END;
    | ValObjLocal (node) =>
        VAR
          fields, newFields: REF ObjFields;
          sync             : Sync;
        BEGIN
          IF tbl.Get(node, (*out*) cache) THEN RETURN cache END;
          fields := node.fields;
          newFields := NEW(REF ObjFields, NUMBER(fields^));
          IF node.serialized THEN
            sync := NEW(Sync, mutex := NEW(Thread.Mutex))
          ELSE
            sync := NIL
          END;
          CASE style OF
          | CopyStyle.LocalToVal =>
              CASE node.type OF
              | OrigObjType.Remote =>
                  WITH newObj = NEW(ValRemObj, remote := NIL) DO
                    tbl.Put(node, newObj);
                    CopyFields(fields, newFields);
                    WITH newObjServ = NEW(RemObjServer, who := node.who,
                                          self := NIL, fields := newFields,
                                          protected := node.protected,
                                          sync := sync) DO
                      newObj.remote := newObjServ;
                      newObjServ.self := newObj;
                    END;
                    RETURN newObj;
                  END;
              | OrigObjType.Replicated =>
                  WITH newObj = NEW(ValReplObj, replica := NIL) DO
                    tbl.Put(node, newObj);
                    CopyFields(fields, newFields);
                    WITH newObjServ = NEW(ReplObjStd, who := node.who,
                                          self := NIL,
                                          protected := node.protected,
                                          fields := newFields).init() DO
                      newObj.replica := newObjServ;
                      newObjServ.self := newObj;
                      RETURN newObj;
                    END;
                  END;
              | OrigObjType.Simple =>
                  WITH newObj = NEW(ValSimpleObj, simple := NIL) DO
                    tbl.Put(node, newObj);
                    CopyFields(fields, newFields);
                    WITH newObjServ = NEW(SimpleObj, who := node.who,
                                          self := NIL, fields := newFields,
                                          protected := node.protected,
                                          sync := sync) DO
                      newObj.simple := newObjServ;
                      newObjServ.self := newObj;
                      RETURN newObj;
                    END;
                  END;
              END;
          ELSE                   <*ASSERT FALSE*>
          END;
        END;
    ELSE                         <*ASSERT FALSE*>
    END;
  END Copy;

PROCEDURE CopyId (           self: ValAnything;
                  <*UNUSED*> tbl : Tbl;
                  <*UNUSED*> loc : SynLocation.T): ValAnything =
  BEGIN
    RETURN self;
  END CopyId;

PROCEDURE CopyError (           self: ValAnything;
                     <*UNUSED*> tbl : Tbl;
                                loc : SynLocation.T): ValAnything
  RAISES {Error} =
  BEGIN
    RaiseError("Cannot copy: " & self.what, loc); <*ASSERT FALSE*>
  END CopyError;

(*--------------------Description routines-----------------------*)

PROCEDURE GetTypeString (val: Val): TEXT =
  BEGIN
    IF val=NIL THEN RETURN "NIL" END;
    TYPECASE val OF
    | ValRemVar => RETURN "Var";
    | ValReplVar => RETURN "Var`Replicated";
    | ValSimpleVar => RETURN "Var`Simple";
    | ValOk => RETURN "Ok";
    | ValBool => RETURN "Bool";
    | ValChar => RETURN "Char";
    | ValText => RETURN "Text";
    | ValInt => RETURN "Int";
    | ValReal => RETURN "Real";
    | ValOption => RETURN "Option";
    | ValAlias => RETURN "Alias";
    | ValRemArray => RETURN "Array`Remote"
    | ValReplArray => RETURN "Array`Replicated"
    | ValSimpleArray => RETURN "Array`Simple"
    | ValAnything(node) => 
      IF node.tag = NIL THEN RETURN "ValAnything" ELSE RETURN node.tag END;
    | ValFun(node) => RETURN "Closure`" & Fmt.Int(node.fun.bindersNo);
    | ValMeth(node) => 
      IF node.meth.update THEN
        RETURN "Method`" & Fmt.Int(node.meth.bindersNo) & "`Update";
      ELSE
        RETURN "Method`" & Fmt.Int(node.meth.bindersNo);
      END;
    | ValRemObj => RETURN "Object`Remote";
    | ValReplObj => RETURN "Object`Replicated";
    | ValSimpleObj => RETURN "Object`Simple";
    | ValEngine => RETURN "Engine";
    | ValException => RETURN "Exception";
    ELSE RETURN "?";
    END;
  END GetTypeString;

(*--------------------Pickling routines-----------------------*)
(* Need a pickle special for the simple objects, and a shared
   object special for the replicated objects *)

PROCEDURE WriteFields (out   : Pickle.Writer;
                       self  : ValObj;
                       fields: REF ObjFields;
                       pkl   : REF ObjFields  )
  RAISES {Pickle.Error, Wr.Failure, Thread.Alerted} =
  VAR
    binderList: ObTree.IdeList;
    newEnv    : Env;
    meth      : ValMeth;
  BEGIN
    IF pkl # NIL THEN
      FOR i := 0 TO NUMBER(fields^) - 1 DO
        PickleStubs.OutText(out, fields[i].label);
        TYPECASE fields[i].field OF
        | ValMeth, ValAlias => 
          PickleStubs.OutRef(out, fields[i].field);
        ELSE
        END;
      END;
      FOR i := 0 TO NUMBER(fields^) - 1 DO
        TYPECASE fields[i].field OF
        | ValMeth, ValAlias => 
        ELSE
          TRY
            meth := pkl[i].field;
            binderList := meth.meth.binders;
            newEnv := NEW(LocalEnv, name := binderList.first, 
                          val := self, rest := NIL);
            binderList := binderList.rest;
            newEnv := NEW(LocalEnv, name := binderList.first, 
                          val := fields[i].field, rest := newEnv);
            PickleStubs.OutRef(
              out, ObEval.Term(Obliq.Console(), meth.meth.body,
                               (*in-out*) newEnv, meth.global, self));
          EXCEPT
          | Thread.Alerted =>
              RAISE Pickle.Error("Obliq NetObj.Error executing pickle "
                                   & "method" & fields[i].label);
          | Error (packet) =>
              RAISE Pickle.Error("Obliq Error executing pickle " & "method"
                                   & fields[i].label & ": " & packet.msg);
          | Exception (packet) =>
              RAISE
                Pickle.Error("Obliq Exception executing pickle " & "method"
                               & fields[i].label & ": " & packet.msg);
          END;
        END;
      END;
    ELSE
      PickleStubs.OutRef(out, fields);
    END;
  END WriteFields;

PROCEDURE ReadFields (in: Pickle.Reader;
                      self  : ValObj;
                      ret: REF ObjFields;
                      pkl: REF ObjFields)
  RAISES {Pickle.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  VAR binderList: ObTree.IdeList;
      newEnv    : Env;
      meth      : ValMeth;
  BEGIN
      IF pkl # NIL THEN
        FOR i := 0 TO NUMBER(ret^) - 1 DO
          ret[i].label := PickleStubs.InText(in);
          IF pkl[i].field = NIL THEN
            ret[i].field := PickleStubs.InRef(in);
          ELSE
            ret[i].field := valOk;
          END;
        END;
        FOR i := 0 TO NUMBER(ret^) - 1 DO
          IF pkl[i].field # NIL THEN
            TRY
              meth := pkl[i].field;
              binderList := meth.meth.binders;
              newEnv := NEW(LocalEnv, name := binderList.first, 
                            val := self, rest := NIL);
              binderList := binderList.rest;
              newEnv := NEW(LocalEnv, name := binderList.first, 
                            val := PickleStubs.InRef(in), rest := newEnv);
              ret[i].field :=
                ObEval.Term(Obliq.Console(), meth.meth.body,
                            (*in-out*) newEnv, meth.global, NIL);
            EXCEPT
            | Thread.Alerted =>
                RAISE Pickle.Error("Obliq NetObj.Error executing pickle "
                                     & "method" & ret[i].label);
            | Error (packet) =>
                RAISE
                  Pickle.Error("Obliq Error executing pickle " & "method"
                                 & ret[i].label & ": " & packet.msg);
            | Exception (packet) =>
                RAISE Pickle.Error(
                        "Obliq Exception executing pickle " & "method"
                          & ret[i].label & ": " & packet.msg);
            END;
          END;
        END;
      ELSE
        ret^ := NARROW(PickleStubs.InRef(in), REF ObjFields)^;
      END;
  END ReadFields;

TYPE
  SimpleObjSpecial = Pickle.Special OBJECT
                       OVERRIDES
                         write := Write_SimpleObj;
                         read := Read_SimpleObj;
                       END;

PROCEDURE Write_SimpleObj (<*UNUSED*>ts: SimpleObjSpecial; 
                           ref: REFANY; out: Pickle.Writer)
  RAISES {Pickle.Error, Wr.Failure, Thread.Alerted} =
  VAR o := NARROW(ref, SimpleObj);
      tc := TYPECODE(ref);
  BEGIN
    IF tc = TYPECODE(SimpleObj) THEN
      (*
      IF NetObjF.IsNetObjWriter(out) OR EventStubLib.IsEventWriter(out) THEN
        PickleStubs.OutText(out, o.who);
      END;
      *)
      IF o.sync # NIL THEN
        PickleStubs.OutBoolean(out, TRUE);
      ELSE
        PickleStubs.OutBoolean(out, FALSE);
      END;
      PickleStubs.OutBoolean(out, o.protected);
      PickleStubs.OutRef(out, o.self);
      
      PickleStubs.OutRef(out, o.pickleIn);
      PickleStubs.OutRef(out, o.pickleOut);
      
      PickleStubs.OutInteger(out, NUMBER(o.fields^));
      WriteFields(out, o.self, o.fields, o.pickleOut);
    ELSE
      RAISE Pickle.Error("Pickle.Error: cannot handle subtypes " & 
            "of ObValue.SimpleObj");
    END;
  END Write_SimpleObj;

PROCEDURE Read_SimpleObj (<*UNUSED*>ts: SimpleObjSpecial;
                          in: Pickle.Reader;
                          id: Pickle.RefID):REFANY
  RAISES {Pickle.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  VAR o := NEW(SimpleObj);
  BEGIN
    in.noteRef(o, id);
    (*
    IF NetObjF.IsNetObjReader(in) OR EventStubLib.IsEventReader(in) THEN
      o.who := PickleStubs.InText(in);
    ELSE
      o.who := "";
    END;
    *)
    o.who := "";
    IF PickleStubs.InBoolean(in) THEN
      o.sync := NEW(Sync, mutex := NEW(Thread.Mutex))
    END;
    o.protected := PickleStubs.InBoolean(in);
    o.self := PickleStubs.InRef(in);

    o.pickleIn := PickleStubs.InRef(in);
    o.pickleOut := PickleStubs.InRef(in);

    (* we don't need to do this for the pickling, but we require that
       o.self is a valid object *)
    o.self.simple := o;
    WITH num = PickleStubs.InInteger(in) DO
      o.fields := NEW(REF ObjFields, num);
      ReadFields(in, o.self, o.fields, o.pickleIn);
    END;
    RETURN o;
  END Read_SimpleObj;

TYPE
  ReplObjStdSpecial = ObValuePickle.ReplObjStdSpecial OBJECT
                       OVERRIDES
                         write := Write_ReplObjStd;
                         read := Read_ReplObjStd;
                       END;

PROCEDURE Write_ReplObjStd (<*UNUSED*>ts: ReplObjStdSpecial; 
                           ref: SharedObj.T; out: Pickle.Writer)
  RAISES {Pickle.Error, Wr.Failure, Thread.Alerted} =
  VAR obj := NARROW(ref, ReplObjStd);
  BEGIN
    (*
    IF NetObjF.IsNetObjWriter(out) OR EventStubLib.IsEventWriter(out) THEN
      PickleStubs.OutRef(out, obj.who);
    END;
    *)
    PickleStubs.OutRef(out, obj.self);
    PickleStubs.OutInteger(out, ORD(obj.protected));
    PickleStubs.OutRef(out, obj.pickleIn);
    PickleStubs.OutRef(out, obj.pickleOut);

    PickleStubs.OutInteger(out, NUMBER(obj.fields^));
    WriteFields(out, obj.self, obj.fields, obj.pickleOut);
  END Write_ReplObjStd;

PROCEDURE Read_ReplObjStd (<*UNUSED*>ts: ReplObjStdSpecial;
                          ref: SharedObj.T;
                          in: Pickle.Reader)
  RAISES {Pickle.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  VAR obj := NARROW(ref, ReplObjStd);
  BEGIN
    (*
    IF NetObjF.IsNetObjReader(in) OR EventStubLib.IsEventReader(in) THEN
      obj.who := PickleStubs.InRef(in, -1);
    ELSE
      obj.who := "";
    END;
    *)
    obj.who := "";
    obj.self := PickleStubs.InRef(in, TYPECODE(ValReplObj));
    obj.protected := VAL(PickleStubs.InInteger(in, 0,1), BOOLEAN);
    obj.pickleIn := PickleStubs.InRef(in, -1);
    obj.pickleOut := PickleStubs.InRef(in, -1);

    (* we don't need to do this for the pickling, but we require that
       o.self is a valid object *)
    obj.self.replica := obj;
    WITH num = PickleStubs.InInteger(in) DO
      obj.fields := NEW(REF ObjFields, num);
      ReadFields(in, obj.self, obj.fields, obj.pickleIn);
    END;
  END Read_ReplObjStd;

TYPE
  InhibitSpecial = Pickle.Special OBJECT
                     reason: TEXT;
                   OVERRIDES
                     write := WriteInhibitTransmission;
                     read  := ReadInhibitTransmission;
                   END;

PROCEDURE WriteInhibitTransmission (           self: InhibitSpecial;
                                    <*UNUSED*> ref : REFANY;
                                    <*UNUSED*> wr  : Pickle.Writer   )
  RAISES {Pickle.Error} =
  BEGIN
    RAISE Pickle.Error(self.reason);
  END WriteInhibitTransmission;

PROCEDURE ReadInhibitTransmission (           self: InhibitSpecial;
                                   <*UNUSED*> rd  : Pickle.Reader;
                                   <*UNUSED*> id  : Pickle.RefID    ):
  REFANY RAISES {Pickle.Error} =
  BEGIN
    RAISE Pickle.Error(self.reason);
  END ReadInhibitTransmission;

PROCEDURE InhibitTransmission (tc: INTEGER; reason: TEXT) =
  BEGIN
    Pickle.RegisterSpecial(NEW(InhibitSpecial, sc := tc, reason := reason));
  END InhibitTransmission;

BEGIN
  Pickle.RegisterSpecial(NEW(SimpleObjSpecial, sc := TYPECODE(SimpleObj)));
  ObValuePickle.RegisterSpecial_ReplObjStd(NEW(ReplObjStdSpecial));
END ObValue.

(* -- This was an attempt to convince the NetObj runtime to do the right
   thing on pickling. Has been replaced by the current obliq pickling code,
   using Copy.

   There should be a way to temporarily register specials for NetObj.T's.
   The array of specials should be a parameter to Pickle.Read/Pickle.Write.

   In Setup:
      Pickle.RegisterSpecial(NEW(ValArraySpecial, sc:=TYPECODE(ValArray)));

  TYPE
    ValArraySpecial =
      Pickle.Special OBJECT
        OVERRIDES
          write := WriteValArray;
          read := ReadValArray;
        END;

  PROCEDURE WriteValArray(self: ValArraySpecial; ref: REFANY;
    wr: Pickle.Writer) RAISES {Pickle.Error, Wr.Failure, Thread.Alerted} =
  BEGIN
    TYPECASE ref OF
    | ValArray(valArray) =>
      TYPECASE valArray.remote OF
      | RemArrayServer(remArrayServer) =>
          wr.write(remArrayServer.array);
      ELSE RAISE Wr.Failure(NIL);
      END;
    ELSE RAISE Wr.Failure(NIL);
    END;
  END WriteValArray;

  PROCEDURE ReadValArray(self: ValArraySpecial;
    rd: Pickle.Reader; id: Pickle.RefID): REFANY
    RAISES {Pickle.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  VAR res: ValArray;
  BEGIN
    res := NEW(ValArray, remote := NEW(RemArrayServer, array := NIL));
    rd.noteRef(res, id);
    NARROW(res.remote, RemArrayServer).array := rd.read();
    RETURN res;
  END ReadValArray;
*)
 
