(* Copyright 1991 Digital Equipment Corporation. *)
(* Distributed only by permission. *)

MODULE ObValue EXPORTS ObValue, ObValueRep;

IMPORT Text, Fmt, SynWr, SynLocation, ObTree, AtomList, Atom, ObEval,
       NetObj, Pickle2 AS Pickle, PickleStubs, Rd, Wr, Thread,
       OSError, TextRefTbl, Refany, FileRd, FileWr, OpSys, SharedObj,
       NetObjNotifier;

IMPORT ObValuePickle; 

(* IMPORT Env AS ProcEnv; *)

REVEAL
    ValRemObj = ValRemObjPublic BRANDED "ValRemObj" OBJECT OVERRIDES
      Who      := ValRemObjWho;
      Select   := ValRemObjSelect;
      Invoke   := ValRemObjInvoke;
      Update   := ValRemObjUpdate;
      Redirect := ValRemObjRedirect;
      Has      := ValRemObjHas;
      Obtain   := ValRemObjObtain;
    END;

    ValReplObj = ValReplObjPublic BRANDED "ValReplObj" OBJECT OVERRIDES
      Who      := ValReplObjWho;
      Select   := ValReplObjSelect;
      Invoke   := ValReplObjInvoke;
      Update   := ValReplObjUpdate;
      Redirect := ValReplObjRedirect;
      Has      := ValReplObjHas;
      Obtain   := ValReplObjObtain;
    END;

    ValSimpleObj = ValSimpleObjPublic BRANDED "ValSimpleObj" OBJECT
      OVERRIDES
        Who      := ValSimpleObjWho;
        Select   := ValSimpleObjSelect;
        Invoke   := ValSimpleObjInvoke;
        Update   := ValSimpleObjUpdate;
        Redirect := ValSimpleObjRedirect;
        Has      := ValSimpleObjHas;
        Obtain   := ValSimpleObjObtain;
      END;

  RemVarServer = RemVar BRANDED "RemVarServer" OBJECT
                   val: Val;
                 OVERRIDES
                   Get := VarGet;
                   Set := VarSet;
                 END;

  RemArrayServer = RemArray BRANDED "RemArrayServer" OBJECT
                     array: REF Vals;
                   OVERRIDES
                     Size   := ArraySize;
                     Get    := ArrayGet;
                     Set    := ArraySet;
                     Sub    := ArraySub;
                     Upd    := ArrayUpd;
                     Obtain := ArrayObtain;
                   END;

  RemObjServer = RemObjServerPublic BRANDED "RemObjServer" OBJECT
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
                 END;

  SimpleObj = SimpleObjPublic BRANDED "SimpleObj" OBJECT
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
              END;

  RemFileSystemServer = RemFileSystem BRANDED "RemFileSystemServer" OBJECT
                          readOnly: BOOLEAN;
                        OVERRIDES
                          OpenRead   := FileSystemOpenRead;
                          OpenWrite  := FileSystemOpenWrite;
                          OpenAppend := FileSystemOpenAppend;
                        END;

  NonRemObjHookServer = NonRemObjHook BRANDED "NonRemObjHookServer" OBJECT
    replObj: ValObj;
  OVERRIDES
    init := NonRemObjHookInit;
    Get := NonRemObjHookGet;
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
    sharedException := NEW(ValException, name := "shared_failure");
    sharedFatal := NEW(ValException, name := "shared_fatal");
    threadAlerted := NEW(ValException, name := "thread_alerted");
    machineAddress := ThisMachine();

    sysCallTable := NEW(TextRefTbl.Default).init();
    sysCallFailure := NEW(ValException, name := "sys_callFailure");
    showNetObjMsgs := FALSE;

    localProcessor := NewProcessor();
    InhibitTransmission(TYPECODE(ValProcessor),
                        "processors cannot be transmitted/duplicated");
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

PROCEDURE RaiseNetException (msg  : TEXT;
                             atoms: AtomList.T;
                             loc  : SynLocation.T) RAISES {Exception} =
  BEGIN
    IF showNetObjMsgs THEN
      msg := msg & " (NetObj says:";
      WHILE atoms # NIL DO
        msg := msg & " " & Atom.ToText(atoms.head);
        atoms := atoms.tail;
      END;
      msg := msg & ")";
    END;
    RaiseException(netException, msg, loc);
  END RaiseNetException;

PROCEDURE RaiseSharedException (msg  : TEXT;
                                atoms: AtomList.T;
                                loc  : SynLocation.T) RAISES {Exception} =
  BEGIN
    IF showNetObjMsgs THEN
      msg := msg & " (SharedObj says:";
      WHILE atoms # NIL DO
        msg := msg & " " & Atom.ToText(atoms.head);
        atoms := atoms.tail;
      END;
      msg := msg & ")";
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

PROCEDURE Is (v1, v2: Val; <*UNUSED*> location: SynLocation.T): BOOLEAN =
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
    | ValArray (node1) =>
        TYPECASE v2 OF
        | ValArray (node2) => RETURN node1.remote = node2.remote;
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
        | ValOption (node2) => RETURN node1 = node2;
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
      (* Obliq++: added handling for the 3 subtypes, and removed the
         generic ValObj supertype *)
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

PROCEDURE NewText (text: TEXT): Val =
  BEGIN
    IF text = NIL THEN text := "" END;
    RETURN NEW(ValText, text := text);
  END NewText;

PROCEDURE NewVar (val: Val): ValVar =
  BEGIN
    RETURN NEW(ValVar, remote := NEW(RemVarServer, val := val));
  END NewVar;

PROCEDURE VarGet (self: RemVarServer): Val RAISES {} =
  BEGIN
    RETURN self.val;
  END VarGet;

PROCEDURE VarSet (self: RemVarServer; val: Val) RAISES {} =
  BEGIN
    self.val := val;
  END VarSet;

PROCEDURE NewArray (READONLY vals: Vals): ValArray =
  VAR newVals: REF Vals;
  BEGIN
    newVals := NEW(REF Vals, NUMBER(vals));
    newVals^ := vals;
    RETURN NewArrayFromVals(newVals);
  END NewArray;

PROCEDURE NewArrayFromVals (vals: REF Vals): ValArray =
  BEGIN
    RETURN NEW(ValArray, remote := NEW(RemArrayServer, array := vals));
  END NewArrayFromVals;

PROCEDURE ArraySize (arr: RemArrayServer): INTEGER RAISES {} =
  BEGIN
    RETURN NUMBER(arr.array^);
  END ArraySize;

PROCEDURE ArrayGet (self: RemArrayServer; i: INTEGER): Val
  RAISES {ServerError} =
  BEGIN
    IF (i < 0) OR (i >= NUMBER(self.array^)) THEN
      RaiseServerError("arg not in range")
    END;
    RETURN self.array^[i];
  END ArrayGet;

PROCEDURE ArraySet (self: RemArrayServer; i: INTEGER; val: Val)
  RAISES {ServerError} =
  BEGIN
    IF (i < 0) OR (i >= NUMBER(self.array^)) THEN
      RaiseServerError("arg 1 not in range");
    END;
    self.array^[i] := val;
  END ArraySet;

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
    RETURN NEW(ValArray, remote := NEW(RemArrayServer, array := vals));
  END ArraySub;

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

PROCEDURE ArrayObtain (self: RemArrayServer): REF Vals RAISES {} =
  BEGIN
    RETURN self.array;
  END ArrayObtain;

PROCEDURE ArrayCat (vals1, vals2: REF Vals): Val RAISES {} =
  VAR
    len1, len2: INTEGER;
    vals      : REF Vals;
  BEGIN
    len1 := NUMBER(vals1^);
    len2 := NUMBER(vals2^);
    vals := NEW(REF Vals, len1 + len2);
    FOR i := 0 TO len1 - 1 DO vals^[i] := vals1^[i]; END;
    FOR i := 0 TO len2 - 1 DO vals^[len1 + i] := vals2^[i]; END;
    RETURN NEW(ValArray, remote := NEW(RemArrayServer, array := vals));
  END ArrayCat;

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
    (* Obliq++: made the new object a ValRemObj *)
    remObjServ :=
      NEW(RemObjServer, who := who, self := NEW(ValRemObj, remote := NIL),
          fields := fields, protected := protected, sync := sync);
    remObjServ.self.remote := remObjServ;
    RETURN remObjServ.self;
  END NewObjectFromFields;

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
    replObjServ := NEW(ReplObjStd, who := who, 
                      self := NEW(ValReplObj, replica := NIL),
                      protected := protected,
                      fields := fields).init();
    replObjServ.self.replica := replObjServ;
    RETURN replObjServ.self;
  END NewReplObjectFromFields;

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
    WITH res = NEW(SimpleObj, who := resWho,
                   self := NEW(ValSimpleObj, simple := NIL), 
                   fields := resFields,
                   protected := protected, sync := sync) DO
      res.self.simple := res;
      RETURN res.self;
    END;
  END ToSimpleObj;

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

    FOR i := FIRST(resFields^) TO LAST(resFields^) DO
      resFields^[i].update := FALSE;
    END;
    FOR i := FIRST(updateMethods) TO LAST(updateMethods) DO
      j := FIRST(resFields^);
      WHILE j <= LAST(resFields^) DO
        IF Text.Equal(updateMethods[i], resFields^[j].label) THEN
          TYPECASE resFields^[j].field OF
          | ValMeth => resFields^[j].update := TRUE; EXIT;
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
    WITH res = NEW(ReplObjStd, who := resWho, protected := protected,
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
 ValObj object wrapper functions
 ***************************)
PROCEDURE ValRemObjWho(self: ValRemObj; 
                    VAR(*out*) protected, serialized: BOOLEAN): TEXT 
          RAISES {NetObj.Error, Thread.Alerted} =
  BEGIN
    RETURN self.remote.Who(protected, serialized);
  END ValRemObjWho;

PROCEDURE ValRemObjSelect(self: ValRemObj; 
                    label: TEXT; internal: BOOLEAN; VAR hint: INTEGER): Val 
          RAISES {Error, Exception, ServerError, SharedObj.Error, 
                  NetObj.Error, Thread.Alerted} =
  BEGIN
    RETURN self.remote.Select(label, internal, hint);
  END ValRemObjSelect;

PROCEDURE ValRemObjInvoke(self: ValRemObj; 
                    label: TEXT; argNo: INTEGER; READONLY args: Vals;
               internal: BOOLEAN; VAR hint: INTEGER): Val
          RAISES {Error, Exception, ServerError, SharedObj.Error,
                  NetObj.Error, Thread.Alerted} =
  BEGIN
    RETURN self.remote.Invoke(label, argNo, args, internal, hint);
  END ValRemObjInvoke;

PROCEDURE ValRemObjUpdate(self: ValRemObj; 
                    label: TEXT; val: Val; internal: BOOLEAN; 
                       VAR hint: INTEGER) 
          RAISES {ServerError, SharedObj.Error,
                  NetObj.Error, Thread.Alerted} =
  BEGIN
    self.remote.Update(label, val, internal, hint);
  END ValRemObjUpdate;

PROCEDURE ValRemObjRedirect(self: ValRemObj; 
                    val: Val; internal: BOOLEAN) 
          RAISES {ServerError, SharedObj.Error,
                  NetObj.Error, Thread.Alerted} =
  BEGIN
    self.remote.Redirect(val, internal);
  END ValRemObjRedirect;

PROCEDURE ValRemObjHas(self: ValRemObj; 
                    label: TEXT; VAR hint: INTEGER): BOOLEAN 
          RAISES {NetObj.Error, Thread.Alerted} =
  BEGIN
    RETURN self.remote.Has(label, hint);
  END ValRemObjHas;

PROCEDURE ValRemObjObtain(self: ValRemObj; 
                    internal: BOOLEAN): REF ObjFields
  RAISES {ServerError, NetObj.Error, Thread.Alerted} =
  BEGIN
    RETURN self.remote.Obtain(internal);
  END ValRemObjObtain;

PROCEDURE ValReplObjWho(self: ValReplObj; 
                    VAR(*out*) protected, serialized: BOOLEAN): TEXT 
          RAISES {SharedObj.Error} =
  BEGIN
    serialized := TRUE;
    RETURN self.replica.Who(protected);
  END ValReplObjWho;

PROCEDURE ValReplObjSelect(self: ValReplObj; 
                    label: TEXT; <*UNUSED*>internal: BOOLEAN; 
                    VAR hint: INTEGER): Val 
          RAISES {Error, Exception, ServerError, SharedObj.Error} =
  BEGIN
    RETURN self.replica.Select(label, hint);
  END ValReplObjSelect;

PROCEDURE ValReplObjInvoke(self: ValReplObj; 
                    label: TEXT; argNo: INTEGER; READONLY args: Vals;
                    <*UNUSED*>internal: BOOLEAN; VAR hint: INTEGER): Val
          RAISES {Error, Exception, ServerError, SharedObj.Error} =
  BEGIN
    RETURN self.replica.Invoke(label, argNo, args, hint);
  END ValReplObjInvoke;

PROCEDURE ValReplObjUpdate(self: ValReplObj; 
                    label: TEXT; val: Val; internal: BOOLEAN; 
                       VAR hint: INTEGER) 
          RAISES {ServerError, SharedObj.Error} =
  BEGIN
    self.replica.Update(label, val, internal, hint);
  END ValReplObjUpdate;

PROCEDURE ValReplObjRedirect(<*UNUSED*>self: ValReplObj; 
                          <*UNUSED*>val: Val; <*UNUSED*>internal: BOOLEAN) 
          RAISES {ServerError} =
  BEGIN
    RaiseServerError("Cannot Redirect Replicated Object Fields");
  END ValReplObjRedirect;

PROCEDURE ValReplObjHas(self: ValReplObj; 
                    label: TEXT; VAR hint: INTEGER): BOOLEAN 
          RAISES {SharedObj.Error} =
  BEGIN
    RETURN self.replica.Has(label, hint);
  END ValReplObjHas;

PROCEDURE ValReplObjObtain(self: ValReplObj; 
                    internal: BOOLEAN): REF ObjFields
  RAISES {ServerError, SharedObj.Error} =
  BEGIN
    RETURN self.replica.Obtain(internal);
  END ValReplObjObtain;

PROCEDURE ValSimpleObjWho(self: ValSimpleObj; 
                    VAR(*out*) protected, serialized: BOOLEAN): TEXT =
  BEGIN
    RETURN self.simple.Who(protected, serialized);
  END ValSimpleObjWho;

PROCEDURE ValSimpleObjSelect(self: ValSimpleObj; 
                    label: TEXT; internal: BOOLEAN; VAR hint: INTEGER): Val 
          RAISES {Error, Exception, ServerError, SharedObj.Error, 
                  NetObj.Error, Thread.Alerted} =
  BEGIN
    RETURN self.simple.Select(label, internal, hint);
  END ValSimpleObjSelect;

PROCEDURE ValSimpleObjInvoke(self: ValSimpleObj; 
                    label: TEXT; argNo: INTEGER; READONLY args: Vals;
               internal: BOOLEAN; VAR hint: INTEGER): Val
          RAISES {Error, Exception, ServerError, SharedObj.Error,
                  NetObj.Error, Thread.Alerted} =
  BEGIN
    RETURN self.simple.Invoke(label, argNo, args, internal, hint);
  END ValSimpleObjInvoke;

PROCEDURE ValSimpleObjUpdate(self: ValSimpleObj; 
                    label: TEXT; val: Val; internal: BOOLEAN; 
                       VAR hint: INTEGER) 
          RAISES {ServerError, SharedObj.Error,
                  NetObj.Error, Thread.Alerted} =
  BEGIN
    self.simple.Update(label, val, internal, hint);
  END ValSimpleObjUpdate;

PROCEDURE ValSimpleObjRedirect(self: ValSimpleObj; 
                    val: Val; internal: BOOLEAN) 
          RAISES {ServerError, SharedObj.Error,
                  NetObj.Error, Thread.Alerted} =
  BEGIN
    self.simple.Redirect(val, internal);
  END ValSimpleObjRedirect;

PROCEDURE ValSimpleObjHas(self: ValSimpleObj; 
                    label: TEXT; VAR hint: INTEGER): BOOLEAN =
  BEGIN
    RETURN self.simple.Has(label, hint);
  END ValSimpleObjHas;

PROCEDURE ValSimpleObjObtain(self: ValSimpleObj; 
                    internal: BOOLEAN): REF ObjFields
  RAISES {ServerError} =
  BEGIN
    RETURN self.simple.Obtain(internal);
  END ValSimpleObjObtain;

(***************************
 object fields
 ***************************)
PROCEDURE ObjWho (            self                 : RemObjServer;
                  VAR (*out*) protected, serialized: BOOLEAN       ): TEXT =
  BEGIN
    protected := self.protected;
    serialized := self.sync # NIL;
    RETURN self.who;
  END ObjWho;

PROCEDURE ReplObjWho (self: ReplObj;
                      VAR (*out*) protected: BOOLEAN ): TEXT =
  BEGIN
    protected := self.protected;
    RETURN self.who;
  END ReplObjWho;

PROCEDURE SimpleObjWho (            self                 : SimpleObj;
                        VAR (*out*) protected, serialized: BOOLEAN    ): TEXT =
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
  VAR
    resWho, who: TEXT;
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
    resWho := "clone of " & who;
    fieldsOf1 := valObj.Obtain(ObjEqual(valObj, mySelf));
    resSize := NUMBER(fieldsOf1^);
    resFields := NEW(REF ObjFields, resSize);
    resFields^ := fieldsOf1^;
    IF serialized THEN
      sync := NEW(Sync, mutex := NEW(Thread.Mutex))
    ELSE
      sync := NIL
    END;
    (* Obliq++: made the new object a ValRemObj *)
    TYPECASE valObj OF
    | ValRemObj =>
      WITH res = NEW(RemObjServer, who := resWho,
                     self := NEW(ValRemObj, remote := NIL), 
                     fields := resFields,
                     protected := protected, sync := sync) DO
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
                     fields := resFields,
                     protected := protected, sync := sync) DO
        res.self.simple := res;
        RETURN res.self;
      END;
    ELSE <*ASSERT FALSE*>
    END;
  END ObjClone1;

PROCEDURE ObjClone (READONLY valObjs: ARRAY OF ValObj; mySelf: ValObj):
  ValObj RAISES {ServerError, NetObj.Error, Thread.Alerted, SharedObj.Error} =
  VAR
    resWho, remWho: TEXT;
  VAR fieldsOfN: REF ARRAY OF REF ObjFields;
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
        TYPECASE valObjs[i] OF ValRemObj => (* ok *)
        ELSE 
          RaiseServerError("Objects to be cloned must be of the same type");
        END;
      END;
    | ValReplObj =>
      FOR i := 1 TO NUMBER(valObjs) - 1 DO
        TYPECASE valObjs[i] OF ValReplObj => (* ok *)
        ELSE 
          RaiseServerError("Objects to be cloned must be of the same type");
        END;
      END;
    | ValSimpleObj =>
      FOR i := 1 TO NUMBER(valObjs) - 1 DO
        TYPECASE valObjs[i] OF ValSimpleObj => (* ok *)
        ELSE 
          RaiseServerError("Objects to be cloned must be of the same type");
        END;
      END;
    ELSE
      RaiseServerError("Arguments of clone must be objects");
    END;      

    resWho := "clone of";
    protected := FALSE;
    serialized := FALSE;
    fieldsOfN := NEW(REF ARRAY OF REF ObjFields, NUMBER(valObjs));
    FOR i := 0 TO NUMBER(valObjs) - 1 DO
      remWho := valObjs[i].Who( (*out*)protected1, (*out*) serialized1);
      IF i = 0 THEN
        protected := protected1;
        serialized := serialized1;
      END;
      IF Text.Empty(remWho) THEN remWho := "someone" END;
      resWho := resWho & " " & remWho;
      fieldsOfN^[i] := valObjs[i].Obtain(ObjEqual(valObjs[i], mySelf));
    END;
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
      FOR i := 0 TO resSize - 1 DO
        FOR j := i + 1 TO resSize - 1 DO
          IF Text.Equal(resFields^[i].label, resFields^[j].label) THEN
            RaiseServerError(
              "duplicated field on cloning: " & resFields^[i].label);
          END;
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
                     fields := resFields,
                     protected := protected, sync := sync) DO
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
                     fields := resFields,
                     protected := protected, sync := sync) DO
        res.self.simple := res;
        RETURN res.self;
      END;
    ELSE <*ASSERT FALSE*>
    END;
  END ObjClone;

PROCEDURE SetObjPickler(obj: ValObj; picklerIn: ValSimpleObj;
                        picklerOut: ValSimpleObj; mySelf: ValObj)
  RAISES {ServerError, NetObj.Error, SharedObj.Error,
          Thread.Alerted} =
  VAR
    objFields := obj.Obtain(ObjEqual(obj, mySelf));
    pklInFields := picklerIn.Obtain(ObjEqual(obj, mySelf));
    pklOutFields := picklerOut.Obtain(ObjEqual(obj, mySelf));

    inFields := NEW(REF ObjFields, NUMBER(objFields^));
    outFields := NEW(REF ObjFields, NUMBER(objFields^));
    hint: INTEGER;
    numFields := 0;
  BEGIN
    TYPECASE obj OF
    | ValSimpleObj, ValReplObj => (*ok*)
    ELSE
      RaiseServerError("Can only set picklers for simple or " & 
        "replicated objects");
    END;

    IF NUMBER(pklInFields^) # NUMBER(pklOutFields^) THEN
      RaiseServerError("in and out pickler objects must have the same " &
        "set of fields");
    END;
    IF NUMBER(objFields^) < NUMBER(pklInFields^) THEN
      RaiseServerError("pickler objects have extra fields");
    END;

    (* want to have the inFields and outFields be in the same order as
       the objects fields, for later efficient use *)
    FOR i := 0 TO NUMBER(objFields^) - 1 DO
      TYPECASE objFields[i].field OF
      | ValMeth => (* ignore *)
        IF FieldsHave (pklInFields, objFields[i].label, hint) OR
          FieldsHave (pklOutFields, objFields[i].label, hint) THEN
          RaiseServerError("field in pickle object corresponds to " &
            "method field in object: " & objFields[i].label);
        END;
        (* put some dummy values for simplicity *)
        inFields[i].label := objFields[i].label;
        inFields[i].field := valOk;
        outFields[i].label := objFields[i].label;
        outFields[i].field := valOk;
      | ValAlias => RaiseServerError("Unexpected Alias field in " &
        "replicated object: " & objFields[i].label);
      ELSE
        IF NOT FieldsHave(pklInFields, objFields[i].label, hint) THEN
          RaiseServerError("pickler 'in' object missing field: " & 
            objFields[i].label);
        END;
        inFields[i].label := objFields[i].label;
        TYPECASE pklInFields[hint].field OF
        | ValMeth(meth) =>
          IF meth.meth.bindersNo # 3 THEN
            RaiseServerError(BadArgsNoMsg(3, meth.meth.bindersNo, 
                                          "pickle 'in' method",
                                          objFields[i].label));
          END;
          inFields[i].field := meth;
        ELSE
          RaiseServerError("pickler 'in' field must be a method: " & 
            objFields[i].label);
        END;
        IF NOT FieldsHave(pklOutFields, objFields[i].label, hint) THEN
          RaiseServerError("pickler 'out' object missing field: " & 
            objFields[i].label);
        END;
        outFields[i].label := objFields[i].label;
        TYPECASE pklOutFields[hint].field OF
        | ValMeth(meth) =>
          IF meth.meth.bindersNo # 3 THEN
            RaiseServerError(BadArgsNoMsg(3, meth.meth.bindersNo,
                                          "pickler 'out' method",
                                          objFields[i].label));
          END;
          outFields[i].field := meth;
        ELSE
          RaiseServerError("pickler 'out' field must be a method: " & 
            objFields[i].label);
        END;
        INC(numFields);
      END;
    END;

    IF numFields # NUMBER(pklOutFields^) THEN
      RaiseServerError("extra fields in pickler objects"); 
    END;

    TYPECASE obj OF
    | ValSimpleObj(simple) => 
      simple.simple.pickleIn := inFields;
      simple.simple.pickleOut := outFields;
    | ValReplObj(repl) =>
      repl.replica.pickleIn := inFields;
      repl.replica.pickleOut := outFields;
    ELSE
      <*ASSERT FALSE*>
    END;
  END SetObjPickler;

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

PROCEDURE NonRemObjHookGet (self: NonRemObjHookServer): ValObj =
  BEGIN
    RETURN self.replObj;
  END NonRemObjHookGet;

PROCEDURE NonRemObjHookInit (self: NonRemObjHookServer; 
                             replObj: ValObj): NonRemObjHook =
  BEGIN
    self.replObj := replObj;
    RETURN self;
  END NonRemObjHookInit; 

<*INLINE*> PROCEDURE FindField (    label : TEXT;
                                    fields: REF ObjFields;
                                VAR hint  : INTEGER        ): Val
  RAISES {ServerError} =
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
          RETURN ObEval.Term(meth.meth.body, (*in-out*) newEnv, 
                             meth.global, self.self);
      | ValAlias (alias) =>
        RETURN alias.obj.Select(alias.label, ObjEqual(alias.obj, self.self),
                                (*var*) alias.labelIndexHint);
      ELSE
        RETURN fieldVal;
      END;

    FINALLY
      IF lock THEN Thread.Release(objMu) END;
    END;
  END ObjSelect;

PROCEDURE SimpleObjSelect (    self    : SimpleObj;
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
          RETURN ObEval.Term(meth.meth.body, (*in-out*) newEnv, 
                             meth.global, self.self);
      | ValAlias (alias) =>
        RETURN alias.obj.Select(alias.label, ObjEqual(alias.obj, self.self),
                                (*var*) alias.labelIndexHint);
      ELSE
        RETURN fieldVal;
      END;

    FINALLY
      IF lock THEN Thread.Release(objMu) END;
    END;
  END SimpleObjSelect;

PROCEDURE ReplObjSelect (    self    : ReplObj;
                             label   : TEXT;
                         VAR hint    : INTEGER        ): Val
  RAISES {Error, Exception, ServerError, SharedObj.Error} =
  VAR
    fields                 := self.fields;
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
        IF fields^[hint].update THEN
          VAR args := ARRAY [0..0] OF Val{NIL};
          BEGIN
            RETURN self.InvokeUpdate(label, 0, args, hint);
          END;
        ELSE
          newEnv := NEW(LocalEnv, name := meth.meth.binders.first,
                        val := self.self, rest := NIL);
          RETURN ObEval.Term(meth.meth.body, (*in-out*) newEnv, 
                             meth.global, self.self);
        END;
    | ValAlias =>
      <*ASSERT FALSE*>(* should not be any aliases on replicated object
                         fields *)
    ELSE
      RETURN fieldVal;
    END;
  END ReplObjSelect;

PROCEDURE FieldsHave (fields: REF ObjFields; label: TEXT; VAR hint: INTEGER):
  BOOLEAN =
  BEGIN
    FOR i := 0 TO NUMBER(fields^) - 1 DO
      IF Text.Equal(label, fields^[i].label) THEN
        hint := i;
        RETURN TRUE;
      END;
    END;
    RETURN FALSE;
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
          RETURN ObEval.Term(meth.meth.body, (*in-out*) newEnv, 
                             meth.global, self.self);
      | ValAlias (alias) =>
        RETURN alias.obj.Invoke(alias.label, argsNo, args,
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
          RETURN ObEval.Term(meth.meth.body, (*in-out*) newEnv, 
                             meth.global, self.self);
      | ValAlias (alias) =>
        RETURN alias.obj.Invoke(alias.label, argsNo, args,
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

PROCEDURE ReplObjInvoke (         self    : ReplObj;
                                  label   : TEXT;
                                  argsNo  : INTEGER;
                         READONLY args    : Vals;
                         VAR      hint    : INTEGER        ): Val
  RAISES {Error, Exception, ServerError, SharedObj.Error} =
  VAR
    fields    : REF ObjFields;
    binderList: ObTree.IdeList;
    newEnv    : Env;
    fieldVal  : Val;
  BEGIN
    fields := self.fields;
    fieldVal := FindField(label, fields, hint);

    (* If it's an update method, do perform the update instead *)
    IF fields^[hint].update THEN
      RETURN self.InvokeUpdate(label, argsNo, args, hint);
    END;

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
        RETURN ObEval.Term(meth.meth.body, (*in-out*) newEnv, 
                           meth.global, self.self);
    | ValAlias =>        <*ASSERT FALSE*>(* should never happen *)
    ELSE
      RaiseServerError("Field used as a method: " & label);
      <*ASSERT FALSE*>
    END;
  END ReplObjInvoke;

PROCEDURE ReplObjInvokeUpdate (         self    : ReplObj;
                                        label   : TEXT;
                                        argsNo  : INTEGER;
                               READONLY args    : Vals;
                                    VAR hint    : INTEGER        ): Val
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
        RETURN ObEval.Term(meth.meth.body, (*in-out*) newEnv, 
                           meth.global, self.self);
    | ValAlias =>        <*ASSERT FALSE*>(* should never happen *)
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
    lock                : BOOLEAN;
    fields              : REF ObjFields;
    objMu               : Thread.Mutex;
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
            alias.obj.Update(alias.label, val,
                             ObjEqual(alias.obj, self.self),
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
    lock                : BOOLEAN;
    fields              : REF ObjFields;
    objMu               : Thread.Mutex;
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
            alias.obj.Update(alias.label, val,
                             ObjEqual(alias.obj, self.self),
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
                         VAR hint    : INTEGER        )
  RAISES {ServerError} =
  VAR
    fields              : REF ObjFields;
  BEGIN
    IF self.protected AND (NOT internal) THEN
      RaiseServerError("Cannot update protected object");
    END;

    TYPECASE val OF
    | ValAlias => 
      RaiseServerError("Cannot alias fields in a replicated object");
    ELSE
    END;

    fields := self.fields;
    EVAL FindField(label, fields, hint);

    TYPECASE fields^[hint].field OF
    | ValAlias => <* ASSERT FALSE *> (* should be impossible *)
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
    TYPECASE val OF ValObj(vo) => valObj := vo ELSE
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
          RaiseServerError("Field not found in object on redirection: " & 
            label);
        END;
        self.fields := newFields; (* atomic swap *)
      END;

    FINALLY
      IF lock THEN Thread.Release(objMu) END;
    END;
  END ObjRedirect;

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
    TYPECASE val OF ValObj(vo) => valObj := vo ELSE
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

PROCEDURE ReplObjObtain (self: ReplObj; internal: BOOLEAN):
  REF ObjFields RAISES {ServerError} =
  BEGIN
    IF self.protected AND (NOT internal) THEN
      RaiseServerError("Cannot obtain protected object");
    END;
    RETURN self.fields;
  END ReplObjObtain;

PROCEDURE SimpleObjObtain (self: SimpleObj; internal: BOOLEAN): REF ObjFields
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
  END SimpleObjObtain;

PROCEDURE ReplObjInit (self: ReplObj): ReplObj =
  BEGIN
    RETURN self;
  END ReplObjInit;

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
        RETURN ObEval.Term(clos.fun.body, (*in-out*) newEnv, 
                           newGlob, mySelf);
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

PROCEDURE NewProcessor (): ValProcessor =
  BEGIN
    RETURN NEW(ValProcessor, picklable := FALSE,
               tag:="Processor",
               what := "<Processor at " & machineAddress & ">");
  END NewProcessor;

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
    proc : ValFun;
  OVERRIDES
    notify := ObNotifyMethod;
  END;

PROCEDURE ObNotifyMethod(self: ObNotifierClosure;
                         obj: NetObj.T; st: NetObjNotifier.OwnerState) =
  VAR args   : ARRAY [0..1] OF Val;
  BEGIN
    TYPECASE obj OF
    | RemVar(var) => args[0] := NEW(ValVar, remote := var);
    | RemArray(var) => args[0] := NEW(ValArray, remote := var);
    | RemObj(var) => args[0] := NEW(ValRemObj, remote := var);
    | RemEngine(var) => args[0] := NEW(ValEngine, remote := var);
    | RemFileSystem(var) => args[0] := NEW(ValFileSystem, remote := var);
    ELSE <* ASSERT FALSE *> (* Shouldn't get here! *)
    END;
    CASE st OF
    | NetObjNotifier.OwnerState.Dead =>
      args[1] := NewText("Dead");
    | NetObjNotifier.OwnerState.Failed =>
      args[1] := NewText("Failed");
    END;
    TRY
      EVAL ObEval.Call(self.proc, args);
    EXCEPT
    | Error (packet) => ErrorMsg(SynWr.err, packet);
    | Exception (packet) => ExceptionMsg(SynWr.err, packet);
    END;
  END ObNotifyMethod;

PROCEDURE ObjNotify(val: Val; notifyProc: ValFun) =
  BEGIN
    WITH notifier = NEW(ObNotifierClosure, proc := notifyProc) DO
      TYPECASE val OF
      | ValVar(var) =>        NetObjNotifier.AddNotifier(var.remote, notifier);
      | ValArray(var) =>      NetObjNotifier.AddNotifier(var.remote, notifier);
      | ValRemObj(var) =>     NetObjNotifier.AddNotifier(var.remote, notifier);
      | ValEngine(var) =>     NetObjNotifier.AddNotifier(var.remote, notifier);
      | ValFileSystem(var) => NetObjNotifier.AddNotifier(var.remote, notifier);
      ELSE (* do nothing for other objects *)
      END;
    END;
  END ObjNotify;
  
(* === GC-safe hash table of refanys :-) === *)

TYPE TblArr = ARRAY OF RECORD old, new: REFANY END;
REVEAL
  Tbl = BRANDED OBJECT
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

TYPE ValVarLocal = Val BRANDED "ValVarLocal" OBJECT val: Val;  END;

TYPE
  ValArrayLocal = Val BRANDED "ValArrayLocal" OBJECT array: REF Vals;  END;

TYPE OrigObjType = {Remote, Replicated, Simple};
TYPE
  ValObjLocal = Val BRANDED "ValObjLocal" OBJECT
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
        newFields^[i].update := FALSE;
      END;
    END CopyFields;

  BEGIN
    TYPECASE val OF
    | ValVar (node) =>
        VAR
          newVar     : ValVar;
          newVarLocal: ValVarLocal;
        BEGIN
          IF tbl.Get(node.remote, (*out*) cache) THEN RETURN cache END;
          CASE style OF
          | CopyStyle.ValToVal =>
              newVar := NEW(ValVar, remote := NIL);
              tbl.Put(node.remote, newVar);
              newVar.remote :=
                NEW(RemVarServer,
                    val := Copy(node.remote.Get(), tbl, loc, style));
              RETURN newVar;
          | CopyStyle.ValToLocal =>
              newVarLocal := NEW(ValVarLocal, val := NIL);
              tbl.Put(node.remote, newVarLocal);
              newVarLocal.val := Copy(node.remote.Get(), tbl, loc, style);
              RETURN newVarLocal;
          ELSE                   <*ASSERT FALSE*>
          END;
        END;
    | ValVarLocal (node) =>
        VAR newVar: ValVar;
        BEGIN
          IF tbl.Get(node, (*out*) cache) THEN RETURN cache END;
          CASE style OF
          | CopyStyle.LocalToVal =>
              newVar := NEW(ValVar, remote := NIL);
              tbl.Put(node, newVar);
              newVar.remote :=
                NEW(RemVarServer, val := Copy(node.val, tbl, loc, style));
              RETURN newVar;
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
    | ValArray (node) =>
        VAR
          vals, newVals: REF Vals;
          newArr       : ValArray;
          newArrLocal  : ValArrayLocal;
        BEGIN
          IF tbl.Get(node.remote, (*out*) cache) THEN RETURN cache END;
          vals := node.remote.Obtain();
          newVals := NEW(REF Vals, NUMBER(vals^));
          CASE style OF
          | CopyStyle.ValToVal =>
              newArr := NEW(ValArray, remote := NIL);
              tbl.Put(node.remote, newArr);
              FOR i := 0 TO NUMBER(vals^) - 1 DO
                newVals^[i] := Copy(vals^[i], tbl, loc, style);
              END;
              newArr.remote := NEW(RemArrayServer, array := newVals);
              RETURN newArr;
          | CopyStyle.ValToLocal =>
              newArrLocal := NEW(ValArrayLocal, array := NIL);
              tbl.Put(node.remote, newArrLocal);
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
          newArr       : ValArray;
        BEGIN
          IF tbl.Get(node, (*out*) cache) THEN RETURN cache END;
          vals := node.array;
          newVals := NEW(REF Vals, NUMBER(vals^));
          CASE style OF
          | CopyStyle.LocalToVal =>
              newArr := NEW(ValArray, remote := NIL);
              tbl.Put(node, newArr);
              FOR i := 0 TO NUMBER(vals^) - 1 DO
                newVals^[i] := Copy(vals^[i], tbl, loc, style);
              END;
              newArr.remote := NEW(RemArrayServer, array := newVals);
              RETURN newArr;
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
            who := node.replica.Who((*out*)protected);
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
                WITH newObjServ = NEW(ReplObjStd, who := who, 
                                      self := newObj,
                                      protected := protected,
                                      fields := newFields).init() DO
                  newObj.replica := newObjServ;
                END;
                RETURN newObj;
              END;
          | CopyStyle.ValToLocal =>
              WITH newObjLocal = NEW(
                                   ValObjLocal, who := who, fields := NIL,
                                   protected := protected,
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
                WITH newObjServ = NEW(SimpleObj, who := who, self := newObj,
                                      fields := newFields,
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
                    WITH newObjServ = NEW(
                                        ReplObjStd, who := node.who,
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

(*--------------------Pickling routines-----------------------*)
(* Need a pickle special for the simple objects, and a shared
   object special for the replicated objects *)

PROCEDURE WriteFields (out: Pickle.Writer; fields: REF ObjFields; 
                       pkl: REF ObjFields) 
  RAISES {Pickle.Error, Wr.Failure, Thread.Alerted} =
  BEGIN
    PickleStubs.OutInteger(out, NUMBER(fields^));
    PickleStubs.OutRef(out, fields);
  END WriteFields;

PROCEDURE ReadFields (in: Pickle.Reader; pkl: REF ObjFields): REF ObjFields
  RAISES {Pickle.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  VAR ret : REF ObjFields;
  BEGIN
    WITH num = PickleStubs.InInteger(in) DO
      ret := PickleStubs.InRef(in);
      <* ASSERT num = NUMBER(ret^) *>
    END;
    RETURN ret;
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
      PickleStubs.OutText(out, o.who);
      IF o.sync # NIL THEN
        PickleStubs.OutBoolean(out, TRUE);
      ELSE
        PickleStubs.OutBoolean(out, FALSE);
      END;
      PickleStubs.OutBoolean(out, o.protected);
      PickleStubs.OutRef(out, o.self);
      
      PickleStubs.OutRef(out, o.pickleIn);
      PickleStubs.OutRef(out, o.pickleOut);
      
      WriteFields(out, o.fields, o.pickleOut);
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
    o.who := PickleStubs.InText(in);
    IF PickleStubs.InBoolean(in) THEN
      o.sync := NEW(Sync, mutex := NEW(Thread.Mutex))
    END;
    o.protected := PickleStubs.InBoolean(in);
    o.self := PickleStubs.InRef(in);

    o.pickleIn := PickleStubs.InRef(in);
    o.pickleOut := PickleStubs.InRef(in);

    o.fields := ReadFields(in, o.pickleIn);
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
    PickleStubs.OutRef(out, obj.who);
    PickleStubs.OutRef(out, obj.self);
    PickleStubs.OutInteger(out, ORD(obj.protected));
    PickleStubs.OutRef(out, obj.pickleIn);
    PickleStubs.OutRef(out, obj.pickleOut);

    WriteFields(out, obj.fields, obj.pickleOut);
  END Write_ReplObjStd;

PROCEDURE Read_ReplObjStd (<*UNUSED*>ts: ReplObjStdSpecial;
                          ref: SharedObj.T;
                          in: Pickle.Reader)
  RAISES {Pickle.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  VAR obj := NARROW(ref, ReplObjStd);
  BEGIN
    obj.who := PickleStubs.InRef(in, -1);
    obj.self := PickleStubs.InRef(in, TYPECODE(ValReplObj));
    obj.protected := VAL(PickleStubs.InInteger(in, 0,1), BOOLEAN);
    obj.pickleIn := PickleStubs.InRef(in, -1);
    obj.pickleOut := PickleStubs.InRef(in, -1);

    obj.fields := ReadFields(in, obj.pickleIn);
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
 
