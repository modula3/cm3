
(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
<*PRAGMA SHARED *>

INTERFACE ObValue;
IMPORT SynWr, SynLocation, Rd, Wr, ObTree, NetObj, Thread, AtomList;
IMPORT SharedObj;

  EXCEPTION 
    Exception(ExceptionPacket); (* trappable     *)
    Error(ErrorPacket);         (* only trappable by try-else *)
    ServerError(TEXT);  (* like Error, but has no location info;
                           immediately converted to Error 
                           on the way back from a server *)
  TYPE
    ErrorPacket =
      BRANDED "ErrorPacket" OBJECT
        msg: TEXT;
        location: SynLocation.T;
      END;
  
    ExceptionPacket =
      BRANDED "ExceptionPacket" OBJECT
        msg: TEXT;
        location: SynLocation.T;
        exception: ValException;
        data: Val;
      END;

    Tbl <: REFANY;

  TYPE
    Vals = ARRAY OF Val;

    Env = BRANDED OBJECT 
	name: ObTree.IdeName;
        rest: Env;
      END;

    LocalEnv =
      Env BRANDED OBJECT
	val: Val;
      END;

    GlobalEnv =
      REF Vals;

    Val = 
      BRANDED "Val" OBJECT
      END;

    ValOk = 
      Val BRANDED "ValOk" OBJECT 
      END;

    ValBool = 
      Val BRANDED "ValBool" OBJECT 
	bool: BOOLEAN;
      END;

    ValChar = 
      Val BRANDED "ValChar" OBJECT 
	char: CHAR;
      END;

    ValText = 
      Val BRANDED "ValText" OBJECT 
	text: TEXT; (* Non-NIL ! *)
      END;

    ValInt = 
      Val BRANDED "ValInt" OBJECT 
	int: INTEGER;
	temp: BOOLEAN:=FALSE;
      END;

    ValReal = 
      Val BRANDED "ValReal" OBJECT
	real: LONGREAL;
	temp: BOOLEAN:=FALSE;
      END;

    ValException = 
      Val BRANDED "ValException" OBJECT
        name: TEXT;
      END;

    ValOption = 
      Val BRANDED "ValOption" OBJECT 
	tag: TEXT;
        val: Val;
      END;

    ValVar =
      Val BRANDED "ValVar" OBJECT
        remote: RemVar;
      END;

    ValArray = 
      Val BRANDED "ValArray" OBJECT 
	remote: RemArray;
      END;

    ValFun = 
      Val BRANDED "ValFun" OBJECT 
        fun: ObTree.TermFun;
	global: GlobalEnv;
      END;

    ValMeth = 
      Val BRANDED "ValMeth" OBJECT 
        meth: ObTree.TermMeth;
	global: GlobalEnv;
      END;

    ValAlias = 
      Val BRANDED "ValAlias" OBJECT 
	label: TEXT;
	labelIndexHint: INTEGER;
        obj: ValObj;
      END;

    ValAnything = (* to be subtyped *)
      Val BRANDED "ValAnything" OBJECT
        what: TEXT; (* Only used for printing. *)
        tag: TEXT := NIL; (* Used to identify the type with reflection *)
        picklable: BOOLEAN := TRUE;
      METHODS
        Is(other: ValAnything): BOOLEAN := IsSelfOther; 
          (* Override with what you want the "is" primitive to do 
             to determine equality of two ValAnything values. 
             The default method returns self=other. *)
        Print(): TEXT := PrintWhat;
          (* Override with a routine to print this value.
             The default method returns the "what"field. *)
        Copy(tbl: Tbl; loc: SynLocation.T): ValAnything 
            RAISES {Error, SharedObj.Error, NetObj.Error, 
                    Thread.Alerted} := CopyError;
          (* Override with a routine that makes a copy of this value
             (the default CopyError raises error, CopyId returns self). 
               Use this pattern:
               IF tbl.Get(self, (*out*)newVal) THEN RETURN newVal END;
               newVal := NEW(ValAnything, what:=self.what,
                              ... all private fields set to NIL ... );
               EVAL tbl.Put(self, newVal);
               ... fill private fields, possibly calling:
                   newVal.field:=ObValue.CopyVal(self.field, tbl, loc) ...
               RETURN newVal;
          *)
      END;

    (* There are 3 kinds of objects, Remote (Network Objects),
       Replicated (Shared Objects) and Simple (normal Modula-3
       Objects).  The Obliq wrappers contain a reference to the real
       data objects in an appropriately typed data field.

       These are ValObj wrapper methods allow each subtype to redirect
       the method calls to their approriate data object (RemObj,
       ReplObj or SimpleObj). *)
    ValObj =
      Val BRANDED "ValObj" OBJECT METHODS
        Who(VAR(*out*) protected, serialized: BOOLEAN): TEXT 
          RAISES {NetObj.Error, SharedObj.Error, Thread.Alerted};
        Select(label: TEXT; internal: BOOLEAN; VAR hint: INTEGER): Val 
          RAISES {Error, Exception, ServerError, SharedObj.Error, 
                  NetObj.Error, Thread.Alerted};
        Invoke(label: TEXT; argNo: INTEGER; READONLY args: Vals;
               internal: BOOLEAN; VAR hint: INTEGER): Val
          RAISES {Error, Exception, ServerError, SharedObj.Error,
                  NetObj.Error, Thread.Alerted};
        Update(label: TEXT; val: Val; internal: BOOLEAN; VAR hint: INTEGER) 
          RAISES {ServerError, SharedObj.Error,
                  NetObj.Error, Thread.Alerted};
        Redirect(val: Val; internal: BOOLEAN) 
          RAISES {ServerError, SharedObj.Error,
                  NetObj.Error, Thread.Alerted};
        Has(label: TEXT; VAR hint: INTEGER): BOOLEAN 
          RAISES {NetObj.Error, Thread.Alerted, SharedObj.Error};
        Obtain(internal: BOOLEAN): REF ObjFields
          RAISES {ServerError, SharedObj.Error, NetObj.Error, Thread.Alerted};
      END;

    ValRemObj <: ValRemObjPublic;
    ValRemObjPublic = ValObj OBJECT
        remote: RemObj;
      END;

    ValReplObj <: ValReplObjPublic;
    ValReplObjPublic = ValObj OBJECT
        replica: ReplObj;
      END;

    ValSimpleObj <: ValSimpleObjPublic;
    ValSimpleObjPublic = ValObj OBJECT
        simple: SimpleObj;
      END;

    ObjFields = ARRAY OF Field;
    Field = RECORD
        label: TEXT;
        field: Val; 
        (* ValMeth for method fields, 
           ValAlias for alias fields, 
           other Val for value fields *)
        update: BOOLEAN := FALSE;
        (* TRUE for replicated object update method, FALSE otherwise *)
      END;

    ValEngine =
      Val BRANDED "ValEngine" OBJECT
        remote: RemEngine;
      END;

    ValFileSystem =
      ValAnything BRANDED "ValFileSystem" OBJECT
        remote: RemFileSystem;
      OVERRIDES
        Is := FileSystemIs;
        Copy := CopyId;
      END;

    ValProcessor =
      ValAnything BRANDED "ValProcessor" OBJECT
      OVERRIDES
        Copy := CopyId;
      END;

    RemVar =
      NetObj.T BRANDED "RemVar" OBJECT
      METHODS
        Get(): Val RAISES {NetObj.Error, Thread.Alerted};
        Set(val: Val) RAISES {NetObj.Error, Thread.Alerted};
      END;

    RemVarServer <:
      RemVar;

   RemArray =
     NetObj.T BRANDED "RemArray" OBJECT
     METHODS
       Size(): INTEGER RAISES {NetObj.Error, Thread.Alerted};
       Get(i: INTEGER): Val RAISES {ServerError, NetObj.Error, Thread.Alerted};
       Set(i: INTEGER; val: Val) 
         RAISES {ServerError, NetObj.Error, Thread.Alerted};
       Sub(start,size: INTEGER): ValArray 
           RAISES {ServerError, NetObj.Error, Thread.Alerted};
         (* Extract the subarray self[start for size]. *)
       Upd(start, size: INTEGER; READONLY other: REF Vals) 
           RAISES {ServerError, NetObj.Error, Thread.Alerted};
         (* Update self[start for size] with other[0 for size]. *)
       Obtain(): REF Vals RAISES {NetObj.Error, Thread.Alerted};
         (* Return self.array if local, or a copy of it if remote.
            Modifying the result of Obtain may violate network transparency. *)
     END;
     
   RemArrayServer <:
     RemArray;

   RemObj =
      NetObj.T BRANDED "RemObj" OBJECT
      METHODS
        Who(VAR(*out*) protected, serialized: BOOLEAN): TEXT 
          RAISES {NetObj.Error, Thread.Alerted};
        Select(label: TEXT; internal: BOOLEAN; VAR hint: INTEGER): Val 
          RAISES {Error, Exception, ServerError, SharedObj.Error, 
                  NetObj.Error, Thread.Alerted};
        Invoke(label: TEXT; argNo: INTEGER; READONLY args: Vals;
          internal: BOOLEAN; VAR hint: INTEGER): Val
          RAISES {Error, Exception, ServerError, SharedObj.Error,
                  NetObj.Error, Thread.Alerted};
        Update(label: TEXT; val: Val; internal: BOOLEAN; VAR hint: INTEGER) 
          RAISES {ServerError, SharedObj.Error,
                  NetObj.Error, Thread.Alerted};
        Redirect(val: Val; internal: BOOLEAN) 
          RAISES {ServerError, SharedObj.Error,
                  NetObj.Error, Thread.Alerted};
        Has(label: TEXT; VAR hint: INTEGER): BOOLEAN 
          RAISES {NetObj.Error, Thread.Alerted};
          (* Whether a field called label exists. *)
        Obtain(internal: BOOLEAN): REF ObjFields
          RAISES {ServerError, NetObj.Error, Thread.Alerted};
          (* Return self.fields if local, or a copy of it if remote.
             Modifying the result of Obtain may violate network transparency. 
             *)
      END;

    RemObjServer <: 
      RemObjServerPublic;
    RemObjServerPublic =
      RemObj BRANDED "RemObjServerPublic" OBJECT
        who: TEXT; 
        sync: Sync;
      END;
    (* RemObjServer, a subtype of RemObj, is the standard implementation of 
       local objects. Another subtype of RemObj is automatically produced by 
       the Network Objects stub generator, for remote surrogates. Further 
       subtypes of RemObj can be defined and used as client-specific 
       pseudo-objects. In the latter case, the result of Who is used for 
       printing; copying, cloning, and pickling operate on the results of 
       Who and Obtain.
       *)

    NonRemObjHookServer <: NonRemObjHook;
    NonRemObjHook =
      NetObj.T BRANDED "NonRemObjHook" OBJECT
      METHODS
        init(remObj: ValObj): NonRemObjHook  RAISES {NetObj.Error,
                                                     Thread.Alerted}; 
        Get(): ValObj RAISES {NetObj.Error, Thread.Alerted};
      END;
    (* NonRemObjHook is used by the obliq net_import and net_export
       commands to allow replicated and simple objects to be exported and
      imported. *)

    ReplObjStd <: ReplObj;
        <* SHARED UPDATE METHODS ReplObjStd.init, ReplObjStd.InvokeUpdate, 
                                 ReplObjStd.Update *>

    ReplObj <: ReplObjPublic;
    ReplObjPublic = SharedObj.T BRANDED "ReplObjServerPublic" OBJECT
      METHODS
        init (): ReplObj RAISES {SharedObj.Error};
        Who(VAR(*out*) protected: BOOLEAN): TEXT 
          RAISES {SharedObj.Error};
        (* All replicated objects are protected and serialized! *)
        Select(label: TEXT; VAR hint: INTEGER): Val 
          RAISES {Error, Exception, ServerError, SharedObj.Error};
        Invoke(label: TEXT; argNo: INTEGER; READONLY args: Vals;
               VAR hint: INTEGER): Val
          RAISES {Error, Exception, ServerError, SharedObj.Error};
        Update(label: TEXT; val: Val; internal: BOOLEAN; 
               VAR hint: INTEGER) RAISES {ServerError, SharedObj.Error};
        Has(label: TEXT; VAR hint: INTEGER): BOOLEAN RAISES {SharedObj.Error};
          (* Whether a field called label exists. *)
        Obtain(internal: BOOLEAN): REF ObjFields
          RAISES {ServerError, SharedObj.Error};
          (* Return self.fields. Modifying the result of Obtain may
             violate network transparency. *)
      END;
    (* ReplObj is the implementation of replicated objects. 
       The default internal implementation is ReplObjStd.  Further 
       subtypes of ReplObj can be defined and used as client-specific 
       pseudo-objects. 
       *)

   SimpleObj <: SimpleObjPublic;
   SimpleObjPublic = BRANDED "SimpleObjPublic" OBJECT
        who: TEXT; 
        sync: Sync;
      METHODS
        Who(VAR(*out*) protected, serialized: BOOLEAN): TEXT;
        Select(label: TEXT; internal: BOOLEAN; VAR hint: INTEGER): Val 
          RAISES {Error, Exception, ServerError, 
                  SharedObj.Error, NetObj.Error, Thread.Alerted};
        Invoke(label: TEXT; argNo: INTEGER; READONLY args: Vals;
          internal: BOOLEAN; VAR hint: INTEGER): Val
          RAISES {Error, Exception, ServerError, 
                  SharedObj.Error, NetObj.Error, Thread.Alerted};
        Update(label: TEXT; val: Val; internal: BOOLEAN; VAR hint: INTEGER) 
          RAISES {ServerError, SharedObj.Error, NetObj.Error, Thread.Alerted};
        Redirect(val: Val; internal: BOOLEAN) 
          RAISES {ServerError, SharedObj.Error, NetObj.Error, Thread.Alerted};
        Has(label: TEXT; VAR hint: INTEGER): BOOLEAN;
          (* Whether a field called label exists. *)
        Obtain(internal: BOOLEAN): REF ObjFields RAISES {ServerError};
          (* Return self.fields if local, or a copy of it if remote.
             Modifying the result of Obtain may violate network transparency. 
             *)
      END;

    Sync =
      BRANDED "Sync" OBJECT
        mutex: Thread.Mutex;
      END;

    RemEngine =
      NetObj.T BRANDED "RemEngine" OBJECT
      METHODS
        Who(): TEXT RAISES {NetObj.Error, Thread.Alerted};
        Eval(proc: Val; mySelf: ValObj): Val 
          RAISES {Error, Exception, ServerError, NetObj.Error, Thread.Alerted};
      END;

    RemEngineServer =
      RemEngine BRANDED "RemEngineServer" OBJECT
        who: TEXT;
        arg: Val;
      OVERRIDES
        Who := EngineWho;
        Eval := EngineEval;
      END;

    RemFileSystem =
      NetObj.T BRANDED "RemFileSystem" OBJECT
      METHODS
        OpenRead(fileName: TEXT): Rd.T 
          RAISES {NetObj.Error, ServerError, Thread.Alerted};
        OpenWrite(fileName: TEXT): Wr.T 
          RAISES {NetObj.Error, ServerError, Thread.Alerted};
        OpenAppend(fileName: TEXT): Wr.T 
          RAISES {NetObj.Error, ServerError, Thread.Alerted};
      END;

    RemFileSystemServer <: RemFileSystem;

    ArgArray = ARRAY [1..8] OF Val;

  VAR 
    valOk: Val;
    netException, threadAlerted, sharedException, sharedFatal: ValException;
    showNetObjMsgs: BOOLEAN;
    machineAddress: TEXT;
    localProcessor: ValProcessor;

  PROCEDURE Setup();
  (* To be called before any other use of this module. *)

  PROCEDURE Is(v1,v2: Val; location: SynLocation.T): BOOLEAN;

  PROCEDURE NewText(text: TEXT): Val;
    (* Create an Obliq text containing a non-NIL m3 TEXT. *)

  PROCEDURE NewVar(val: Val): ValVar;
    (* Create a new variable. *)

  PROCEDURE NewArray(READONLY vals: Vals): ValArray;
    (* Create a new array. *)
  PROCEDURE NewArrayFromVals(vals: REF Vals): ValArray;
    (* Careful: the vals passed in are shared and may get modified later. *)
  PROCEDURE ArrayCat(vals1, vals2: REF Vals): Val RAISES {};

  PROCEDURE NewObject(READONLY fields: ObjFields; 
                      who: TEXT:=""; protected: BOOLEAN:=FALSE; 
                      sync: Sync:=NIL): ValObj;
    (* Create a new object. *)
  PROCEDURE NewObjectFromFields(fields: REF ObjFields; 
                                who: TEXT; protected: BOOLEAN; 
                                sync: Sync): ValObj;
    (* Careful: the fields passed in are shared and may get modified later. *)
  PROCEDURE NewReplObject(READONLY fields: ObjFields; 
                      who: TEXT:=""; protected: BOOLEAN:=FALSE): ValObj
    RAISES {SharedObj.Error};
    (* Create a new replicated object. *)
  PROCEDURE NewReplObjectFromFields(fields: REF ObjFields; 
                                who: TEXT; protected: BOOLEAN): ValObj
    RAISES {SharedObj.Error};
    (* Careful: the fields passed in are shared and may get modified later. *)
  PROCEDURE NewSimpleObject(READONLY fields: ObjFields; 
                      who: TEXT:=""; protected: BOOLEAN:=FALSE; 
                      sync: Sync:=NIL): ValObj;
    (* Create a new simple object. *)
  PROCEDURE NewSimpleObjectFromFields(fields: REF ObjFields; 
                                who: TEXT; protected: BOOLEAN; 
                                sync: Sync): ValObj;
    (* Careful: the fields passed in are shared and may get modified later. *)

  PROCEDURE ToSimpleObj(READONLY obj: ValObj; mySelf: ValObj): ValObj
    RAISES {ServerError, NetObj.Error, SharedObj.Error, Thread.Alerted};
    (* Create a SimpleObj from another ValObj. *)
  PROCEDURE ToReplObj(READONLY obj: ValObj; mySelf: ValObj; 
                      READONLY updateMethods: ARRAY OF TEXT): ValObj
    RAISES {ServerError, NetObj.Error, SharedObj.Error, Thread.Alerted};
    (* Create a ReplObj from another ValObj. *)
  PROCEDURE ToRemObj(READONLY obj: ValObj; mySelf: ValObj): ValObj 
    RAISES {ServerError, NetObj.Error, SharedObj.Error, Thread.Alerted};
    (* Create a RemObj from another ValObj. *)

  PROCEDURE SetObjPickler(obj: ValObj; picklerIn: ValSimpleObj;
                          picklerOut: ValSimpleObj; mySelf: ValObj)
      RAISES {ServerError, NetObj.Error, SharedObj.Error, Thread.Alerted};
    (* mySelf is the object invoking the pickler setting.  
       set the pickler objects "picklerIn" and "picklerOut" for an object 
       "obj."  These objects are valid iff they have the set of fields
       corresponding to {obj's fields - obj's method fields} (ie. all
       of obj's non-method fields.) *)

  PROCEDURE ObjClone1(remObj: ValObj; mySelf: ValObj): ValObj 
      RAISES {ServerError, NetObj.Error, SharedObj.Error, Thread.Alerted};
    (* mySelf is the object invoking cloning, if any, or NIL. 
       Then cloning is self-inflicted if Is(remObj, mySelf) *)
  PROCEDURE ObjClone(READONLY valObjs: ARRAY OF ValObj; 
                     mySelf: ValObj): ValObj 
    RAISES {ServerError, NetObj.Error, SharedObj.Error, Thread.Alerted};
    (* mySelf is the object invoking cloning, if any, or NIL. 
       Then cloning is self-inflicted if for any Is(remObjs[i],mySelf). *)

  PROCEDURE ObjNotify(val: Val; notifyProc: ValFun);
    (* Create a NetObjNotifier on val if it is a remote data value.
       When val becomes inaccesible, notifyProc is called. 
       notifyProc should take two arguments, the value and a text
       string which will contain "Dead" or "Failed".   See
       NetObjNotifier.i3 for details. *)

  PROCEDURE NewAlias(obj: ValObj; label: TEXT; location: SynLocation.T)
    : ValAlias RAISES {Error, Exception};

  PROCEDURE EngineWho(self: RemEngineServer): TEXT RAISES {NetObj.Error};
  PROCEDURE EngineEval(self: RemEngineServer; proc: Val; mySelf: ValObj): Val 
          RAISES {Error, Exception, ServerError, NetObj.Error};

  PROCEDURE NewFileSystem(readOnly: BOOLEAN): ValFileSystem;
  PROCEDURE FileSystemIs(self: ValFileSystem; other: ValAnything): BOOLEAN;

  PROCEDURE SameException(exc1, exc2: ValException): BOOLEAN;

  PROCEDURE RaiseError(msg: TEXT; location: SynLocation.T) RAISES {Error}; 
  PROCEDURE RaiseException(exception: ValException; msg: TEXT; 
    loc: SynLocation.T) RAISES {Exception};
  PROCEDURE RaiseNetException(msg: TEXT; atoms: AtomList.T; loc: SynLocation.T)
    RAISES {Exception};
  PROCEDURE RaiseSharedException(msg: TEXT; atoms: AtomList.T;
                                 loc: SynLocation.T) RAISES {Exception};

  PROCEDURE ErrorMsg(swr: SynWr.T; packet: ErrorPacket);
  PROCEDURE ExceptionMsg(wr: SynWr.T; packet: ExceptionPacket);

  PROCEDURE BadOp(pkg, op: TEXT; location: SynLocation.T) RAISES {Error};
  PROCEDURE BadArgType(argNo: INTEGER; expected, pkg, op: TEXT; 
    location: SynLocation.T) RAISES {Error};
  PROCEDURE BadArgVal(argNo: INTEGER; expected, pkg, op: TEXT; 
    location: SynLocation.T) RAISES {Error};
  PROCEDURE BadArgsNoMsg(desired, found: INTEGER; 
    routineKind, routineName: TEXT): TEXT;

  PROCEDURE IsSelfOther(self, other: ValAnything): BOOLEAN;
  PROCEDURE PrintWhat(self: ValAnything): TEXT;
  PROCEDURE CopyError(self: ValAnything; tbl: Tbl; 
                      loc: SynLocation.T): ValAnything RAISES {Error};
  PROCEDURE CopyId(self: ValAnything; tbl: Tbl;
                   loc: SynLocation.T): ValAnything;

  PROCEDURE InhibitTransmission(tc: INTEGER; reason: TEXT);
  (* Inhibits the network transmission of any object whose type has
     typecode "tc"; an obliq exception will result. To be used on all
     M3 types that are embedded in obliq values but that have only 
     "local" meaning, such as threads. The reason should be a string
     like "threads cannot be transmitted/duplicated". *)

  PROCEDURE NewTbl(): Tbl;  
  (* A new empty table for CopyVal. *)

  PROCEDURE TblGet (self: Tbl; old: REFANY; VAR (*out*) new: REFANY): BOOLEAN;
  PROCEDURE TblPut (self: Tbl; old, new: REFANY);

  PROCEDURE CopyVal(val: Val; tbl: Tbl; loc: SynLocation.T): Val 
    RAISES {Error, SharedObj.Error, NetObj.Error, Thread.Alerted};
  (* Make a complete local copy of val, preserving sharing and circularities.
     Substructures are fetched over the network, if necessary. Protected
     objects and non-transferrable values produce errors. *)

  PROCEDURE CopyValToLocal(val: Val; tbl: Tbl; loc: SynLocation.T)
    : Val RAISES {Error, SharedObj.Error, NetObj.Error, Thread.Alerted};
  PROCEDURE CopyLocalToVal(val: Val; tbl: Tbl; loc: SynLocation.T)
    : Val RAISES {Error, SharedObj.Error, NetObj.Error, Thread.Alerted};
  (* Internal use only. *)

  VAR
    sysCallFailure: ValException;

  TYPE
    SysCallClosure = OBJECT
    METHODS
      SysCall(READONLY args: Vals; loc: SynLocation.T:=NIL): Val 
        RAISES{Error, Exception};
      (* To be overridden. It should return an obliq Val, or raise an error
         by calling RaiseError, or raise an exception by calling 
         RaiseException. The raised exception should normally be 
         sysCallFailure. The loc parameter should be passed through whenever 
         appropriate, or appriate error locations should be synthesized.
         Alternatively, errors and exceptions may be left uncought, so
         that they propagate back to Obliq; however this may result
         in poor error location reporting. *)
    END;

  PROCEDURE RegisterSysCall(name: TEXT; clos: SysCallClosure);
    (* To register a Modula-3 procedure that can be called from Obliq,
       under a given name. Re-registering for the same name overrides 
       the previous proc for that name. Use clos=NIL to unregister. *)

  PROCEDURE FetchSysCall(name: TEXT; VAR(*out*) clos: SysCallClosure): BOOLEAN;
    (* To fetch a registered Modula-3 procedure by its given name.
       Returns FALSE if not found. *)

END ObValue.
