(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
<*PRAGMA SHARED *>
(*                                                                           *)
(* Parts Copyright (C) 1997, Columbia University                             *)
(* All rights reserved.                                                      *)
(*
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Sat Apr  4 13:46:18 1998
 *)

INTERFACE ObValue;
IMPORT SynWr, SynLocation, Rd, Wr, ObTree, NetObj, Thread, AtomList,
       File, Time, Pathname, Process;
IMPORT SharedObj;

  EXCEPTION 
    Exception(ExceptionPacket); (* trappable     *)
    Error(ErrorPacket);         (* only trappable by try-else *)
    ServerError(TEXT);  (* like Error, but has no location info;
                           immediately converted to Error 
                           on the way back from a server *)
  TYPE
    ErrorPacket =
      BRANDED "ObValue.ErrorPacket" OBJECT
        msg: TEXT;
        location: SynLocation.T;
      END;
  
    ExceptionPacket =
      BRANDED "ObValue.ExceptionPacket" OBJECT
        msg: TEXT;
        location: SynLocation.T;
        exception: ValException;
        data: Val;
      END;

    Tbl <: REFANY;

  TYPE
    Vals = ARRAY OF Val;

    Env = BRANDED "ObValue.Env" OBJECT 
	name: ObTree.IdeName;
        rest: Env;
      END;

    LocalEnv =
      Env BRANDED "ObValue.LocalEnv" OBJECT
	val: Val;
      END;

    GlobalEnv =
      REF Vals;

    Val = 
      BRANDED "ObValue.Val" OBJECT
      END;

    ValOk = 
      Val BRANDED "ObValue.ValOk" OBJECT 
      END;

    ValBool = 
      Val BRANDED "ObValue.ValBool" OBJECT 
	bool: BOOLEAN;
      END;

    ValChar = 
      Val BRANDED "ObValue.ValChar" OBJECT 
	char: CHAR;
      END;

    ValText = 
      Val BRANDED "ObValue.ValText" OBJECT 
	text: TEXT; (* Non-NIL ! *)
      END;

    ValInt = 
      Val BRANDED "ObValue.ValInt" OBJECT 
	int: INTEGER;
	temp: BOOLEAN:=FALSE;
      END;

    ValReal = 
      Val BRANDED "ObValue.ValReal" OBJECT
	real: LONGREAL;
	temp: BOOLEAN:=FALSE;
      END;

    ValException = 
      Val BRANDED "ObValue.ValException" OBJECT
        name: TEXT;
      END;

    ValOption = 
      Val BRANDED "ObValue.ValOption" OBJECT 
	tag: TEXT;
        val: Val;
      END;

    ValFun = 
      Val BRANDED "ObValue.ValFun" OBJECT 
        fun: ObTree.TermFun;
	global: GlobalEnv;
      END;

    ValMeth = 
      Val BRANDED "ObValue.ValMeth" OBJECT 
        meth: ObTree.TermMeth;
	global: GlobalEnv;
      END;

    ValAlias = 
      Val BRANDED "ObValue.ValAlias" OBJECT 
	label: TEXT;
	labelIndexHint: INTEGER;
        obj: ValObj;
      END;

    ValAnything = (* to be subtyped *)
      Val BRANDED "ObValue.ValAnything" OBJECT
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

    ValEngine =
      Val BRANDED "ObValue.ValEngine" OBJECT
        remote: RemEngine;
      END;

    ValFileSystem =
      ValAnything BRANDED "ObValue.ValFileSystem" OBJECT
        remote: RemFileSystem;
      OVERRIDES
        Is := FileSystemIs;
        Copy := CopyId;
      END;

    ValIterator =
      ValAnything BRANDED "ObValue.ValIterator" OBJECT
        remote: RemIterator;
      OVERRIDES
        Is := IteratorIs;
        Copy := CopyId;
      END;

    ValProcessor =
      ValAnything BRANDED "ObValue.ValProcessor" OBJECT
        remote: RemProcessor;
      OVERRIDES
        Is := ProcessorIs;
        Copy := CopyId;
      END;

    ValRd =
      ValAnything BRANDED "ObValue.ValRd" OBJECT
        rd: Rd.T;
      OVERRIDES 
        Is := RdIs; 
        Copy := CopyId;
      END;

    ValWr =
      ValAnything BRANDED "ObValue.ValWr" OBJECT
        wr: Wr.T;
      OVERRIDES
        Is := WrIs; 
        Copy := CopyId;
      END;

    ValProcess =
      ValAnything BRANDED "ObValue.VarProcess" OBJECT
        remote: RemProcess;
        in: ValWr;
        out: ValRd;
        err: ValRd;
      OVERRIDES
        Is := ProcessIs;
      END;

    (* There are 3 kinds of objects, Remote (Network Objects),
       Replicated (Shared Objects) and Simple (normal Modula-3
       Objects).  The Obliq wrappers contain a reference to the real
       data objects in an appropriately typed data field.

       These are ValObj wrapper methods allow each subtype to redirect
       the method calls to their approriate data object (RemObj,
       ReplObj or SimpleObj). *)
    ValArray = 
      Val BRANDED "ObValue.ValArray" OBJECT METHODS
       Size(): INTEGER RAISES {SharedObj.Error, NetObj.Error, Thread.Alerted};
       Get(i: INTEGER): Val RAISES {SharedObj.Error, ServerError,
                                    NetObj.Error, Thread.Alerted}; 
       Set(i: INTEGER; val: Val) 
         RAISES {SharedObj.Error, ServerError, NetObj.Error, Thread.Alerted};
       Sub(start,size: INTEGER): ValArray 
           RAISES {SharedObj.Error, ServerError, NetObj.Error, Thread.Alerted};
         (* Extract the subarray self[start for size]. *)
       Upd(start, size: INTEGER; READONLY other: REF Vals) 
           RAISES {SharedObj.Error, ServerError, NetObj.Error, Thread.Alerted};
         (* Update self[start for size] with other[0 for size]. *)
       Obtain(): REF Vals RAISES {SharedObj.Error, NetObj.Error,
                                  Thread.Alerted}; 
         (* Return self.array if local, or a copy of it if remote.
            Modifying the result of Obtain may violate network transparency. *)
      END;

    ValVar =
      Val BRANDED "ObValue.ValVar" OBJECT
      METHODS
        Get(): Val RAISES {SharedObj.Error, NetObj.Error, Thread.Alerted};
        Set(val: Val) RAISES {SharedObj.Error, NetObj.Error, Thread.Alerted};
      END;

    ValObj =
      Val BRANDED "ObValue.ValObj" OBJECT METHODS
        Who(VAR(*out*) protected, serialized: BOOLEAN): TEXT 
          RAISES {NetObj.Error, SharedObj.Error, Thread.Alerted};
        Select(swr: SynWr.T; label: TEXT; internal: BOOLEAN; 
               VAR hint: INTEGER): Val 
          RAISES {Error, Exception, ServerError, SharedObj.Error, 
                  NetObj.Error, Thread.Alerted};
        Invoke(swr: SynWr.T; label: TEXT; argNo: INTEGER; READONLY args: Vals;
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

        (* these last ones are used by the reflection package *)
        ObtainField(label: TEXT; internal: BOOLEAN): Val
          RAISES {ServerError, SharedObj.Error, NetObj.Error, Thread.Alerted};
        ObtainDescriptions(): REF ObjFieldTypes
          RAISES {ServerError, SharedObj.Error, NetObj.Error, Thread.Alerted};
        Describe(label: TEXT): TEXT
          RAISES {ServerError, SharedObj.Error, NetObj.Error, Thread.Alerted};
      END;

    ValRemObj <: ValRemObjPublic;
    ValRemObjPublic = ValObj OBJECT
        remote: RemObj;
      END;

    ValRemVar <: ValRemVarPublic;
    ValRemVarPublic = ValVar OBJECT
        remote: RemVar;
      END;

    ValRemArray <: ValRemArrayPublic;
    ValRemArrayPublic = ValArray OBJECT
        remote: RemArray;
      END;

    ValReplObj <: ValReplObjPublic;
    ValReplObjPublic = ValObj OBJECT
        replica: ReplObj;
      END;

    ValReplVar <: ValReplVarPublic;
    ValReplVarPublic = ValVar OBJECT
        replica: ReplVar;
      END;

    ValReplArray <: ValReplArrayPublic;
    ValReplArrayPublic = ValArray OBJECT
        replica: ReplArray;
      END;

    ValSimpleObj <: ValSimpleObjPublic;
    ValSimpleObjPublic = ValObj OBJECT
        simple: SimpleObj;
      END;

    ValSimpleVar <: ValSimpleVarPublic;
    ValSimpleVarPublic = ValVar OBJECT
        simple: SimpleVar;
      END;

    ValSimpleArray <: ValSimpleArrayPublic;
    ValSimpleArrayPublic = ValArray OBJECT
        simple: SimpleArray;
      END;

    ObjFields = ARRAY OF Field;
    Field = RECORD
        label: TEXT;
        field: Val; 
        (* ValMeth for method fields, 
           ValAlias for alias fields, 
           other Val for value fields *)
      END;

    ObjFieldTypes = ARRAY OF FieldType;
    FieldType = RECORD
        label: TEXT;
        type: TEXT; 
      END;

    (* RemObjServer, a subtype of RemObj, is the standard implementation of 
       local objects. Another subtype of RemObj is automatically produced by 
       the Network Objects stub generator, for remote surrogates. Further 
       subtypes of RemObj can be defined and used as client-specific 
       pseudo-objects. In the latter case, the result of Who is used for 
       printing; copying, cloning, and pickling operate on the results of 
       Who and Obtain.
       *)
    RemObjServer <: RemObjServerPublic;
    RemObjServerPublic = RemObj BRANDED "ObValue.RemObjServerPublic" OBJECT
        who: TEXT; 
        sync: Sync;
      END;
    RemObj = NetObj.T BRANDED "ObValue.RemObj" OBJECT
      METHODS
        Who(VAR(*out*) protected, serialized: BOOLEAN): TEXT 
          RAISES {NetObj.Error, Thread.Alerted};
        Select(swr: SynWr.T; label: TEXT; internal: BOOLEAN; 
               VAR hint: INTEGER): Val 
          RAISES {Error, Exception, ServerError, SharedObj.Error, 
                  NetObj.Error, Thread.Alerted};
        Invoke(swr: SynWr.T; label: TEXT; argNo: INTEGER; READONLY args: Vals;
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

        ObtainField(label: TEXT; internal: BOOLEAN): Val
          RAISES {ServerError, NetObj.Error, Thread.Alerted};
        ObtainDescriptions():REF ObjFieldTypes
          RAISES {ServerError, SharedObj.Error, NetObj.Error,Thread.Alerted};
        Describe(label: TEXT): TEXT 
          RAISES {ServerError, SharedObj.Error, NetObj.Error, Thread.Alerted};
      END;

    RemVarServer <: RemVar;
    RemVar = NetObj.T BRANDED "ObValue.RemVar" OBJECT
      METHODS
        Get(): Val RAISES {NetObj.Error, Thread.Alerted};
        Set(val: Val) RAISES {NetObj.Error, Thread.Alerted};
      END;

    RemArrayServer <: RemArray;
    RemArray = NetObj.T BRANDED "ObValue.RemArray" OBJECT METHODS
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
     
    ReplObjStd <: ReplObj;
        <* SHARED UPDATE METHODS ReplObjStd.init, ReplObjStd.InvokeUpdate, 
                                ReplObjStd.Update, ReplObjStd.RedirectFields *>
    ReplObj <: ReplObjPublic;
    ReplObjPublic = SharedObj.T BRANDED "ObValue.ReplObjServerPublic" OBJECT
      METHODS
        init (): ReplObj RAISES {SharedObj.Error};
        Who(VAR(*out*) protected: BOOLEAN): TEXT 
          RAISES {SharedObj.Error};
        (* All replicated objects are protected and serialized! *)
        Select(swr: SynWr.T; label: TEXT; VAR hint: INTEGER): Val 
          RAISES {Error, Exception, ServerError, SharedObj.Error};
        Invoke(swr: SynWr.T; label: TEXT; argNo: INTEGER; READONLY args: Vals;
               VAR hint: INTEGER): Val
          RAISES {Error, Exception, ServerError, SharedObj.Error};
        Update(label: TEXT; val: Val; internal: BOOLEAN; 
               VAR hint: INTEGER) RAISES {ServerError, SharedObj.Error};
        Redirect(val: Val; internal: BOOLEAN) 
          RAISES {ServerError, SharedObj.Error};
        Has(label: TEXT; VAR hint: INTEGER): BOOLEAN RAISES {SharedObj.Error};
          (* Whether a field called label exists. *)
        Obtain(internal: BOOLEAN): REF ObjFields
          RAISES {ServerError, SharedObj.Error};
          (* Return self.fields. Modifying the result of Obtain may
             violate network transparency. *)

        ObtainField(label: TEXT; internal: BOOLEAN): Val
          RAISES {ServerError, SharedObj.Error};
        ObtainDescriptions(): REF ObjFieldTypes 
          RAISES {ServerError, SharedObj.Error};
        Describe(label: TEXT): TEXT RAISES {ServerError, SharedObj.Error};
      END;

    ReplArrayStd <: ReplArray;
        <* SHARED UPDATE METHODS ReplArrayStd.init, ReplArrayStd.Set, 
                                 ReplArrayStd.Upd *>
    ReplArray <: ReplArrayPublic;
    ReplArrayPublic = SharedObj.T BRANDED "ObValue.ReplArrayServerPublic" OBJECT
      METHODS
        init (): ReplArray RAISES {SharedObj.Error};
        Size(): INTEGER RAISES {SharedObj.Error};
        Get(i: INTEGER): Val RAISES {ServerError, SharedObj.Error};
        Set(i: INTEGER; val: Val) RAISES {ServerError, SharedObj.Error};
        Sub(start,size: INTEGER): ValArray 
          RAISES {ServerError, SharedObj.Error};
          (* Extract the subarray self[start for size]. *)
        Upd(start, size: INTEGER; READONLY other: REF Vals) 
          RAISES {ServerError, SharedObj.Error};
          (* Update self[start for size] with other[0 for size]. *)
        Obtain(): REF Vals RAISES {SharedObj.Error};
          (* Return self.array if local, or a copy of it if remote.
             Modifying the result of Obtain may violate network transparency.*)
      END;
     
    ReplVarStd <: ReplVar;
        <* SHARED UPDATE METHODS ReplVarStd.init, ReplVarStd.Set *>
    ReplVar <: ReplVarPublic;
    ReplVarPublic = SharedObj.T BRANDED "ObValue.ReplVarServerPublic" OBJECT
      METHODS
        init (): ReplVar RAISES {SharedObj.Error};
        Get(): Val RAISES {SharedObj.Error};
        Set(val: Val) RAISES {SharedObj.Error};
      END;
    (* ReplObj and ReplVar are the implementations of replicated
       objects and variables.  The default internal implementation is
       ReplObjStd and ReplVarStd.  Further subtypes can be defined and
       used as client-specific pseudo-objects.  *)

    SimpleObj <: SimpleObjPublic;
    SimpleObjPublic = BRANDED "ObValue.SimpleObjPublic" OBJECT
        who: TEXT; 
        sync: Sync;
      METHODS
        Who(VAR(*out*) protected, serialized: BOOLEAN): TEXT;
        Select(swr: SynWr.T; label: TEXT; internal: BOOLEAN; 
               VAR hint: INTEGER): Val 
          RAISES {Error, Exception, ServerError, 
                  SharedObj.Error, NetObj.Error, Thread.Alerted};
        Invoke(swr: SynWr.T; label: TEXT; argNo: INTEGER; READONLY args: Vals;
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

        ObtainField(label: TEXT; internal: BOOLEAN): Val
          RAISES {ServerError, SharedObj.Error, NetObj.Error, Thread.Alerted};
        ObtainDescriptions(): REF ObjFieldTypes
          RAISES {ServerError, SharedObj.Error, NetObj.Error, Thread.Alerted};
        Describe(label: TEXT): TEXT
          RAISES {ServerError, SharedObj.Error, NetObj.Error, Thread.Alerted};
      END;

    SimpleVar <: SimpleVarPublic;
    SimpleVarPublic = BRANDED "ObValue.SimpleVarPublic" OBJECT METHODS
        Get(): Val RAISES {SharedObj.Error, NetObj.Error, Thread.Alerted};
        Set(val: Val) RAISES {SharedObj.Error, NetObj.Error, Thread.Alerted};
      END;

    SimpleArray <: SimpleArrayPublic;
    SimpleArrayPublic = BRANDED "ObValue.SimpleArrayPublic" OBJECT 
      METHODS
        Size(): INTEGER RAISES {SharedObj.Error, NetObj.Error, Thread.Alerted};
        Get(i: INTEGER): Val RAISES {SharedObj.Error, ServerError,
                                     NetObj.Error, Thread.Alerted}; 
        Set(i: INTEGER; val: Val) 
          RAISES {SharedObj.Error, ServerError, NetObj.Error, Thread.Alerted};
        Sub(start,size: INTEGER): ValArray 
          RAISES {SharedObj.Error, ServerError, NetObj.Error, Thread.Alerted};
          (* Extract the subarray self[start for size]. *)
        Upd(start, size: INTEGER; READONLY other: REF Vals) 
          RAISES {SharedObj.Error, ServerError, NetObj.Error, Thread.Alerted};
          (* Update self[start for size] with other[0 for size]. *)
        Obtain(): REF Vals RAISES {SharedObj.Error, NetObj.Error,
                                   Thread.Alerted}; 
          (* Return self.array if local, or a copy of it if remote.
             Modifying the result of Obtain may violate network transparency.*)
      END;

    NonRemValHookServer <: NonRemValHook;
    NonRemValHook =
      NetObj.T BRANDED "ObValue.NonRemValHook" OBJECT
      METHODS
        init(val: Val): NonRemValHook  RAISES {NetObj.Error,
                                                     Thread.Alerted}; 
        Get(): Val RAISES {NetObj.Error, Thread.Alerted};
      END;
    (* NonRemValHook is used by the obliq net_import and net_export
       commands to allow replicated and simple values to be exported and
      imported. *)

    Sync =
      BRANDED "ObValue.Sync" OBJECT
        mutex: Thread.Mutex;
      END;

    RemEngine =
      NetObj.T BRANDED "ObValue.RemEngine" OBJECT
      METHODS
        Who(): TEXT RAISES {NetObj.Error, Thread.Alerted};
        Eval(proc: Val; mySelf: ValObj): Val 
          RAISES {Error, Exception, ServerError, NetObj.Error, Thread.Alerted};
      END;

    RemEngineServer =
      RemEngine BRANDED "ObValue.RemEngineServer" OBJECT
        who: TEXT;
        arg: Val;
      OVERRIDES
        Who := EngineWho;
        Eval := EngineEval;
      END;

    RemFileSystem =
      NetObj.T BRANDED "ObValue.RemFileSystem" OBJECT
      METHODS
        OpenRead(fileName: TEXT): Rd.T 
          RAISES {NetObj.Error, ServerError, Thread.Alerted};
        OpenWrite(fileName: TEXT): Wr.T 
          RAISES {NetObj.Error, ServerError, Thread.Alerted};
        OpenAppend(fileName: TEXT): Wr.T 
          RAISES {NetObj.Error, ServerError, Thread.Alerted};
        GetAbsolutePathname(path: TEXT): TEXT
          RAISES {NetObj.Error, ServerError, Thread.Alerted};
        CreateDirectory(path: TEXT)
          RAISES {NetObj.Error, ServerError, Thread.Alerted};
        DeleteDirectory(path: TEXT)
          RAISES {NetObj.Error, ServerError, Thread.Alerted};
        DeleteFile(path: TEXT)
          RAISES {NetObj.Error, ServerError, Thread.Alerted};
        Rename(path1, path2: TEXT)
          RAISES {NetObj.Error, ServerError, Thread.Alerted};
        Iterate(path1: TEXT): RemIterator
          RAISES {NetObj.Error, ServerError, Thread.Alerted};
        Status(path1: TEXT): File.Status
          RAISES {NetObj.Error, ServerError, Thread.Alerted};
        SetModificationTime(path1: TEXT; time: Time.T)
          RAISES {NetObj.Error, ServerError, Thread.Alerted};

        (* path support *)
        PathSep(): TEXT RAISES {NetObj.Error, Thread.Alerted};
        PathSearchSep(): TEXT RAISES {NetObj.Error, Thread.Alerted};
        PathCurrent(): TEXT RAISES {NetObj.Error, Thread.Alerted};
        PathParent(): TEXT RAISES {NetObj.Error, Thread.Alerted};
        PathValid(pn: Pathname.T): BOOLEAN RAISES {NetObj.Error, Thread.Alerted};
        PathDecompose(pn: Pathname.T): Pathname.Arcs 
          RAISES {NetObj.Error, Pathname.Invalid, Thread.Alerted};
        PathCompose(a: Pathname.Arcs): Pathname.T
          RAISES {NetObj.Error, Pathname.Invalid, Thread.Alerted};
        PathAbsolute(pn: Pathname.T): BOOLEAN 
          RAISES {NetObj.Error, Thread.Alerted};
        PathPrefix(pn: Pathname.T): Pathname.T 
          RAISES {NetObj.Error, Thread.Alerted};
        PathLast(pn: Pathname.T): Pathname.T 
          RAISES {NetObj.Error, Thread.Alerted};
        PathBase(pn: Pathname.T): Pathname.T 
          RAISES {NetObj.Error, Thread.Alerted};
        PathJoin(pn, base: Pathname.T; ext: TEXT): Pathname.T
          RAISES {NetObj.Error, Thread.Alerted};
        PathLastBase(pn: Pathname.T): Pathname.T 
          RAISES {NetObj.Error, Thread.Alerted};
        PathLastExt(pn: Pathname.T): TEXT
          RAISES {NetObj.Error, Thread.Alerted};
        PathReplaceExt(pn: Pathname.T; ext: TEXT): Pathname.T 
          RAISES {NetObj.Error, Thread.Alerted};
      END;
    RemFileSystemServer <: RemFileSystem;

    RemIterator =
      NetObj.T BRANDED "ObValue.RemIterator" OBJECT
      METHODS
        Next(VAR (*OUT*) name: TEXT): BOOLEAN
          RAISES {NetObj.Error, Thread.Alerted};
        NextWithStatus(VAR (*OUT*) name: TEXT;
                       VAR (*OUT*) stat: File.Status): BOOLEAN 
          RAISES {NetObj.Error, ServerError, Thread.Alerted};
        Close() RAISES {NetObj.Error, Thread.Alerted};
      END;
    RemIteratorServer <: RemIterator;

    RemProcessor =
      NetObj.T BRANDED "ObValue.RemProcessor" OBJECT
      METHODS
        CreateProcess(cmd: TEXT; READONLY params: ARRAY OF TEXT;
                      env: REF ARRAY OF TEXT := NIL;
                      wd: TEXT := NIL; mergeOut: BOOLEAN;
                      VAR (*out*) stdin: Wr.T;
                      VAR (*out*) stdout: Rd.T;
                      VAR (*out*) stderr: Rd.T): RemProcess
          RAISES {NetObj.Error, ServerError, Thread.Alerted};
        GetWorkingDirectory(): TEXT
          RAISES {NetObj.Error, ServerError, Thread.Alerted};
        SetWorkingDirectory(path: TEXT)
          RAISES {NetObj.Error, ServerError, Thread.Alerted};
      END;
    RemProcessorServer <: RemProcessor;

    RemProcess =
      NetObj.T BRANDED "ObValue.RemProcess" OBJECT
      METHODS
        Wait(): Process.ExitCode 
          RAISES {NetObj.Error, ServerError, Thread.Alerted};
        GetID(): Process.ID
          RAISES {NetObj.Error, ServerError, Thread.Alerted};
      END;
    RemProcessServer <: RemProcess;

    ArgArray = ARRAY [1..8] OF Val;

  VAR 
    valOk: Val;
    netException, threadAlerted, osError, sharedException, 
         sharedFatal: ValException;
    showNetObjMsgs: BOOLEAN;
    machineAddress: TEXT;
    localProcessor: ValProcessor;

  PROCEDURE Setup();
    (* To be called before any other use of this module. *)

  PROCEDURE Is(v1,v2: Val; location: SynLocation.T): BOOLEAN;

  PROCEDURE NewText(text: TEXT): Val;
    (* Create an Obliq text containing a non-NIL m3 TEXT. *)

  PROCEDURE NewVar(val: Val): ValVar;
  PROCEDURE NewReplVar(val: Val): ValVar RAISES {SharedObj.Error};
  PROCEDURE NewSimpleVar(val: Val): ValVar;
    (* Create a new variable. *)

  PROCEDURE NewArray(READONLY vals: Vals): ValArray;
  PROCEDURE NewReplArray(READONLY vals: Vals): ValArray 
    RAISES {SharedObj.Error};
  PROCEDURE NewSimpleArray(READONLY vals: Vals): ValArray;
    (* Create a new array. *)

  PROCEDURE NewArrayFromVals(vals: REF Vals): ValArray;
  PROCEDURE NewReplArrayFromVals(vals: REF Vals): ValArray
    RAISES {SharedObj.Error};
  PROCEDURE NewSimpleArrayFromVals(vals: REF Vals): ValArray;
    (* Careful: the vals passed in are shared and may get modified later. *)

  PROCEDURE ArrayCat(vals1, vals2: REF Vals): Val;
  PROCEDURE ReplArrayCat(vals1, vals2: REF Vals): Val RAISES {SharedObj.Error};
  PROCEDURE SimpleArrayCat(vals1, vals2: REF Vals): Val;

  PROCEDURE NewObject(READONLY fields: ObjFields; 
                      who: TEXT:=""; protected: BOOLEAN:=FALSE; 
                      sync: Sync:=NIL): ValObj;
  PROCEDURE NewReplObject(READONLY fields: ObjFields; 
                      who: TEXT:=""; protected: BOOLEAN:=FALSE): ValObj
    RAISES {ServerError, SharedObj.Error};
  PROCEDURE NewSimpleObject(READONLY fields: ObjFields; 
                      who: TEXT:=""; protected: BOOLEAN:=FALSE; 
                      sync: Sync:=NIL): ValObj;
    (* Create a new object. *)

  PROCEDURE NewObjectFromFields(fields: REF ObjFields; 
                                who: TEXT; protected: BOOLEAN; 
                                sync: Sync): ValObj;
  PROCEDURE NewReplObjectFromFields(fields: REF ObjFields; 
                                who: TEXT; protected: BOOLEAN): ValObj
    RAISES {ServerError, SharedObj.Error};
  PROCEDURE NewSimpleObjectFromFields(fields: REF ObjFields; 
                                who: TEXT; protected: BOOLEAN; 
                                sync: Sync): ValObj;
    (* Careful: the fields passed in are shared and may get modified later. *)
    (* Also, the fields are sorted by "label" *)

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

  PROCEDURE ObjNotify(val: Val; notifyProc: ValFun; swr: SynWr.T);
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
  PROCEDURE IteratorIs(self: ValIterator; other: ValAnything): BOOLEAN;

  PROCEDURE ProcessorIs(self: ValProcessor; other: ValAnything): BOOLEAN;
  PROCEDURE NewProcess (proc: Process.T): RemProcess;
  PROCEDURE ProcessIs(self: ValProcess; other: ValAnything): BOOLEAN;

  PROCEDURE NewRd(rd: Rd.T; what: TEXT := "<a reader>"): ValRd;
  PROCEDURE RdIs(self: ValRd; other: ValAnything): BOOLEAN;
  PROCEDURE NewWr(wr: Wr.T; what: TEXT := "<a writer>"): ValWr;
  PROCEDURE WrIs(self: ValWr; other: ValAnything): BOOLEAN;

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

  PROCEDURE GetTypeString (val: Val): TEXT;
  (* Return a string describing the obliq Val. *)

  VAR
    sysCallFailure: ValException;

  TYPE
    SysCallClosure = OBJECT
    METHODS
      SysCall(READONLY args: Vals; swr: SynWr.T; loc: SynLocation.T:=NIL): Val 
        RAISES{Error, Exception};
      (* To be overridden. It should return an obliq Val, or raise an error
         by calling RaiseError, or raise an exception by calling 
         RaiseException. The raised exception should normally be 
         sysCallFailure. The swr parameter should be used for any text
         output. The loc parameter should be passed through whenever 
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
