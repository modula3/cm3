(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(*                                                                           *)
(* Parts Copyright (C) 1997, Columbia University                             *)
(* All rights reserved.                                                      *)
(*
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Sat Apr  4 13:29:41 1998
 *)

INTERFACE Obliq;
IMPORT SynWr, SynLocation, ObTree, ObValue, ObScope, ObCheck, ObLib,
       Thread, SharedObj;
FROM ObValue IMPORT Error, Exception;

(* Program interface to Obliq run-time values and evaluation.
   Other important interfaces are ObTree and ObValue, but the
   present interface attempts to be fairly self-contained.
*)

(* ====== Setup ====== *)

  PROCEDURE PackageSetup(console: SynWr.T);
  (* To be called at least once before any other use of the obliqrt package.
     "console" is the writer to write all output that has nowhere else
     to go. *)

  PROCEDURE Console(): SynWr.T;
    (* Get the console writer *)

(* ====== Types ====== *)

  TYPE 
    Term = ObTree.Term;
    Phrase = ObTree.Phrase;
    Val = ObValue.Val;
    Vals = ARRAY OF Val;
    Fields = ObValue.ObjFields;
    Location = SynLocation.T; 
      (* Error locations: if one is passed to your procedures, pass it along
         to other procedures that take location parameters. Otherwise
         use the NIL defaults for location (for pretty good error reporting)
         or synthesize you own locations (for optimal error reporting). *)
    Env = 
      BRANDED "Obliq.Env" OBJECT
        frameName: TEXT; (* the module name, to handle reloading *)
        forName: TEXT;   (* the qualified name for the operations. *)
        libEnv: ObLib.Env;
        scopeEnv: ObScope.Env;
        checkEnv: ObCheck.Env;
        valueEnv: ObValue.Env;
        nextFrame: Env;
      END;
      (* An interpreter environment. Consider this opaque. *)

(* ====== Environments ====== *)

  PROCEDURE EmptyEnv(wr: SynWr.T): Env;
  (* The empty evaluation environment, containing the currently registered
     built-in modules. This is a legal environment. *)
  
  PROCEDURE NewEnv(wr: SynWr.T; name: TEXT; val: Val; rest: Env; 
                   loc: Location:=NIL)
    : Env RAISES {Error};
  (* Extend an evaluation environment with a new association name-val. 
     This is a legal environment if "rest" is, and if "val" is
     a legal value. *)

  PROCEDURE Lookup(name: TEXT; env: Env): Val RAISES {Error};
  (* Retrieve the value associated with name in env, or Error if not found. *)

(* ====== Eval ====== *)

  PROCEDURE EvalTerm(wr: SynWr.T; term: Term; env: Env; loc: Location:=NIL)
    : Val RAISES {Error, Exception};
  (* Check and evaluate a term in an environment. "env" must be a legal
     environment. Produces a legal value, or an exception.
     A Term can be obtained via ObliqParser.i3. *)

  PROCEDURE EvalPhrase(wr: SynWr.T; phrase: Phrase; VAR (*in-out*) env: Env; 
    loc: Location:=NIL): Val RAISES {Error, Exception};
  (* Check and evaluate a term, definition, or command phrase in an 
     environment. "env" must be a legal environment. Produces an enriched 
     legal environment or an exception. (The result environment is enriched 
     with new bindings when phrase is a definition.) A Phrase can be obtained 
     via ObliqParser.i3. 
     N.B. This procedure does NOT evaluate load phrases and module phrases;
     ObliqParser.EvalPhrase is a more general general procedure that
     also does that. *)

(* ====== Ground types ====== *)

  VAR (*READONLY*) ok: Val;

  VAR (*READONLY*) true, false: Val;
  PROCEDURE NewBool(bool: BOOLEAN): Val;
  PROCEDURE ToBool(val: Val; loc: Location:=NIL): BOOLEAN RAISES{Error};
  PROCEDURE Is(val1, val2: Val): BOOLEAN;

  VAR (*READONLY*) zero, one: Val;
  PROCEDURE NewInt(int: INTEGER): Val;
  PROCEDURE ToInt(val: Val; loc: Location:=NIL): INTEGER RAISES{Error};

  VAR (*READONLY*) zeroPointZero, onePointZero: Val;
  PROCEDURE NewReal(real: LONGREAL): Val;
  PROCEDURE ToReal(val: Val; loc: Location:=NIL): LONGREAL RAISES{Error};

  VAR (*READONLY*) char: ARRAY [0..255] OF Val;
  PROCEDURE NewChar(char: CHAR): Val;
  PROCEDURE ToChar(val: Val; loc: Location:=NIL): CHAR RAISES{Error};

  VAR (*READONLY*) emptyText: Val;
  PROCEDURE NewText(text: TEXT): Val; (* Converts NIL to "" *)
  PROCEDURE ToText(val: Val; loc: Location:=NIL): TEXT RAISES{Error};

(* ====== Objects ====== *)

  PROCEDURE NewObject(READONLY fields: Fields): Val;
  (* Allocates a legal object from a list of fields. This is a legal
     value if the fields have unique names and contain legal values. *)

  (* The fields parameter should contain non-method values only.
     Constructing methods by hand requires knowledge beyond the scope 
     of this interface, and is highly obliq-implementation-dependent. 
     It is much better to call Eval with appropriate arguments, so that 
     objects-with-methods are produced. *)

  PROCEDURE ObjectSelect(object: Val; label: TEXT; swr: SynWr.T; 
                         loc: Location:=NIL; internal:=FALSE): Val 
    RAISES {Error, Exception};
  (* Selects the contents of a field of an object. The value produced
     (if any) is a legal value provided that "object" is both a legal value 
     and an object. If this is executed as if
     from "within" the object, internal should be set to TRUE. *)
    
  PROCEDURE ObjectInvoke(object: Val; label: TEXT; READONLY args: Vals; 
                         swr: SynWr.T; loc: Location:=NIL; 
                         internal:=FALSE): Val RAISES {Error, Exception};
  (* Invokes a method of an object. The value produced (if any) is a 
     legal value provided that "object" is both a legal value and 
     an object, and args is an array of legal values. If this is executed as if
     from "within" the object, internal should be set to TRUE. *)
    
  PROCEDURE ObjectUpdate(object: Val; label: TEXT; val: Val; 
                         loc: Location:=NIL; 
                         internal:=FALSE) RAISES {Error, Exception};
  (* Updates a field or method of an object. The value produced (if any) is a 
     legal value provided that "object" is both a legal value and 
     an object, and val is a legal value. If this is executed as if
     from "within" the object, internal should be set to TRUE. *)

  PROCEDURE ObjectClone1(object: Val; loc: Location:=NIL; self: Val:=NIL): Val
    RAISES {Error, Exception};
  (* Clone a single object. The value produced (if any) is a legal value 
     provided that "object" is both a legal value and an object.  If
     this is executed as if from "within" another object, self should be set
     to that object. *)

  PROCEDURE ObjectClone(READONLY objects: Vals; loc: Location:=NIL; 
                        self: Val:=NIL): Val RAISES {Error, Exception};
  (* Clone many objects into one. The value produced (if any) is a legal value 
     provided that "objects" are both legal values and objects. If
     this is executed as if from "within" another object, self should be set
     to that object. *)

  PROCEDURE ObjectHas(object: Val; label: TEXT; loc: Location:=NIL): 
    BOOLEAN RAISES{Error, Exception};
  (* Whether the object has a field called label. *)

(* ====== Network Objects and Engines ====== *)

  PROCEDURE NetExport(name, server: TEXT; object: Val; 
    loc: SynLocation.T:=NIL) RAISES {Error, Exception};
  (* Export an object to a name server under a name. object must be an object;
     server must be the IP address of a machine running a network object
     daemon. Otherwise an exception is raised. *)

  PROCEDURE NetImport(name, server: TEXT;
    loc: SynLocation.T:=NIL): Val RAISES {Exception};
  (* Import a named object from a name server. server must be the IP address 
     of a machine running a network object, holding the named object. 
     The result is an object, or an exception is raised if the named 
     object cannot be obtained. *)

  PROCEDURE NetExportEngine(name, server: TEXT; arg: Val; 
    loc: SynLocation.T:=NIL) RAISES {Error, Exception};
  (* Export an engine to a name server under a name. arg is given as an
     argument to engine clients; server must be the IP address of a machine 
     running a network object daemon. Otherwise an exception is raised. *)

  PROCEDURE NetImportEngine(name, server: TEXT;
    loc: SynLocation.T:=NIL): Val RAISES {Exception};
  (* Import a named engine from a name server. server must be the IP address 
     of a machine running a network object, holding the named object. 
     The result is an engine that accepts procedures of one argument, 
     (use "Call" to invoke) or an exception is raised if the named engine
     cannot be obtained. *)

  PROCEDURE NetWho(object: Val; loc: SynLocation.T:=NIL): TEXT 
    RAISES {Error, Exception};
  (* Return a description of the origin of an object.
     The precise format is not determined at this moment. *)

(* ====== Replicated Objects ====== *)

  PROCEDURE ReplicaAcquireLock(object: Val; loc:SynLocation.T:=NIL)
    RAISES {Exception};
    (* Acquire a global lock on the replicated object.  It is an
       exception if it is not a replicated object. *)
  PROCEDURE ReplicaReleaseLock(object: Val; loc: SynLocation.T:=NIL)
    RAISES {Exception};
    (* Release a global lock from the replicated object.  It is an
       exception if it is not a replicated object. *)

  PROCEDURE ReplicaSetSiteName(name: TEXT := NIL; 
                               loc: SynLocation.T:=NIL) : TEXT
    RAISES {Exception, Error};
    (* Set the "site name" of the process (site) for replicated object
       system.  Each process has a name, which defaults to the machine
       name if the name passed in is NIL or the empty string.  The
       name used is returned. *)

  PROCEDURE ReplicaSetDefaultSequencer(host, name: TEXT; 
                                       loc: SynLocation.T:=NIL): TEXT
    RAISES {Exception, Error};
    (* Select the process to be used as the default sequencer for the
       replicated object system.  The host is the machine the process
       is running on, and the name is the the name of that process, as
       set locally with "ReplicaSetSiteName."  The name of the
       sequencer is returned. *)

  PROCEDURE ReplicaNotify(object: Val; notifyObj: Val;
                          loc: SynLocation.T := NIL): Val
    RAISES {Exception};
    (* Create a callback object on the replicated object "object."
       The notification methods are in the simple object "notifyObj".
       See the ObValueNotify interface for a more detailed description of
       the relationship between the objects. *)

  PROCEDURE ReplicaCancelNotifier(object: Val; loc: SynLocation.T := NIL)
    RAISES {Exception};
    (* Cancel a callback object.  If all the references to a callback
       object are dropped, when it is garbage collected the
       notification will be cancelled.   However, if it is desirable
       to ensure that notification is cancelled immediately, this
       function can be called. *)

(* ====== Arrays ====== *)

  PROCEDURE NewArray(READONLY vals: Vals): Val;
  PROCEDURE NewReplArray(READONLY vals: Vals): Val  RAISES {SharedObj.Error};
  PROCEDURE NewSimpleArray(READONLY vals: Vals): Val;
  (* Allocates an array value from an array of values. *)
  
  PROCEDURE ArraySize(array: Val; loc: Location:=NIL): INTEGER RAISES {Error};
  (* The size of an array value. *)

  PROCEDURE ArrayGet(array: Val; i: INTEGER; loc: Location:=NIL)
    : Val RAISES {Error};
  (* The ith component of an array value. *)
    
  PROCEDURE ArraySet(array: Val; i: INTEGER; val: Val;
    loc: Location:=NIL) RAISES {Error};
  (* Set the ith componet of an array value to val. *)

  PROCEDURE ArraySub(array: Val; start,size: INTEGER; loc: Location:=NIL)
    : Val RAISES {Error};
  (* Extract a subarray an array value. *)

  PROCEDURE ArrayUpd(array: Val; start, size: INTEGER; sub: Val; 
    loc: Location:=NIL) RAISES {Error};
  (* Update array[start for size] with sub[0 for size]. *)
    
  PROCEDURE ArrayCat(array1, array2: Val; loc: Location:=NIL)
    : Val RAISES {Error};
  (* A new array that is the concatenation of two others. *)

  PROCEDURE ToArray(val: Val; VAR(*out*) array:  Vals; loc: Location:=NIL) 
    RAISES {Error};
  (* Put the elements of an array value into an array. The size of "array"
     must much the array size of "val". *)
  
  PROCEDURE NewIntArray(READONLY array: ARRAY OF INTEGER): Val;
  PROCEDURE ToIntArray(val: Val; VAR(*out*) array: ARRAY OF INTEGER;
    loc: Location:=NIL) RAISES {Error};
  PROCEDURE NewRealArray(READONLY array: ARRAY OF LONGREAL): Val;
  PROCEDURE ToRealArray(val: Val; VAR(*out*) array: ARRAY OF LONGREAL;
    loc: Location:=NIL) RAISES {Error};
  PROCEDURE NewTextArray(READONLY array: ARRAY OF TEXT): Val;
  PROCEDURE ToTextArray(val: Val; VAR(*out*) array: ARRAY OF TEXT;
    loc: Location:=NIL) RAISES {Error};
  (* These utility array routines maps to and from arrays of ground
     types. They use NewArray and ToArray with appopriate coercions
     for the array elements. *)

(* ====== Variables ====== *)

  PROCEDURE NewVar(val: Val): Val;
  PROCEDURE NewReplVar(val: Val): Val RAISES {SharedObj.Error};
  PROCEDURE NewSimpleVar(val: Val): Val;
    (* Create a new variable with given contents. *)

  PROCEDURE VarGet(var: Val; loc: Location:=NIL): Val RAISES {Error};
  (* The contents of a variable. *)
    
  PROCEDURE VarSet(var: Val; val: Val; loc: Location:=NIL) RAISES {Error};
  (* Set the contents of a variable to val. *)

(* ====== Procedures ====== *)

  PROCEDURE Call(proc: Val; READONLY args: Vals; swr: SynWr.T; 
                 loc: Location:=NIL): Val 
    RAISES {Error, Exception};
  (* Take a procedure value with N parameters (i.e. the result of evaluating
     an Obliq program of the form "proc(x1..xn)...end"), and call it 
     (do the equivalent of "(proc(x1..xn)...end)(a1..an)"), returning
     its result. N.B. the original procedure text may have global variables.
     Moreover, proc may be an engine, with the single arg a procedure of
     one argument.
  *)

 (* Constructing procedure values by hand requires knowledge beyond the scope 
    of this interface, and is highly obliq-implementation-dependent. 
    It is much better to call Eval with appropriate arguments, so that 
    procedure values are produced. *)

(* ====== Threads Etc. ====== *)

  PROCEDURE NewMutex(): Val;
    (* Create a new mutex. *)
  
  PROCEDURE MutexGet(mutex: Val; loc: Location:=NIL): Thread.Mutex 
    RAISES {Error};
    (* The Thread.Mutex of a mutex. *)
  
  PROCEDURE NewCondition(): Val;
    (* Create a new condition. *)
  
  PROCEDURE ConditionGet(mutex: Val; loc: Location:=NIL): Thread.Condition 
    RAISES {Error};
    (* The Thread.Condition of a condition. *)
  
  PROCEDURE Fork(proc: Val; stackSize: INTEGER; swr: SynWr.T; 
                 loc: Location:=NIL): Val 
    RAISES {Error};
    (* Fork a procedure of no arguments and return a thread value. *)

  PROCEDURE Join(thread: Val; loc: Location:=NIL): Val 
    RAISES {Error, Exception};
    (* Join a thread and return its result. *)

(* ====== Calling Modula-3 from Obliq via sys_call ====== *)

  VAR 
    sysCallFailure: ObValue.ValException;
    (* An exception to be raised by SysCall procedures. *)

  TYPE
    SysCallClosure = OBJECT
    METHODS
      SysCall(READONLY args: Vals; swr: SynWr.T; loc: Location:=NIL): Val 
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

  PROCEDURE RaiseSysCallFailure(self: SysCallClosure; READONLY args: Vals;
    loc: Location:=NIL): Val RAISES{Error, Exception};
    (* A SysCallProc that simply raises sysCallFailure. *)

(* ====== Errors and Exceptions ====== *)

  PROCEDURE RaiseError(msg: TEXT; loc: Location:=NIL) RAISES {Error};
    (* Raises an Obliq error. *)

  PROCEDURE NewException(name: TEXT): ObValue.ValException;
    (* Creates a new obliq exception; the name is used for exception
       matching in try expressions. *)

  PROCEDURE RaiseException(exception: ObValue.ValException; msg: TEXT;
    loc: Location:=NIL) RAISES {Exception};
    (* Raises an Obliq exception. *)

  PROCEDURE ReportError(swr: SynWr.T; packet: ObValue.ErrorPacket);
  PROCEDURE ReportException(swr: SynWr.T; packet: ObValue.ExceptionPacket);
  (* Report error conditions and return normally. 
     Use to intercept Obliq errors and report them:
       TRY
          ... code that raises ObValue.Error and ObValue.Exception ...
       EXCEPT ObValue.Error(packet) => Obliq.ReportError(swr, packet);
       |      ObValue.Exception(packet) => Obliq.ReportException(swr, packet);
       END; *)

END Obliq.
