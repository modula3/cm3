
(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

MODULE Obliq;
IMPORT Thread, SynWr, SynLocation, ObErr, ObCommand, ObLib, ObTree, ObScope, 
       ObCheck, ObValue, ObEval, ObBuiltIn, NetObj, Text, SharedObj,
       MxConfig, ObPathSep;
FROM ObValue IMPORT Error, Exception;

  VAR 
    setupDone := FALSE;

  PROCEDURE PackageSetup() =
  BEGIN
    IF NOT setupDone THEN
      setupDone := TRUE;
      ObCommand.Setup();
      ObLib.Setup();
      ObTree.Setup();
      ObScope.Setup();
      ObCheck.Setup();
      ObValue.Setup();
      ObEval.Setup();
      ObBuiltIn.Setup();
      Setup();
    END;
  END PackageSetup;

  PROCEDURE Setup() =
  BEGIN
    Thread.IncDefaultStackSize(64*1024);

    ok := ObValue.valOk;
    true := NEW(ObValue.ValBool, bool:=TRUE);
    false := NEW(ObValue.ValBool, bool:=FALSE);
    zero := NEW(ObValue.ValInt, int:=0, temp:=FALSE);
    one := NEW(ObValue.ValInt, int:=1, temp:=FALSE);
    zeroPointZero := NEW(ObValue.ValReal, real:=0.0d0, temp:=FALSE);
    onePointZero := NEW(ObValue.ValReal, real:=1.0d0, temp:=FALSE);
    FOR i:=0 TO 255 DO char[i] := NEW(ObValue.ValChar, char:=VAL(i, CHAR)) END;
    emptyText := NEW(ObValue.ValText, text:="");

    sysCallFailure := ObValue.sysCallFailure;
  END Setup;

  PROCEDURE EmptyEnv(): Env =
  VAR env: Env;
  BEGIN
    env :=
      NEW(Env, frameName := "", forName := "",
          libEnv := ObLib.libraries, 
          scopeEnv := NIL, checkEnv := NIL, valueEnv := NIL, 
          nextFrame := NIL);
    TRY
      env := NewEnv("hostName", NewText(ObValue.machineAddress), env);
      env := NewEnv("target", NewText(MxConfig.HOST()), env);
      env := NewEnv("osType", NewText(MxConfig.HOST_OS_TYPE_TEXT()), env);
      env := NewEnv("pathSep", NewText(MxConfig.HOST_PATH_SEP), env);
      env := NewEnv("searchPathSep", 
                    NewText(Text.FromChar(ObPathSep.SearchPathSeparator)),
                    env);
      env := NewEnv("fileSysReader", 
                    ObValue.NewFileSystem(readOnly:=TRUE), env);
      env := NewEnv("fileSys", ObValue.NewFileSystem(readOnly:=FALSE), env);
      env := NewEnv("processor", ObValue.localProcessor, env);
    EXCEPT ELSE <* ASSERT FALSE *> (* should never fail *) END;
    RETURN env;
  END EmptyEnv;

  PROCEDURE NewEnv(name: TEXT; val: Val; rest: Env; loc: Location:=NIL)
    : Env RAISES {Error} =
  VAR ideName: ObTree.IdeName;
  BEGIN
    IF loc=NIL THEN loc:=SourceLocation("Obliq.NewEnv") END;
    TRY
      ideName := NEW(ObTree.IdeName, text:=name, variant:=0);
      (* return a copy of rest, but whose component envs grow by 1 *)
      RETURN NEW(Env,
        frameName := rest.frameName,
        forName := rest.forName,
        libEnv := rest.libEnv,
        scopeEnv := 
            ObScope.NewTermEnv(loc, ideName, rest.libEnv, rest.scopeEnv),
        checkEnv := ObCheck.NewTermEnv(ideName, rest.checkEnv),
        valueEnv := NEW(ObValue.LocalEnv, name:=ideName, val:=val, 
                        rest:=rest.valueEnv), nextFrame := rest.nextFrame);
    EXCEPT
    | ObErr.Fail => 
        SynWr.Flush(SynWr.out); 
        RaiseError("Static Error", loc); 
        <*ASSERT FALSE*>
    END;
  END NewEnv;  

  PROCEDURE Lookup(name: TEXT; env: Env): Val RAISES {Error} =
  VAR checkEnv: ObCheck.Env; valueEnv: ObValue.Env;
  BEGIN
    checkEnv := env.checkEnv;
    valueEnv := env.valueEnv;
    LOOP
      IF (checkEnv=NIL) OR (valueEnv=NIL) THEN EXIT END;
      TYPECASE checkEnv OF
      | ObCheck.TermEnv(checkNode) =>
	  TYPECASE valueEnv OF
	  | ObValue.LocalEnv(valueNode) =>
	    IF Text.Equal(name, checkNode.name.text) THEN
	      RETURN valueNode.val;
	    END;
	  ELSE EXIT
	  END;
      ELSE EXIT
      END;
      checkEnv := checkEnv.rest;
      valueEnv := valueEnv.rest;
    END;
    RaiseError("Obliq.Lookup: '" & name & "' not found", NIL);
    <*ASSERT FALSE*>
  END Lookup;

  PROCEDURE EvalTerm(term: Term; env: Env; loc :Location:=NIL)
    : Val RAISES {Error, Exception} =
  VAR scopeEnv, freeEnv: ObScope.Env; checkEnv: ObCheck.Env;
    valueEnv: ObValue.Env; 
  BEGIN
    IF loc=NIL THEN loc:=term.location END;
    TRY
      scopeEnv := env.scopeEnv;
      checkEnv := env.checkEnv;
      valueEnv := env.valueEnv;
      freeEnv := NIL;
      ObScope.ScopeTerm(term, env.libEnv, 
                        (*in-out*)scopeEnv, (*in-out*)freeEnv);
      ObScope.UnboundIdes(freeEnv);
      ObCheck.CheckTerm(term, (*in-out*)checkEnv);
      RETURN ObEval.Term(term, (*in-out*)valueEnv, NIL, NIL);
    EXCEPT
    | ObErr.Fail => 
        SynWr.Flush(SynWr.out); 
        RaiseError("Static Error", loc);
        <*ASSERT FALSE*>
    END;
  END EvalTerm;

  PROCEDURE EvalPhrase(phrase: Phrase; VAR (*in-out*) env: Env; 
    loc: Location:=NIL): Val RAISES {Error, Exception} =
  VAR newEnv: Env; freeEnv: ObScope.Env; val: Val;
  BEGIN
    IF loc=NIL THEN loc:=phrase.location END;
    TRY
      (* start with copy newEnv of env; its components are extended below. *)
      newEnv := 
          NEW(Env, frameName := env.frameName, forName := env.forName,
              libEnv := env.libEnv,
              scopeEnv:=env.scopeEnv, checkEnv:=env.checkEnv, 
              valueEnv:=env.valueEnv, nextFrame:= env.nextFrame);
      TYPECASE phrase OF
      | ObTree.PhraseCommand(node) =>
          ObCommand.Exec(node.name, node.arg, node.set, env);
      | ObTree.PhraseTerm(node) =>
          freeEnv := NIL;
          ObScope.ScopeTerm(node.term, newEnv.libEnv,
            (*in-out*)newEnv.scopeEnv, (*in-out*)freeEnv);
          ObScope.UnboundIdes(freeEnv);
          ObCheck.CheckTerm(node.term, (*in-out*)newEnv.checkEnv);
          val := ObEval.Term(node.term, (*in-out*)newEnv.valueEnv, NIL, NIL);
      ELSE
        <*ASSERT FALSE*> (*should never happen*)
      END;
      env := newEnv;
      RETURN val;
    EXCEPT
    | ObErr.Fail => 
        SynWr.Flush(SynWr.out); 
        RaiseError("Static Error", loc);
        <*ASSERT FALSE*>
    END;
  END EvalPhrase;

  PROCEDURE NewBool(bool: BOOLEAN): Val =
  BEGIN
    RETURN NEW(ObValue.ValBool, bool:=bool);
  END NewBool;

  PROCEDURE ToBool(val: Val; loc: Location:=NIL): BOOLEAN RAISES {Error} =
  BEGIN
    TYPECASE val OF
    | ObValue.ValBool(node) => RETURN node.bool;
    ELSE
      RaiseError("Obliq.ToBool: not an ValBool", loc);
      <*ASSERT FALSE*>
    END;
  END ToBool;

  PROCEDURE Is(val1, val2: Val): BOOLEAN =
  BEGIN
    RETURN ObValue.Is(val1, val2, SourceLocation("Obliq.Is"));
  END Is;

  PROCEDURE NewInt(int: INTEGER): Val =
  BEGIN
    RETURN NEW(ObValue.ValInt, int:=int, temp:=FALSE);
  END NewInt;

  PROCEDURE ToInt(val: Val; loc: Location:=NIL): INTEGER RAISES {Error} =
  BEGIN
    TYPECASE val OF
    | ObValue.ValInt(node) => RETURN node.int;
    ELSE
      RaiseError("Obliq.ToInt: not a ValInt", loc);
      <*ASSERT FALSE*>
    END;
  END ToInt;

  PROCEDURE NewReal(real: LONGREAL): Val =
  BEGIN
    RETURN NEW(ObValue.ValReal, real:=real, temp:=FALSE);
  END NewReal;

  PROCEDURE ToReal(val: Val; loc: Location:=NIL): LONGREAL RAISES {Error} =
  BEGIN
    TYPECASE val OF
    | ObValue.ValReal(node) => RETURN node.real;
    ELSE
      RaiseError("Obliq.ToReal: not a ValReal", loc);
      <*ASSERT FALSE*>
    END;
  END ToReal;

  PROCEDURE NewChar(char: CHAR): Val =
  BEGIN
    RETURN NEW(ObValue.ValChar, char:=char);
  END NewChar;

  PROCEDURE ToChar(val: Val; loc: Location:=NIL): CHAR RAISES {Error} =
  BEGIN
    TYPECASE val OF
    | ObValue.ValChar(node) => RETURN node.char;
    ELSE
      RaiseError("Obliq.ToChar: not a ValChar", loc);
      <*ASSERT FALSE*>
    END;
  END ToChar;

  PROCEDURE NewText(text: TEXT): Val =
  BEGIN
    IF text=NIL THEN text:="" END;
    RETURN NEW(ObValue.ValText, text:=text);
  END NewText;

  PROCEDURE ToText(val: Val; loc: Location:=NIL): TEXT RAISES {Error} =
  BEGIN
    TYPECASE val OF
    | ObValue.ValText(node) => RETURN node.text;
    ELSE
      RaiseError("Obliq.ToText: not a ValText", loc);
      <*ASSERT FALSE*>
    END;
  END ToText;

  PROCEDURE NewArray(READONLY vals: Vals): Val =
  BEGIN
    RETURN ObValue.NewArray(vals)
  END NewArray;

  PROCEDURE ArraySize(array: Val; loc: Location:=NIL): INTEGER RAISES {Error} =
  BEGIN
    TYPECASE array OF
    | ObValue.ValArray(arr) =>
        TRY RETURN arr.remote.Size();
        EXCEPT 
        | NetObj.Error, Thread.Alerted => 
          RaiseError("on remote array access", loc);
          <*ASSERT FALSE*>
        END;
    ELSE
      RaiseError("Obliq.ArraySize: array expected", loc); 
      <*ASSERT FALSE*>
    END;
  END ArraySize;

  PROCEDURE ArrayGet(array: Val; i: INTEGER; loc: Location:=NIL)
    : Val RAISES {Error} =
  BEGIN
    TYPECASE array OF
    | ObValue.ValArray(arr) =>
        TRY RETURN arr.remote.Get(i);
        EXCEPT 
        | ObValue.ServerError(msg) => RaiseError(msg, loc); <*ASSERT FALSE*>
        | NetObj.Error, Thread.Alerted => 
          RaiseError("on remote array access", loc);
          <*ASSERT FALSE*>
        END;
    ELSE RaiseError("Obliq.ArrayGet: array expected", loc); <*ASSERT FALSE*>
    END;
  END ArrayGet;

  PROCEDURE ArraySet(array: Val; i: INTEGER; val: Val; loc: Location:=NIL) 
    RAISES {Error} =
  BEGIN
    TYPECASE array OF
    | ObValue.ValArray(arr) =>
        TRY arr.remote.Set(i, val);
        EXCEPT
        | ObValue.ServerError(msg) => RaiseError(msg, loc);
        | NetObj.Error, Thread.Alerted => 
          RaiseError("on remote array access", loc);
        END;
    ELSE RaiseError("Obliq.ArraySet: array expected", loc); 
    END;
  END ArraySet;

  PROCEDURE ArraySub(array: Val; start,size: INTEGER; loc: Location:=NIL)
    : Val RAISES {Error} =
  BEGIN
    TYPECASE array OF
    | ObValue.ValArray(arr) =>
        TRY RETURN arr.remote.Sub(start, size);
        EXCEPT 
        | ObValue.ServerError(msg) => RaiseError(msg, loc); <*ASSERT FALSE*>
        | NetObj.Error, Thread.Alerted => 
          RaiseError("on remote array access", loc); <*ASSERT FALSE*>
        END;
    ELSE RaiseError("Obliq.ArraySub: array expected", loc); <*ASSERT FALSE*>
    END;
  END ArraySub;

  PROCEDURE ArrayUpd(array: Val; start, size: INTEGER; sub: Val; 
    loc: Location:=NIL) RAISES {Error} =
  VAR subArr1: REF Vals;
  BEGIN
    TYPECASE array OF
    | ObValue.ValArray(arr) =>
        TYPECASE sub OF
        | ObValue.ValArray(subArr) =>
            TRY subArr1 := subArr.remote.Obtain()
            EXCEPT 
            | NetObj.Error, Thread.Alerted => 
              RaiseError("on remote array access", loc);
            END;
        ELSE RaiseError("Obliq.ArrayUpd: array expected (arg 3)", loc); 
        END;
        TRY arr.remote.Upd(start, size, subArr1);
        EXCEPT 
        | ObValue.ServerError(msg) => RaiseError(msg, loc);
        | NetObj.Error, Thread.Alerted => 
          RaiseError("on remote array access", loc);
        END;
    ELSE RaiseError("Obliq.ArrayUpd: array expected (arg 1)", loc); 
    END;
  END ArrayUpd;

  PROCEDURE ArrayCat(array1,array2: Val; loc: Location:=NIL) 
    : Val RAISES {Error} =
  VAR vals1, vals2: REF ObValue.Vals;
  BEGIN
    TRY
      TYPECASE array1 OF
      | ObValue.ValArray(arr1) => vals1 := arr1.remote.Obtain();
      ELSE
        RaiseError("Obliq.ArrayCat: array expected (arg 1)", loc); 
        <*ASSERT FALSE*>
      END;
      TYPECASE array2 OF
      | ObValue.ValArray(arr2) => vals2 := arr2.remote.Obtain();
      ELSE 
        RaiseError("Obliq.ArrayCat: array expected (arg 2)", loc); 
        <*ASSERT FALSE*>
      END;
      RETURN ObValue.ArrayCat(vals1, vals2);
    EXCEPT 
    | NetObj.Error, Thread.Alerted => 
      RaiseError("on remote array access", loc); <*ASSERT FALSE*>
    END;      
  END ArrayCat;

  PROCEDURE ToArray(val: Val; VAR(*out*) array: Vals; loc: Location:=NIL) 
    RAISES {Error} =
  VAR size: INTEGER; vals: REF Vals;
  BEGIN
    TYPECASE val OF
    | ObValue.ValArray(arr) =>
        TRY vals := arr.remote.Obtain();
        EXCEPT 
        | NetObj.Error, Thread.Alerted => 
          RaiseError("on remote array access", loc);
        END;      
    ELSE RaiseError("Obliq.ToArray: array expected", loc); 
    END;    
    size := NUMBER(vals^);
    IF size # NUMBER(array) THEN
      RaiseError("Obliq.ToArray: array sizes do not match", loc); 
    END;
    FOR i:=0 TO size-1 DO array[i] := vals^[i]; END;
  END ToArray;

  PROCEDURE NewIntArray(READONLY array: ARRAY OF INTEGER): Val =
  VAR vals: REF Vals;
  BEGIN
    vals := NEW(REF Vals, NUMBER(array));
    FOR i:=0 TO NUMBER(array)-1 DO 
      vals[i] := NEW(ObValue.ValInt, int:=array[i], temp:=FALSE);
    END;
    RETURN ObValue.NewArrayFromVals(vals);
  END NewIntArray;

  PROCEDURE ToIntArray(val: Val; VAR(*out*) array: ARRAY OF INTEGER;
    loc: Location:=NIL) RAISES{Error} =
  VAR size: INTEGER; vals: REF Vals;
  BEGIN
    size := ArraySize(val, loc);
    vals := NEW(REF Vals, size);
    ToArray(val, (*out*) vals^, loc);
    FOR i:=0 TO size-1 DO array[i] := ToInt(vals[i], loc) END;
  END ToIntArray;

  PROCEDURE NewRealArray(READONLY array: ARRAY OF LONGREAL): Val =
  VAR vals: REF Vals;
  BEGIN
    vals := NEW(REF Vals, NUMBER(array));
    FOR i:=0 TO NUMBER(array)-1 DO 
      vals[i] := NEW(ObValue.ValReal, real:=array[i], temp:=FALSE);
    END;
    RETURN ObValue.NewArrayFromVals(vals);
  END NewRealArray;

  PROCEDURE ToRealArray(val: Val; VAR(*out*) array: ARRAY OF LONGREAL;
    loc: Location:=NIL) RAISES{Error} =
  VAR size: INTEGER; vals: REF Vals;
  BEGIN
    size := ArraySize(val, loc);
    vals := NEW(REF Vals, size);
    ToArray(val, (*out*) vals^, loc);
    FOR i:=0 TO size-1 DO array[i] := ToReal(vals[i], loc) END;
  END ToRealArray;

  PROCEDURE NewTextArray(READONLY array: ARRAY OF TEXT): Val =
  VAR vals: REF Vals;
  BEGIN
    vals := NEW(REF Vals, NUMBER(array));
    FOR i:=0 TO NUMBER(array)-1 DO 
      vals[i] := ObValue.NewText(array[i]);
    END;
    RETURN ObValue.NewArrayFromVals(vals);
  END NewTextArray;

  PROCEDURE ToTextArray(val: Val; VAR(*out*) array: ARRAY OF TEXT;
    loc: Location:=NIL) RAISES{Error} =
  VAR size: INTEGER; vals: REF Vals;
  BEGIN
    size := ArraySize(val, loc);
    vals := NEW(REF Vals, size);
    ToArray(val, (*out*) vals^, loc);
    FOR i:=0 TO size-1 DO array[i] := ToText(vals[i], loc) END;
  END ToTextArray;

  PROCEDURE NewObject(READONLY fields: Fields): Val =
  BEGIN
    RETURN ObValue.NewObject((*readonly*) fields, "", FALSE, NIL);
  END NewObject;

  PROCEDURE ObjectSelect(object: Val; label: TEXT; loc: Location:=NIL): Val 
    RAISES {Error, Exception} =
    VAR hint:=-1;
  BEGIN
    TRY 
      TYPECASE object OF
      | ObValue.ValObj(obj) =>
          RETURN obj.Select(label, FALSE, (*var*)hint);
      ELSE
        RaiseError("Obliq.ObjectSelect: object expected", loc);
        <*ASSERT FALSE*>
      END;
    EXCEPT 
    | ObValue.ServerError(msg) => RaiseError(msg, loc); <*ASSERT FALSE*>
    | SharedObj.Error =>
        RaiseException(ObValue.sharedException,
          "on remote object selection", loc);
        <*ASSERT FALSE*>
    | NetObj.Error =>
        RaiseException(ObValue.netException,
          "on remote object selection", loc);
        <*ASSERT FALSE*>
    | Thread.Alerted =>
        RaiseException(ObValue.threadAlerted,
          "on remote object selection", loc);
        <*ASSERT FALSE*>
    END;
  END ObjectSelect;

  PROCEDURE ObjectInvoke(object: Val; label: TEXT; READONLY args: Vals;
                         loc: Location:=NIL): Val RAISES {Error, Exception} =
    VAR hint := -1;
  BEGIN
    TRY 
      TYPECASE object OF
      | ObValue.ValObj(obj) =>
        RETURN obj.Invoke(label, NUMBER(args), args, 
                                 FALSE, (*var*) hint);
      ELSE
        RaiseError("Obliq.ObjectInvoke: object expected", loc);
        <*ASSERT FALSE*>
      END;
    EXCEPT 
    | ObValue.ServerError(msg) => RaiseError(msg, loc); <*ASSERT FALSE*>
    | SharedObj.Error =>
        RaiseException(ObValue.sharedException,
          "on remote object invocation", loc);
        <*ASSERT FALSE*>
    | NetObj.Error =>
        RaiseException(ObValue.netException,
          "on remote object invocation", loc);
        <*ASSERT FALSE*>
    | Thread.Alerted =>
        RaiseException(ObValue.threadAlerted,
          "on remote object invocation", loc);
        <*ASSERT FALSE*>
    END;
  END ObjectInvoke;

  PROCEDURE ObjectUpdate(object: Val; label: TEXT; val: Val;
    loc: Location:=NIL) RAISES {Error, Exception} =
    VAR hint := -1;
  BEGIN
      TYPECASE object OF
      | ObValue.ValObj(obj) =>
          TRY 
            obj.Update(label, val, FALSE, (*var*) hint);
          EXCEPT 
          | ObValue.ServerError(msg) =>
              RaiseError(msg, loc);
          | SharedObj.Error =>
            RaiseException(ObValue.sharedException,
                           "on remote object update", loc);
          | NetObj.Error =>
              RaiseException(ObValue.netException,
                "on remote object update", loc);
          | Thread.Alerted =>
              RaiseException(ObValue.threadAlerted,
                "on remote object update", loc);
          END;
      ELSE RaiseError("Obliq.ObjectUpdate: object expected", loc);
      END;
  END ObjectUpdate;

  PROCEDURE ObjectHas(object: Val; label: TEXT; loc: Location:=NIL): BOOLEAN 
    RAISES {Error, Exception} =
  VAR hint: INTEGER;
  BEGIN
      TYPECASE object OF
      | ObValue.ValObj(obj) =>
          TRY 
            RETURN obj.Has(label, (*var*) hint);
          EXCEPT 
          | SharedObj.Error =>
            RaiseException(ObValue.sharedException,
                           "on remote object 'has'", loc);
            <*ASSERT FALSE*>
          | NetObj.Error =>
              RaiseException(ObValue.netException,
                "on remote object 'has'", loc);
              <*ASSERT FALSE*>
          | Thread.Alerted =>
              RaiseException(ObValue.threadAlerted,
                "on remote object 'has'", loc);
              <*ASSERT FALSE*>
          END;
      ELSE
        RaiseError("Obliq.ObjectSelect: object expected", loc);
        <*ASSERT FALSE*>
      END;
  END ObjectHas;

  PROCEDURE ObjectClone1(object: Val; loc: Location:=NIL): Val 
    RAISES {Error, Exception} =
  BEGIN
    TRY 
      TYPECASE object OF
      | ObValue.ValObj(obj) =>
          RETURN ObValue.ObjClone1(obj, NIL);
      ELSE 
        RaiseError("Obliq.ObjectClone1: object expected", loc);
        <*ASSERT FALSE*>
      END;
    EXCEPT 
    | ObValue.ServerError(msg) => RaiseError(msg, loc); <*ASSERT FALSE*>
    | SharedObj.Error =>
        RaiseException(ObValue.sharedException,
          "on remote object cloning", loc);
        <*ASSERT FALSE*>
    | NetObj.Error =>
        RaiseException(ObValue.netException,
          "on remote object cloning", loc);
        <*ASSERT FALSE*>
    | Thread.Alerted =>
        RaiseException(ObValue.threadAlerted,
          "on remote object cloning", loc);
        <*ASSERT FALSE*>
    END;
  END ObjectClone1;

  PROCEDURE ObjectClone(READONLY objects: Vals; loc: Location:=NIL): Val 
    RAISES {Error, Exception} =
  VAR remObjs: REF ARRAY OF ObValue.ValObj;
  BEGIN
    remObjs := NEW(REF ARRAY OF ObValue.ValObj, NUMBER(objects));
    FOR i:=0 TO NUMBER(objects)-1 DO
      TYPECASE objects[i] OF
      | ObValue.ValObj(obj) => remObjs[i] := obj;
      ELSE
        RaiseError("Obliq.ObjectClone: object expected", loc);
        <*ASSERT FALSE*>
      END;
    END;
    TRY 
      RETURN ObValue.ObjClone(remObjs^, NIL); 
    EXCEPT 
    | ObValue.ServerError(msg) => RaiseError(msg, loc); <*ASSERT FALSE*>
    | SharedObj.Error =>
        RaiseException(ObValue.sharedException,
          "on remote object cloning", loc);
        <*ASSERT FALSE*>
    | NetObj.Error =>
        RaiseException(ObValue.netException,
          "on remote object cloning", loc);
        <*ASSERT FALSE*>
    | Thread.Alerted =>
        RaiseException(ObValue.threadAlerted,
          "on remote object cloning", loc);
        <*ASSERT FALSE*>
    END;
  END ObjectClone;

  PROCEDURE NetExport(name, server: TEXT; object: Val; 
    loc: SynLocation.T:=NIL) RAISES {Error, Exception} =
  BEGIN
      TYPECASE object OF
      | ObValue.ValObj(object) =>
          ObBuiltIn.NetExport(name, server, object, loc);
      ELSE RaiseError("Obliq.NetExport: object expected", loc);
      END;
  END NetExport;
  
  PROCEDURE NetImport(name, server: TEXT;
    loc: SynLocation.T:=NIL): Val RAISES {Exception} =
  BEGIN
      RETURN ObBuiltIn.NetImport(name, server, loc);
  END NetImport;

  PROCEDURE NetExportEngine(name, server: TEXT; arg: Val; 
    loc: SynLocation.T:=NIL) RAISES {Exception} =
  BEGIN
    ObBuiltIn.NetExportEngine(name, server, arg, loc);
  END NetExportEngine;
  
  PROCEDURE NetImportEngine(name, server: TEXT;
    loc: SynLocation.T:=NIL): Val RAISES {Exception} =
  BEGIN
      RETURN ObBuiltIn.NetImportEngine(name, server, loc);
  END NetImportEngine;

  PROCEDURE NetWho(object: Val; loc: SynLocation.T:=NIL): TEXT 
    RAISES {Error, Exception} =
  BEGIN
      TYPECASE object OF
      | ObValue.ValObj(object) =>
          RETURN ToText(ObBuiltIn.NetObjectWho(object, loc), loc);
      | ObValue.ValEngine(engine) =>
          RETURN ToText(ObBuiltIn.NetEngineWho(engine.remote, loc), loc);
      ELSE
        RaiseError("Obliq.NetWho: object or engine expected", loc);
        <*ASSERT FALSE*>
      END;
  END NetWho;

  PROCEDURE ReplicaAcquireLock(object: Val; loc:SynLocation.T:=NIL)
    RAISES {Exception} =
    BEGIN
      EVAL ObBuiltIn.ReplicaAcquireLock(object, loc);
    END ReplicaAcquireLock;

  PROCEDURE ReplicaReleaseLock(object: Val; loc: SynLocation.T:=NIL)
    RAISES {Exception} =
    BEGIN
      EVAL ObBuiltIn.ReplicaReleaseLock(object, loc);
    END ReplicaReleaseLock;

  PROCEDURE ReplicaSetNodeName(name: TEXT := NIL; 
                               loc: SynLocation.T:=NIL) : TEXT
    RAISES {Exception, Error} =
    BEGIN
      IF name = NIL THEN name := "" END;
      RETURN ToText(ObBuiltIn.ReplicaSetNodeName(name,loc));
    END ReplicaSetNodeName;

  PROCEDURE ReplicaSetDefaultSequencer(host, name: TEXT; 
                                       loc: SynLocation.T:=NIL): TEXT
    RAISES {Exception, Error} =
    BEGIN
      IF name = NIL THEN name := "" END;
      IF host = NIL THEN host := "" END;
      RETURN ToText(ObBuiltIn.ReplicaSetDefaultSequencer(host,name,loc));
    END ReplicaSetDefaultSequencer;

  PROCEDURE ReplicaNotify(object: Val; notifyObj: Val; 
                          loc: SynLocation.T := NIL): Val
    RAISES {Exception} =
    BEGIN
      RETURN ObBuiltIn.ReplicaNotify(object, notifyObj, loc);
    END ReplicaNotify;

  PROCEDURE ReplicaCancelNotifier(object: Val; loc: SynLocation.T := NIL)
    RAISES {Exception} =
    BEGIN
      ObBuiltIn.ReplicaCancelNotifier(object, loc);
    END ReplicaCancelNotifier;

  PROCEDURE NewVar(val: Val): Val =
  BEGIN
    RETURN ObValue.NewVar(val);
  END NewVar;

  PROCEDURE VarGet(var: Val; loc: Location:=NIL): Val RAISES {Error} =
  BEGIN
    TYPECASE var OF
    | ObValue.ValVar(valVar) =>
        TRY RETURN valVar.remote.Get();
        EXCEPT 
        | NetObj.Error => 
          RaiseError("on remote variable access", loc);
          <*ASSERT FALSE*>
        | Thread.Alerted => 
          RaiseError("on remote variable access", loc);
          <*ASSERT FALSE*>
        END;
    ELSE
      RaiseError("Obliq.VarGet: variable expected", loc); 
      <*ASSERT FALSE*>
    END;
  END VarGet;

  PROCEDURE VarSet(var: Val; val: Val; loc: Location:=NIL) RAISES {Error} =
  BEGIN
    TYPECASE var OF
    | ObValue.ValVar(valVar) =>
        TRY valVar.remote.Set(val);
        EXCEPT 
        | NetObj.Error => RaiseError("on remote variable access", loc);
        | Thread.Alerted => RaiseError("on remote variable access", loc);
        END;
    ELSE RaiseError("Obliq.VarSet: variable expected", loc); 
    END;
  END VarSet;

  PROCEDURE Call(proc: Val; READONLY args: Vals; loc: Location:=NIL): Val 
    RAISES {Error, Exception} =
  BEGIN
      TYPECASE proc OF
      | ObValue.ValFun(clos) =>
        RETURN ObEval.Call(clos, args, loc);
      | ObValue.ValEngine(eng) =>
        IF NUMBER(args)=1 THEN
          RETURN ObEval.CallEngine(eng, args[0], loc);
        ELSE
          RaiseError("Obliq.Call: engine expects 1 argument", loc);
          <*ASSERT FALSE*>
        END;
      ELSE 
        RaiseError("Obliq.Call: procedure or engine expected", loc);
        <*ASSERT FALSE*>
      END;
  END Call;

  PROCEDURE Fork(proc: Val; stackSize: INTEGER; loc: Location:=NIL): Val 
    RAISES {Error} =
  BEGIN
      TYPECASE proc OF
      | ObValue.ValFun(clos) =>
	RETURN ObBuiltIn.ForkThread(clos, stackSize, loc);
      ELSE
        RaiseError("Obliq.Fork: procedure expected", loc);
        <*ASSERT FALSE*>
      END;
  END Fork;

  PROCEDURE Join(thread: Val; loc: Location:=NIL): Val 
    RAISES {Error, Exception} =
  BEGIN
      TYPECASE thread OF
      | ObBuiltIn.ValThread(threadVal) =>
	RETURN ObBuiltIn.JoinThread(threadVal, loc);
      ELSE RaiseError("Obliq.Join: thread expected", loc); <*ASSERT FALSE*>
      END;
  END Join;

  PROCEDURE NewMutex(): Val =
  BEGIN
    RETURN NEW(ObBuiltIn.ValMutex, what:="<a Thread.Mutex>", picklable:=FALSE,
               tag:="Thread`Mutex",
               mutex:=NEW(Thread.Mutex));
  END NewMutex;

  PROCEDURE MutexGet(mutex: Val; loc: Location:=NIL): Thread.Mutex 
    RAISES {Error} =
  BEGIN
    TYPECASE mutex OF
    | ObBuiltIn.ValMutex(valMutex) =>
        RETURN valMutex.mutex;
    ELSE RaiseError("Obliq.MutexGet: mutex expected", loc);  <*ASSERT FALSE*>
    END;
  END MutexGet;

  PROCEDURE NewCondition(): Val =
  BEGIN
    RETURN NEW(ObBuiltIn.ValCondition, what:="<a Thread.Condition>", 
               tag:="Thread`Condition",
	picklable:=FALSE, condition:= NEW(Thread.Condition));
  END NewCondition;

  PROCEDURE ConditionGet(condition: Val; loc: Location:=NIL): Thread.Condition 
    RAISES {Error} =
  BEGIN
    TYPECASE condition OF
    | ObBuiltIn.ValCondition(valCondition) =>
        RETURN valCondition.condition;
    ELSE 
      RaiseError("Obliq.ConditionGet: condition expected", loc); 
      <*ASSERT FALSE*>
    END;
  END ConditionGet;

  PROCEDURE ReportError(swr: SynWr.T; packet: ObValue.ErrorPacket) =
  BEGIN
    ObValue.ErrorMsg(swr, packet);
  END ReportError;

  PROCEDURE ReportException(swr: SynWr.T; packet: ObValue.ExceptionPacket) =
  BEGIN
    ObValue.ExceptionMsg(swr, packet);
  END ReportException;

  PROCEDURE RaiseError(msg: TEXT; loc: Location:=NIL) RAISES {Error} =
  BEGIN
    IF loc=NIL THEN loc:=SourceLocation("Obliq.RaiseError") END;
    ObValue.RaiseError(msg, loc);
  END RaiseError;

  PROCEDURE NewException(name: TEXT): ObValue.ValException =
  BEGIN
    RETURN NEW(ObValue.ValException, name:=name);
  END NewException;

  PROCEDURE RaiseException(exception: ObValue.ValException; msg: TEXT;
    loc: Location:=NIL) RAISES{Exception} =
  BEGIN
    IF loc=NIL THEN loc:=SourceLocation("Obliq.RaiseException") END;
    ObValue.RaiseException(exception, msg, loc); 
  END RaiseException;

  PROCEDURE RaiseSysCallFailure(<*UNUSED*>self: SysCallClosure; 
                                <*UNUSED*>READONLY args: Vals;
    loc: Location:=NIL): Val RAISES{Exception} =
  BEGIN
    IF loc=NIL THEN loc:=SourceLocation("Obliq.RaiseSysCallFailure") END;
    ObValue.RaiseException(sysCallFailure, "Default sys_call procedure", loc);
    <*ASSERT FALSE*>
  END RaiseSysCallFailure;

  PROCEDURE RegisterSysCall(name: TEXT; clos: SysCallClosure) =
  BEGIN
    ObValue.RegisterSysCall(name, clos);
  END RegisterSysCall;

  PROCEDURE SourceLocation(where: TEXT): SynLocation.T =
  BEGIN
    RETURN
      SynLocation.NewLineLocation(
        SynLocation.Info{fileName:=where, line:=0, lineChar:=0, char:=0});
  END SourceLocation;

BEGIN
END Obliq.
