(* Copyright 1991 Digital Equipment Corporation. *)
(* Distributed only by permission. *)

MODULE ObEval;
IMPORT Text, SynLocation, ObTree, ObValue, ObLib, ObBuiltIn, NetObj,
       Thread, SharedObj, RegEx, SynWr;

PROCEDURE Setup () =
  BEGIN
    traceExecution := FALSE;
  END Setup;

PROCEDURE LookupIde (name  : ObTree.IdeName;
                     place : ObTree.IdePlace;
                     lValue: BOOLEAN;
                     env   : ObValue.Env;
                     glob  : ObValue.GlobalEnv;
                     loc   : SynLocation.T      ): ObValue.Val
  RAISES {ObValue.Exception} =
  VAR
    i  : INTEGER;
    val: ObValue.Val;
  BEGIN
    TYPECASE place OF
    | ObTree.IdePlaceGlobal (node) => val := glob^[node.index - 1];
    | ObTree.IdePlaceLocal (node) =>
        i := node.index;
        LOOP
          (* IF i<0 THEN ObErr.Fault("Eval.LookupIde") END; *)
          TYPECASE env OF
            (*
            | NULL =>
                ObErr.Fault("Eval.LookupIde: Unbound var: "
                  & ObTree.FmtIde(name, place, NIL));
            *)
          | ObValue.LocalEnv (node) =>
              IF i = 1 THEN
                (*
                IF NOT ObTree.SameIdeName(name, node.name) THEN
                      ObErr.Fault("Eval.LookupIde");
                END;
                *)
                val := node.val;
                EXIT;
              ELSE
                DEC(i);
                env := node.rest;
              END;
          ELSE                   <*ASSERT FALSE*>
          END;
        END;
    ELSE                         <*ASSERT FALSE*>
    END;
    IF lValue THEN
      RETURN val;
    ELSE
      TYPECASE val OF
      | ObValue.ValVar (node) =>
          TRY
            RETURN node.Get();
          EXCEPT
          | NetObj.Error (atoms) =>
              ObValue.RaiseNetException("on remote access to variable '"
                                          & name.text & "'", atoms, loc);
            <*ASSERT FALSE*>
          | SharedObj.Error (atoms) =>
              ObValue.RaiseSharedException(
                  "on access to replicated variable '" &
                  name.text & "'", atoms, loc);
            <*ASSERT FALSE*>
          | Thread.Alerted =>
              ObValue.RaiseException(
                ObValue.threadAlerted,
                "on remote access to variable '" & name.text & "'", loc);
            <*ASSERT FALSE*>
          END;
      ELSE
        RETURN val;
      END;
    END;
  END LookupIde;

PROCEDURE TermBindingSeq (swr         : SynWr.T; 
                          binding     : ObTree.TermBinding;
                          var         : BOOLEAN;
                          semantics   : ObTree.SharingSemantics;
                          initEnv, env: ObValue.Env;
                          glob        : ObValue.GlobalEnv;
                          mySelf      : ObValue.ValObj;
                          loc         : SynLocation.T): ObValue.Env
  RAISES {ObValue.Error, ObValue.Exception} =
  VAR
    val : ObValue.Val;
    env1: ObValue.Env;
  BEGIN
    TYPECASE binding OF
    | NULL => RETURN env;
    | ObTree.TermBinding (node) =>
        env1 := initEnv;
        val := Term(swr, node.term, (*in-out*) env1, glob, mySelf);
        IF var THEN
          CASE semantics OF
          | ObTree.SharingSemantics.Remote =>
            val := ObValue.NewVar(val);
          | ObTree.SharingSemantics.Replicated =>
            TRY
              val := ObValue.NewReplVar(val);
            EXCEPT SharedObj.Error (atoms) =>
              ObValue.RaiseSharedException(
                  "on replicated var creation", atoms, loc);
            END;
          | ObTree.SharingSemantics.Simple =>
            val := ObValue.NewSimpleVar(val);
          END;
        END;
        RETURN TermBindingSeq(swr, node.rest, var, semantics, initEnv,
                              NEW(ObValue.LocalEnv, name := node.binder,
                                  val := val, rest := env), glob, mySelf,loc);
    END;
  END TermBindingSeq;

PROCEDURE TermBindingRec (swr    : SynWr.T; 
                          binding: ObTree.TermBinding;
                          var    : BOOLEAN;
                          semantics   : ObTree.SharingSemantics;
                          env    : ObValue.LocalEnv;
                          glob   : ObValue.GlobalEnv;
                          mySelf : ObValue.ValObj;
                          loc    : SynLocation.T): ObValue.Env
  RAISES {ObValue.Error, ObValue.Exception} =
  (* Executes definitions backwards, but it's ok since they are all
     functions. *)
  VAR
    val            : ObValue.Val;
    dumFun         : ObValue.ValFun;
    recEnv, recEnv1: ObValue.Env;
  BEGIN
    TYPECASE binding OF
    | NULL => RETURN env;
    | ObTree.TermBinding (node) =>
        dumFun := NEW(ObValue.ValFun, fun := NIL, global := NIL);
        IF var THEN
          CASE semantics OF
          | ObTree.SharingSemantics.Remote =>
            val := ObValue.NewVar(dumFun);
          | ObTree.SharingSemantics.Replicated =>
            TRY
              val := ObValue.NewReplVar(dumFun);
            EXCEPT SharedObj.Error (atoms) =>
              ObValue.RaiseSharedException(
                  "on replicated var creation", atoms, loc);
            END;
          | ObTree.SharingSemantics.Simple =>
            val := ObValue.NewSimpleVar(dumFun);
          END;
        ELSE val := dumFun END;
        recEnv :=
          TermBindingRec(swr, node.rest, var, semantics, 
                         NEW(ObValue.LocalEnv, name := node.binder,
                             val := val, rest := env), glob, mySelf, loc);
        recEnv1 := recEnv;
        TYPECASE Term(swr, node.term, (*in-out*) recEnv1, glob, mySelf) OF
        | ObValue.ValFun (valFun) =>
            dumFun.fun := valFun.fun;
            dumFun.global := valFun.global;
        ELSE
          ObValue.RaiseError(
            "Recursive definition of a non-function", binding.location);
        END;
        RETURN recEnv;
    END;
  END TermBindingRec;

PROCEDURE Term (               swr   : SynWr.T;
                               term  : ObTree.Term;
                VAR (*in-out*) env   : ObValue.Env;
                               glob  : ObValue.GlobalEnv;
                               mySelf: ObValue.ValObj     ): ObValue.Val
  RAISES {ObValue.Error, ObValue.Exception} =
  TYPE Vals = REF ARRAY OF ObValue.Val;
  VAR result: ObValue.Val;
  BEGIN
    IF interrupt THEN
      interrupt := FALSE;
      ObValue.RaiseError("Interrupt", term.location);
    END;
    IF traceExecution THEN
      SynLocation.PrintLocation(swr, term.location);
      SynWr.NewLine(swr, loud:=TRUE);
(*
      SynWr.Text(swr, ": ", loud:=TRUE);
      ObPrintTree.PrintTerm(swr, term, NIL, NIL, 1);
*)
    END;
    TYPECASE term OF
      (* | NULL => ObErr.Fault("Eval.Term NIL"); *)
    | ObTree.TermIde (node) =>
        result :=
          LookupIde(node.name, node.place, FALSE, env, glob, term.location);
    | ObTree.TermOk => result := ObValue.valOk;
    | ObTree.TermBool (node) =>
        IF node.cache = NIL THEN
          node.cache := NEW(ObValue.ValBool, bool := node.bool);
        END;
        result := node.cache;
    | ObTree.TermChar (node) =>
        IF node.cache = NIL THEN
          node.cache := NEW(ObValue.ValChar, char := node.char);
        END;
        result := node.cache;
    | ObTree.TermText (node) =>
        IF node.cache = NIL THEN
          node.cache := ObValue.NewText(node.text);
        END;
        result := node.cache;
    | ObTree.TermInt (node) =>
        IF node.cache = NIL THEN
          node.cache :=
            NEW(ObValue.ValInt, int := node.int, temp := FALSE);
        END;
        result := node.cache;
    | ObTree.TermReal (node) =>
        IF node.cache = NIL THEN
          node.cache :=
            NEW(ObValue.ValReal, real := node.real, temp := FALSE);
        END;
        result := node.cache;
    | ObTree.TermOption (node) =>
        VAR
          env1: ObValue.Env;
          text: TEXT;
        BEGIN
          env1 := env;
          TYPECASE Term(swr, node.tag, (*in-out*) env1, glob, mySelf) OF
          | ObValue.ValText (txt) => text := txt.text;
          ELSE
            ObValue.RaiseError("Option tag must be a text", term.location);
          END;
          env1 := env;
          result := NEW(ObValue.ValOption, tag := text,
                        val := Term(swr, node.term, (*in-out*) env1, glob,
                                    mySelf));
        END;
    | ObTree.TermAlias (node) =>
        VAR
          env1: ObValue.Env;
          val : ObValue.Val;
        BEGIN
          env1 := env;
          val := Term(swr, node.term, (*in-out*) env1, glob, mySelf);
          TYPECASE val OF
          | ObValue.ValObj (obj) =>
              result :=
                ObValue.NewAlias(obj, node.label.text, term.location);
          ELSE
            ObValue.RaiseError(
              "Aliasing must operate on an object", term.location);
          END;
        END;
    | ObTree.TermArray (node) =>
        VAR
          vals                 := NEW(Vals, node.elemsNo);
          argList              := node.elems;
          env1   : ObValue.Env;
        BEGIN
          FOR i := 0 TO node.elemsNo - 1 DO
            env1 := env;
            vals[i] :=
              Term(swr, argList.first, (*in-out*) env1, glob, mySelf);
            argList := argList.rest;
          END;
          CASE node.semantics OF
          | ObTree.SharingSemantics.Remote =>
              result := ObValue.NewArrayFromVals(vals);
          | ObTree.SharingSemantics.Replicated =>
              TRY
                result := ObValue.NewReplArrayFromVals(vals);
              EXCEPT
                SharedObj.Error (atoms) =>
                  ObValue.RaiseSharedException(
                    "on replicated array creation", atoms, term.location);
              END;
          | ObTree.SharingSemantics.Simple =>
              result := ObValue.NewSimpleArrayFromVals(vals);
          END;
        END;
    | ObTree.TermOp (node) =>
        VAR
          argList                    := node.args;
          opCode                     := NARROW(node.opCode, ObLib.OpCode);
          argArray: ObValue.ArgArray;
          env1    : ObValue.Env;
          msg     : TEXT;
        BEGIN
          IF (opCode.arity >= -1) AND (node.argsNo # opCode.arity) THEN
            IF opCode.arity = -1 THEN
              msg := "Not expecting an argument list for procedure: "
                       & node.pkg.text & "_" & node.op.text;
            ELSIF node.argsNo = -1 THEN
              msg := "Expecting an argument list for procedure: "
                       & node.pkg.text & "_" & node.op.text;
            ELSE
              msg := ObValue.BadArgsNoMsg(
                       opCode.arity, node.argsNo, "procedure",
                       node.pkg.text & "_" & node.op.text);
            END;
            ObValue.RaiseError(msg, term.location);
          END;
          IF node.argsNo > NUMBER(argArray) THEN
            ObValue.RaiseError("Too many arguments", term.location);
          END;
          FOR i := 1 TO node.argsNo DO
            env1 := env;
            argArray[i] :=
              Term(swr, argList.first, (*in-out*) env1, glob, mySelf);
            argList := argList.rest;
          END;
          result := NARROW(node.package, ObLib.T).Eval(
                      opCode, node.argsNo, argArray, node.temp, swr,
                      term.location);
        END;
    | ObTree.TermFun (node) =>
        VAR
          newGlob := NEW(ObValue.GlobalEnv, node.globalsNo);
          globals := node.globals;
        BEGIN
          FOR i := 0 TO node.globalsNo - 1 DO
            newGlob^[i] := LookupIde(globals.name, globals.place, TRUE,
                                     env, glob, term.location);
            globals := globals.rest;
          END;
          result := NEW(ObValue.ValFun, fun := node, global := newGlob);
        END;
    | ObTree.TermMeth (node) =>
        VAR
          newGlob := NEW(ObValue.GlobalEnv, node.globalsNo);
          globals := node.globals;
        BEGIN
          FOR i := 0 TO node.globalsNo - 1 DO
            newGlob^[i] := LookupIde(globals.name, globals.place, TRUE,
                                     env, glob, term.location);
            globals := globals.rest;
          END;
          result := NEW(ObValue.ValMeth, meth := node, global := newGlob);
        END;
    | ObTree.TermAppl (node) =>
        VAR
          env1, newEnv: ObValue.Env;
          newGlob     : ObValue.GlobalEnv;
          binderList  : ObTree.IdeList;
          argList     : ObTree.TermList;
          val         : ObValue.Val;
        BEGIN
          env1 := env;
          TYPECASE Term(swr, node.fun, (*in-out*) env1, glob, mySelf) OF
          | ObValue.ValFun (clos) =>
              IF node.argsNo # clos.fun.bindersNo THEN
                ObValue.RaiseError(ObValue.BadArgsNoMsg(
                                     clos.fun.bindersNo, node.argsNo, "",
                                     ""), term.location);
              END;
              newGlob := clos.global;
              newEnv := NIL;
              binderList := clos.fun.binders;
              argList := node.args;
              FOR i := 1 TO node.argsNo DO
                env1 := env;
                newEnv :=
                  NEW(ObValue.LocalEnv, name := binderList.first,
                      val := Term(swr, argList.first, (*in-out*) env1,
                                  glob, mySelf), rest := newEnv);
                binderList := binderList.rest;
                argList := argList.rest;
              END;
              result := Term(swr, clos.fun.body, (*in-out*) newEnv,
                             newGlob, mySelf);
          | ObValue.ValEngine (engine) =>
              IF node.argsNo # 1 THEN
                ObValue.RaiseError(
                  ObValue.BadArgsNoMsg(1, node.argsNo, "", ""),
                  term.location);
              END;
              env1 := env;
              val :=
                Term(swr, node.args.first, (*in-out*) env1, glob, mySelf);
              TRY
                result := engine.remote.Eval(val, mySelf);
              EXCEPT
              | ObValue.ServerError (msg) =>
                  ObValue.RaiseError(msg, term.location);
              | NetObj.Error (atoms) =>
                  ObValue.RaiseNetException(
                    "on remote engine execution", atoms, term.location);
              | Thread.Alerted =>
                  ObValue.RaiseException(
                    ObValue.threadAlerted, "on remote engine execution",
                    term.location);
              END;
          ELSE
            ObValue.RaiseError(
              "Application of a non-procedure", term.location);
          END;
        END;
    | ObTree.TermObj (node) =>
        VAR
          sync: ObValue.Sync;
          fields    := NEW(REF ObValue.ObjFields, node.fieldsNo);
          fieldList := node.fields;
          env1: ObValue.Env;
        BEGIN
          CASE node.sync OF
          | ObTree.Sync.None => sync := NIL;
          | ObTree.Sync.Monitored =>
              sync := NEW(ObValue.Sync, mutex := NEW(Thread.Mutex));
          ELSE                   <*ASSERT FALSE*>
          END;
          FOR i := 0 TO node.fieldsNo - 1 DO
            env1 := env;
            fields^[i].label := fieldList.label.text;
            fields^[i].field :=
              Term(swr, fieldList.term, (*in-out*) env1, glob, mySelf);
            fieldList := fieldList.rest;
          END;

          CASE node.semantics OF
          | ObTree.SharingSemantics.Remote =>
              result := ObValue.NewObjectFromFields(
                          fields, "", node.protected, sync);
          | ObTree.SharingSemantics.Replicated =>
              IF sync # NIL THEN
                ObValue.RaiseError(
                  "serialized implied by replicated", term.location);
              END;
              TRY
                result := ObValue.NewReplObjectFromFields(
                            fields, "", node.protected);
              EXCEPT
              | ObValue.ServerError (msg) =>
                  ObValue.RaiseError(msg, term.location);
              | SharedObj.Error (atoms) =>
                  ObValue.RaiseSharedException(
                    "on replicated object creation", atoms, term.location);
              END;
          | ObTree.SharingSemantics.Simple =>
              result := ObValue.NewSimpleObjectFromFields(
                          fields, "", node.protected, sync);
          END;
        END;
    | ObTree.TermClone (node) =>
        VAR
          env1   : ObValue.Env;
          objs   : ObTree.TermList;
          valObjs: REF ARRAY OF ObValue.ValObj;
        BEGIN
          TRY
            IF node.objsNo = 1 THEN
              env1 := env;
              TYPECASE Term(swr, node.objs.first,
                            (*in-out*) env1, glob, mySelf) OF
              | ObValue.ValObj (obj) =>
                  result := ObValue.ObjClone1(obj, mySelf);
              ELSE
                ObValue.RaiseError(
                  "Arguments of clone must be objects", term.location);
              END;
            ELSE
              objs := node.objs;
              valObjs := NEW(REF ARRAY OF ObValue.ValObj, node.objsNo);
              FOR i := 0 TO node.objsNo - 1 DO
                env1 := env;
                TYPECASE
                    Term(swr, objs.first, (*in-out*) env1, glob, mySelf) OF
                | ObValue.ValObj (obj) => valObjs^[i] := obj;
                ELSE
                  ObValue.RaiseError(
                    "Arguments of clone must be objects", term.location);
                END;
                objs := objs.rest;
              END;
              result := ObValue.ObjClone( (*readonly*)valObjs^, mySelf);
            END;
          EXCEPT
          | ObValue.ServerError (msg) =>
              ObValue.RaiseError(msg, term.location);
          | SharedObj.Error (atoms) =>
              ObValue.RaiseSharedException(
                "on replicated object cloning", atoms, term.location);
          | NetObj.Error (atoms) =>
              ObValue.RaiseNetException(
                "on remote object cloning", atoms, term.location);
          | Thread.Alerted =>
              ObValue.RaiseException(
                ObValue.threadAlerted, "on remote object cloning",
                term.location);
          END;
        END;
    | ObTree.TermNotify (node) =>
        VAR
          env1: ObValue.Env;
          val1: ObValue.Val;
        BEGIN
          env1 := env;
          TYPECASE Term(swr, node.withObj, (*in-out*) env1, glob, mySelf)
              OF
          | ObValue.ValFun (fun) =>
              env1 := env;
              val1 := Term(swr, node.obj, (*in-out*) env1, glob, mySelf);
              TYPECASE val1 OF
              | ObValue.ValObj, ObValue.ValVar, ObValue.ValArray,
                  ObValue.ValEngine, ObValue.ValFileSystem =>
                  ObValue.ObjNotify(val1, fun, swr);
              ELSE
                ObValue.RaiseError(
                  "First argument of notify must be a remote data object",
                  term.location);
              END;
          ELSE
            ObValue.RaiseError(
              "Second argument of notify must be a procedure",
              term.location);
          END;
          result := ObValue.valOk;
        END;
    | ObTree.TermPickler (node) =>
        VAR env1: ObValue.Env;
        BEGIN
          env1 := env;
          TYPECASE Term(swr, node.pklIn, (*in-out*) env1, glob, mySelf) OF
          | ObValue.ValSimpleObj (in) =>
              env1 := env;
              TYPECASE
                  Term(swr, node.pklOut, (*in-out*) env1, glob, mySelf) OF
              | ObValue.ValSimpleObj (out) =>
                  env1 := env;
                  TYPECASE
                      Term(swr, node.obj, (*in-out*) env1, glob, mySelf) OF
                  | ObValue.ValObj (valobj) =>
                      TRY
                        ObValue.SetObjPickler(valobj, in, out, mySelf);
                      EXCEPT
                      | ObValue.ServerError (msg) =>
                          ObValue.RaiseError(msg, term.location);
                      | SharedObj.Error (atoms) =>
                          ObValue.RaiseSharedException(
                            "while setting pickler", atoms, term.location);
                      | NetObj.Error (atoms) =>
                          ObValue.RaiseNetException(
                            "while setting pickler", atoms, term.location);
                      | Thread.Alerted =>
                          ObValue.RaiseException(
                            ObValue.threadAlerted, "while setting pickler",
                            term.location);
                      END;
                  ELSE
                    ObValue.RaiseError(
                      "First argument of registerPickler must be an object",
                      term.location);
                  END;
              ELSE
                ObValue.RaiseError(
                  "Second argument of registerPickler must be a simple object",
                  term.location);
              END;
          ELSE
            ObValue.RaiseError(
              "Third argument of registerPickler must be a simple object",
              term.location);
          END;
          result := ObValue.valOk;
        END;
    | ObTree.TermReplicate (node) =>
        VAR
          env1  : ObValue.Env;
          array1: Vals;
          arr   : REF ARRAY OF TEXT;
        BEGIN
          env1 := env;
          TYPECASE
              Term(swr, node.args.first, (*in-out*) env1, glob, mySelf) OF
          | ObValue.ValObj (obj) =>
              IF node.argsNo # 2 THEN
                ObValue.RaiseError(
                    ObValue.BadArgsNoMsg(2, node.argsNo, "", ""), 
                    term.location);
              END;
              env1 := env;
              TYPECASE Term(swr, node.args.rest.first,
                            (*in-out*) env1, glob, mySelf) OF
              | ObValue.ValArray (arrayObj) =>
                  TRY
                    array1 := arrayObj.Obtain();
                    arr := NEW(REF ARRAY OF TEXT, NUMBER(array1^));
                    FOR i := 0 TO NUMBER(array1^) - 1 DO
                      TYPECASE array1^[i] OF
                      | ObValue.ValText (txt) => arr[i] := txt.text;
                      ELSE
                        ObValue.RaiseError(
                          "second argument must be array of text",
                          term.location);
                      END;
                    END;
                    result := ObValue.ToReplObj(obj, mySelf, arr^);
                  EXCEPT
                  | ObValue.ServerError (msg) =>
                      ObValue.RaiseError(msg, term.location);
                  | SharedObj.Error (atoms) =>
                      ObValue.RaiseSharedException(
                        "on conversion to replicated object", atoms,
                        term.location);
                  | NetObj.Error (atoms) =>
                      ObValue.RaiseNetException(
                        "on conversion to replicated object", atoms,
                        term.location);
                  | Thread.Alerted =>
                      ObValue.RaiseException(
                        ObValue.threadAlerted,
                        "on conversion to replicated object", term.location);
                  END;
              ELSE
                ObValue.RaiseError(
                  "second argument must be array of text", term.location);
              END;
          | ObValue.ValArray (arr) =>
              IF node.argsNo # 1 THEN
                ObValue.RaiseError(
                    ObValue.BadArgsNoMsg(1, node.argsNo, "", ""), 
                    term.location);
              END;
              TRY
                array1 := arr.Obtain();
                result := ObValue.NewReplArray(array1^);
              EXCEPT
              | SharedObj.Error (atoms) =>
                  ObValue.RaiseSharedException(
                    "on conversion to replicated array", atoms,
                    term.location);
              | NetObj.Error (atoms) =>
                  ObValue.RaiseNetException(
                    "on conversion to replicated array", atoms,
                    term.location);
              | Thread.Alerted =>
                  ObValue.RaiseException(
                    ObValue.threadAlerted,
                    "on conversion to replicated array", term.location);
              END;
          ELSE
            ObValue.RaiseError(
              "Redirection must operate on an object or array", term.location);
          END;
        END;
    | ObTree.TermRemote (node) =>
        VAR env1: ObValue.Env;
            array1: Vals;
        BEGIN
          env1 := env;
          TYPECASE Term(swr, node.obj, (*in-out*) env1, glob, mySelf) OF
          | ObValue.ValObj (obj) =>
              TRY
                result := ObValue.ToRemObj(obj, mySelf);
              EXCEPT
              | ObValue.ServerError (msg) =>
                  ObValue.RaiseError(msg, term.location);
              | SharedObj.Error (atoms) =>
                  ObValue.RaiseSharedException(
                    "on conversion to remote object", atoms, term.location);
              | NetObj.Error (atoms) =>
                  ObValue.RaiseNetException(
                    "on conversion to remote object", atoms, term.location);
              | Thread.Alerted =>
                  ObValue.RaiseException(
                    ObValue.threadAlerted,
                    "on conversion to remote object", term.location);
              END;
          | ObValue.ValArray (arr) =>
              TRY
                array1 := arr.Obtain();
                result := ObValue.NewArray(array1^);
              EXCEPT
              | SharedObj.Error (atoms) =>
                  ObValue.RaiseSharedException(
                    "on conversion to remote array", atoms, term.location);
              | NetObj.Error (atoms) =>
                  ObValue.RaiseNetException(
                    "on conversion to remote array", atoms, term.location);
              | Thread.Alerted =>
                  ObValue.RaiseException(
                    ObValue.threadAlerted, "on conversion to remote array",
                    term.location);
              END;
          ELSE
            ObValue.RaiseError(
              "remote must operate on an object or array", term.location);
          END;
        END;
    | ObTree.TermSimple (node) =>
        VAR env1: ObValue.Env;
            array1: Vals;
        BEGIN
          env1 := env;
          TYPECASE Term(swr, node.obj, (*in-out*) env1, glob, mySelf) OF
          | ObValue.ValObj (obj) =>
              TRY
                result := ObValue.ToSimpleObj(obj, mySelf);
              EXCEPT
              | ObValue.ServerError (msg) =>
                  ObValue.RaiseError(msg, term.location);
              | SharedObj.Error (atoms) =>
                  ObValue.RaiseSharedException(
                    "on conversion to simple object", atoms, term.location);
              | NetObj.Error (atoms) =>
                  ObValue.RaiseNetException(
                    "on conversion to simple object", atoms, term.location);
              | Thread.Alerted =>
                  ObValue.RaiseException(
                    ObValue.threadAlerted,
                    "on conversion to simple object", term.location);
              END;
          | ObValue.ValArray (arr) =>
              TRY
                array1 := arr.Obtain();
                result := ObValue.NewSimpleArray(array1^);
              EXCEPT
              | SharedObj.Error (atoms) =>
                  ObValue.RaiseSharedException(
                    "on conversion to simple array", atoms, term.location);
              | NetObj.Error (atoms) =>
                  ObValue.RaiseNetException(
                    "on conversion to simple array", atoms, term.location);
              | Thread.Alerted =>
                  ObValue.RaiseException(
                    ObValue.threadAlerted, "on conversion to simple array",
                    term.location);
              END;
          ELSE
            ObValue.RaiseError(
              "simple must operate on an object or array", term.location);
          END;
        END;
    | ObTree.TermRedirect (node) =>
        VAR
          env1 : ObValue.Env;
          toObj: ObValue.Val;
        BEGIN
          env1 := env;
          TYPECASE Term(swr, node.obj, (*in-out*) env1, glob, mySelf) OF
          | ObValue.ValObj (obj) =>
              env1 := env;
              toObj :=
                Term(swr, node.toObj, (*in-out*) env1, glob, mySelf);
              TRY
                obj.Redirect(toObj, ObValue.Is(obj, mySelf, term.location));
              EXCEPT
              | ObValue.ServerError (msg) =>
                  ObValue.RaiseError(msg, term.location);
              | SharedObj.Error (atoms) =>
                  ObValue.RaiseSharedException(
                    "on replicated object invocation", atoms, term.location);
              | NetObj.Error (atoms) =>
                  ObValue.RaiseNetException(
                    "on remote object invocation", atoms, term.location);
              | Thread.Alerted =>
                  ObValue.RaiseException(
                    ObValue.threadAlerted, "on remote object invocation",
                    term.location);
              END;
              result := ObValue.valOk;
          ELSE
            ObValue.RaiseError(
              "Redirection must operate on an object", term.location);
          END;
        END;
    | ObTree.TermSelect (node) =>
        VAR
          env1    : ObValue.Env;
          argList : ObTree.TermList;
          argArray: ObValue.ArgArray;
        BEGIN
          IF node.argsNo > NUMBER(argArray) THEN
            ObValue.RaiseError("Too many arguments.", term.location);
          END;
          env1 := env;
          TYPECASE Term(swr, node.obj, (*in-out*) env1, glob, mySelf) OF
          | ObValue.ValObj (obj) =>
              argList := node.args;
              FOR i := 1 TO node.argsNo DO
                env1 := env;
                argArray[i] :=
                  Term(swr, argList.first, (*in-out*) env1, glob, mySelf);
                argList := argList.rest;
              END;
              TRY
                IF node.invoke THEN
                  FOR i := node.argsNo + 1 TO NUMBER(argArray) DO
                    argArray[i] := NIL; (* Clear for transmission *)
                  END;
                  result :=
                    obj.Invoke(swr, node.label.text, node.argsNo, argArray,
                               ObValue.Is(obj, mySelf, term.location),
                               (*var*) node.labelIndexHint);
                ELSE
                  result :=
                    obj.Select(swr, node.label.text,
                               ObValue.Is(obj, mySelf, term.location),
                               (*var*) node.labelIndexHint);
                END;
              EXCEPT
              | ObValue.ServerError (msg) =>
                  ObValue.RaiseError(msg, term.location);
              | SharedObj.Error (atoms) =>
                  ObValue.RaiseSharedException(
                    "on replicated object invocation", atoms, term.location);
              | NetObj.Error (atoms) =>
                  ObValue.RaiseNetException(
                    "on remote object invocation", atoms, term.location);
              | Thread.Alerted =>
                  ObValue.RaiseException(
                    ObValue.threadAlerted, "on remote object invocation",
                    term.location);
              END;
          ELSE
            ObValue.RaiseError(
              "Selection must operate on an object", term.location);
          END;
        END;
    | ObTree.TermUpdate (node) =>
        VAR
          env1: ObValue.Env;
          val : ObValue.Val;
        BEGIN
          env1 := env;
          TYPECASE Term(swr, node.obj, (*in-out*) env1, glob, mySelf) OF
          | ObValue.ValObj (obj) =>
              env1 := env;
              val := Term(swr, node.term, (*in-out*) env1, glob, mySelf);
              TRY
                obj.Update(node.label.text, val,
                           ObValue.Is(obj, mySelf, term.location),
                           (*var*) node.labelIndexHint);
              EXCEPT
              | ObValue.ServerError (msg) =>
                  ObValue.RaiseError(msg, term.location);
              | SharedObj.Error (atoms) =>
                  ObValue.RaiseSharedException(
                    "on replicated object update", atoms, term.location);
              | NetObj.Error (atoms) =>
                  ObValue.RaiseNetException(
                    "on remote object update", atoms, term.location);
              | Thread.Alerted =>
                  ObValue.RaiseException(
                    ObValue.threadAlerted, "on remote object update",
                    term.location);
              END;
              result := ObValue.valOk;
          ELSE
            ObValue.RaiseError(
              "Update must operate on an object", term.location);
          END;
        END;
    | ObTree.TermSeq =>
        VAR
          term1 := term;
          env1  := env;
        BEGIN
          LOOP
            TYPECASE term1 OF
            | ObTree.TermSeq (seq) =>
                EVAL Term(swr, seq.before, (*in-out*) env1, glob, mySelf);
                term1 := seq.after;
            ELSE
              result := Term(swr, term1, (*in-out*) env1, glob, mySelf);
              EXIT;
            END;
          END;
        END;
    | ObTree.TermLet (node) =>
        IF node.rec THEN
          env :=
            TermBindingRec(swr, node.binding, node.var, node.semantics,
                           env, glob, mySelf, term.location);
        ELSE
          env :=
            TermBindingSeq(swr, node.binding, node.var, node.semantics,
                           env, env, glob, mySelf, term.location);
        END;
        result := ObValue.valOk;
    | ObTree.TermAssign (node) =>
        VAR
          env1: ObValue.Env;
          val : ObValue.Val;
        BEGIN
          TYPECASE LookupIde(
                     node.name, node.place, TRUE, env, glob, term.location)
              OF
          | ObValue.ValVar (var) =>
              env1 := env;
              val := Term(swr, node.val, (*in-out*) env1, glob, mySelf);
              TRY
                var.Set(val);
              EXCEPT
              | NetObj.Error (atoms) =>
                  ObValue.RaiseNetException(
                    "on remote assigment to variable '" & node.name.text
                      & "'", atoms, term.location);
              | SharedObj.Error (atoms) =>
                  ObValue.RaiseSharedException(
                    "on assignment to replicated variable '"
                      & node.name.text & "'", atoms, term.location);
              | Thread.Alerted =>
                  ObValue.RaiseException(
                    ObValue.threadAlerted,
                    "on remote assigment to variable '" & node.name.text
                      & "'", term.location);
              END;
          ELSE
            ObValue.RaiseError(
              "Assigment must operate on a variable", term.location);
          END;
          result := ObValue.valOk;
        END;
    | ObTree.TermIf (node) =>
        VAR env1: ObValue.Env;
        BEGIN
          env1 := env;
          TYPECASE Term(swr, node.test, (*in-out*) env1, glob, mySelf) OF
          | ObValue.ValBool (bool) =>
              IF bool.bool THEN
                env1 := env;
                result :=
                  Term(swr, node.ifTrue, (*in-out*) env1, glob, mySelf);
              ELSIF node.ifFalse = NIL THEN
                result := ObValue.valOk;
              ELSE
                env1 := env;
                result :=
                  Term(swr, node.ifFalse, (*in-out*) env1, glob, mySelf);
              END;
          ELSE
            ObValue.RaiseError(
              "Conditional test must be a boolean", term.location);
          END;
        END;
    | ObTree.TermCase (node) =>
        VAR
          env1    : ObValue.Env;
          caseList: ObTree.TermCaseList;
          mem     : REF RegEx.Memory    := NIL;
          matchNo : INTEGER;
          compiled: RegEx.Pattern; 
          vals: ARRAY [0 .. NUMBER(RegEx.Memory) - 1] OF ObValue.Val;
        BEGIN
          env1 := env;
          TYPECASE Term(swr, node.option, (*in-out*) env1, glob, mySelf) OF
          | ObValue.ValOption (option) =>
              caseList := node.caseList;
              LOOP
                IF caseList = NIL THEN
                  ObValue.RaiseError("No case branch applies to tag: "
                                       & option.tag, term.location);
                END;
                IF caseList.pattern = NIL THEN (* "else" case *)
                  env1 := env;
                  result := Term(swr, caseList.body, (*in-out*) env1, glob,
                                 mySelf);
                  EXIT;
                END;
                IF caseList.compiled = NIL THEN
                  TYPECASE Term(swr, caseList.pattern, (*in-out*) env1, glob, 
                                mySelf) OF
                  | ObValue.ValText (txt) =>
                    TRY
                      compiled := RegEx.Compile(txt.text);
                    EXCEPT
                      RegEx.Error (txt2) =>
                      ObValue.RaiseError("Case branch regular expression '"&
                        txt.text & "' error:" & txt2, term.location)
                    END;
                  ELSE
                    ObValue.RaiseError("Non-text case branch", term.location);
                  END;

                  (* if the case branch is actually a constant, we can
                     store the compiled pattern for the future *)
                  TYPECASE caseList.pattern OF
                  | ObTree.TermText => caseList.compiled := compiled;
                  ELSE END;
                ELSE
                  compiled := caseList.compiled;
                END;
                IF caseList.binderMatch # NIL THEN
                  mem := NEW(REF RegEx.Memory);
                END;
                IF RegEx.Execute(compiled, option.tag, mem := mem)
                     > -1 THEN
                  IF caseList.binder = NIL THEN
                    env1 := env;
                  ELSE
                    env1 := NEW(ObValue.LocalEnv, name := caseList.binder,
                                val := option.val, rest := env);
                    IF caseList.binderMatch # NIL THEN
                      matchNo := 0;
                      FOR i := FIRST(mem^) TO LAST(mem^) DO
                        IF mem[i].start > -1 THEN
                          vals[matchNo] :=
                            ObValue.NewText(
                              Text.Sub(option.tag, mem[i].start,
                                       mem[i].stop - mem[i].start));
                          INC(matchNo);
                        END;
                      END;
                      env1 :=
                        NEW(ObValue.LocalEnv, name := caseList.binderMatch,
                            val := ObValue.NewSimpleArray(
                                     SUBARRAY(vals, 0, matchNo)),
                            rest := env1);
                    END;
                  END;
                  result := Term(swr, caseList.body, (*in-out*) env1, glob,
                                 mySelf);
                  EXIT;
                END;
                caseList := caseList.rest;
              END;
          ELSE
            ObValue.RaiseError(
              "Case over a non-option value", term.location);
          END;
        END;
    | ObTree.TermLoop (node) =>
        VAR env1: ObValue.Env;
        BEGIN
          TRY
            LOOP
              env1 := env;
              EVAL Term(swr, node.loop, (*in-out*) env1, glob, mySelf);
            END;
          EXCEPT
          | ObValue.Error (pkt) =>
              IF NOT Text.Equal(pkt.msg, "exit") THEN
                RAISE ObValue.Error(pkt);
              END;
          END;
          result := ObValue.valOk;
        END;
    | ObTree.TermExit (node) =>
        RAISE ObValue.Error(NEW(ObValue.ErrorPacket, msg := "exit",
                                location := node.location));
    | ObTree.TermFor (node) =>
        VAR
          env1        : ObValue.Env;
          forEnv      : ObValue.LocalEnv;
          lbVal, ubVal: ObValue.Val;
          i, ub       : INTEGER;
        BEGIN
          env1 := env;
          lbVal := Term(swr, node.lb, (*in-out*) env1, glob, mySelf);
          TYPECASE lbVal OF
          | ObValue.ValInt (node) => i := node.int;
          ELSE
            ObValue.RaiseError(
              "Lower bound of 'for' must be an integer", term.location);
          END;
          env1 := env;
          ubVal := Term(swr, node.ub, (*in-out*) env1, glob, mySelf);
          TYPECASE ubVal OF
          | ObValue.ValInt (node) => ub := node.int;
          ELSE
            ObValue.RaiseError(
              "Upper bound of 'for' must be an integer", term.location);
          END;
          forEnv := NEW(ObValue.LocalEnv, name := node.binder, val := NIL,
                        rest := env);
          TRY
            LOOP
              IF i > ub THEN EXIT END;
              forEnv.val := NEW(ObValue.ValInt, int := i, temp := FALSE);
              env1 := forEnv;
              EVAL Term(swr, node.body, (*in-out*) env1, glob, mySelf);
              INC(i);
            END;
          EXCEPT
          | ObValue.Error (pkt) =>
              IF NOT Text.Equal(pkt.msg, "exit") THEN
                RAISE ObValue.Error(pkt);
              END;
          END;
          result := ObValue.valOk;
        END;
    | ObTree.TermForeach (node) =>
        VAR
          env1                 : ObValue.Env;
          forEnv               : ObValue.LocalEnv;
          val, rangeVal        : ObValue.Val;
          vals, oldVals, array1: Vals;
          i, ub                : INTEGER;
        BEGIN
          env1 := env;
          rangeVal := Term(swr, node.range, (*in-out*) env1, glob, mySelf);
          TYPECASE rangeVal OF
          | ObValue.ValArray (node) =>
              TRY
                array1 := node.Obtain();
              EXCEPT
              | NetObj.Error (atoms) =>
                  ObValue.RaiseNetException(
                    "on remote array access", atoms, term.location);
              | SharedObj.Error (atoms) =>
                  ObValue.RaiseSharedException(
                    "on replicated array access", atoms, term.location);
              | Thread.Alerted =>
                  ObValue.RaiseException(
                    ObValue.threadAlerted, "on remote array access",
                    term.location);
              END;
          ELSE
            ObValue.RaiseError(
              "Range of 'for' must be an array", term.location);
          END;
          i := 0;
          forEnv := NEW(ObValue.LocalEnv, name := node.binder, val := NIL,
                        rest := env);
          TRY
            ub := NUMBER(array1^);
            IF node.map THEN vals := NEW(Vals, ub); END;
            LOOP
              IF i >= ub THEN EXIT END;
              forEnv.val := array1^[i];
              env1 := forEnv;
              val := Term(swr, node.body, (*in-out*) env1, glob, mySelf);
              IF node.map THEN vals^[i] := val END;
              INC(i);
            END;
          EXCEPT
          | ObValue.Error (pkt) =>
              IF NOT Text.Equal(pkt.msg, "exit") THEN
                RAISE ObValue.Error(pkt);
              ELSIF node.map THEN
                oldVals := vals;
                vals := NEW(Vals, i);
                vals^ := SUBARRAY(oldVals^, 0, i);
              END;
          END;
          IF node.map THEN
            result := ObValue.NewArrayFromVals(vals);
          ELSE
            result := ObValue.valOk;
          END;
        END;
    | ObTree.TermException (node) =>
        VAR env1: ObValue.Env;
        BEGIN
          env1 := env;
          TYPECASE Term(swr, node.name, (*in-out*) env1, glob, mySelf) OF
          | ObValue.ValText (str) =>
              result := NEW(ObValue.ValException, name := str.text);
          ELSE
            ObValue.RaiseError(
              "Argument of exception must be a text", term.location);
          END;
        END;
    | ObTree.TermRaise (node) =>
        VAR env1: ObValue.Env;
        BEGIN
          env1 := env;
          TYPECASE Term(swr, node.exception, (*in-out*) env1, glob, mySelf)
              OF
          | ObValue.ValException (exc) =>
              ObValue.RaiseException(exc, "", node.location);
          ELSE
            ObValue.RaiseError(
              "Argument of raise must be an exception", term.location);
          END;
        END;
    | ObTree.TermTry (node) =>
        VAR
          env1   : ObValue.Env;
          tryList: ObTree.TermTryList;
        BEGIN
          TRY
            env1 := env;
            result := Term(swr, node.body, (*in-out*) env1, glob, mySelf);
          EXCEPT
          | ObValue.Exception (packet) =>
              tryList := node.tryList;
              LOOP
                IF tryList = NIL THEN RAISE ObValue.Exception(packet) END;
                IF tryList.exception = NIL THEN (* "else" case *)
                  env1 := env;
                  result := Term(swr, tryList.recover, (*in-out*) env1,
                                 glob, mySelf);
                  EXIT;
                END;
                env1 := env;
                TYPECASE Term(swr, tryList.exception, (*in-out*) env1,
                              glob, mySelf) OF
                | ObValue.ValException (exc) =>
                    IF ObValue.SameException(exc, packet.exception) THEN
                      env1 := env;
                      result := Term(swr, tryList.recover, (*in-out*) env1,
                                     glob, mySelf);
                      EXIT;
                    END;
                    tryList := tryList.rest;
                ELSE
                  ObValue.RaiseError(
                    "Guard of try must be an exception", term.location);
                END;
              END;
          | ObValue.Error (packet) =>
              tryList := node.tryList;
              LOOP
                IF tryList = NIL THEN RAISE ObValue.Error(packet); END;
                IF tryList.exception = NIL THEN (* "else" case *)
                  env1 := env;
                  result := Term(swr, tryList.recover, (*in-out*) env1,
                                 glob, mySelf);
                  EXIT;
                END;
                tryList := tryList.rest;
              END;
          END;
        END;
    | ObTree.TermTryFinally (node) =>
        VAR env1: ObValue.Env;
        BEGIN
          TRY
            env1 := env;
            result := Term(swr, node.body, (*in-out*) env1, glob, mySelf);
          FINALLY
            env1 := env;
            result :=
              Term(swr, node.finally, (*in-out*) env1, glob, mySelf);
          END;
        END;
    | ObTree.TermWatch (node) =>
        VAR
          env1  : ObValue.Env;
          mySync: ObValue.Sync := NIL;
        BEGIN
          TYPECASE mySelf OF
          | ObValue.ValRemObj (remObj) =>
              IF remObj = NIL THEN
                ObValue.RaiseError(
                    "watch-until must be used inside a method",
                    term.location);
              END;
              TYPECASE remObj.remote OF
              | ObValue.RemObjServer (remObjServer) =>
                  mySync := remObjServer.sync;
              ELSE
                ObValue.RaiseError(
                  "watch-until does not work on remote objects",
                  term.location);
              END;

              IF mySync = NIL THEN
                ObValue.RaiseError(
                  "watch-until must be used inside a serialized object",
                  term.location);
              END;
          | ObValue.ValSimpleObj (simpleObj) =>
              (* Simple objs are always local! *)
              IF simpleObj = NIL THEN
                ObValue.RaiseError(
                  "watch-until must be used inside a method", term.location);
              END;
              mySync := simpleObj.simple.sync;

              IF mySync = NIL THEN
                ObValue.RaiseError(
                  "watch-until must be used inside a serialized object",
                  term.location);
              END;
          | ObValue.ValReplObj (replObj) =>
              IF replObj = NIL THEN
                ObValue.RaiseError(
                  "watch-until must be used inside a method", term.location);
              END;
          ELSE
            ObValue.RaiseError(
              "watch-until does not work on non-local remote objects",
              term.location);
          END;
          env1 := env;
          TYPECASE Term(swr, node.condition, (*in-out*) env1, glob, mySelf)
              OF
          | ObBuiltIn.ValCondition (cond) =>
                LOOP
                  env1 := env;
                  TYPECASE
                      Term(swr, node.guard, (*in-out*) env1, glob, mySelf)
                      OF
                  | ObValue.ValBool (guard) =>
                      IF guard.bool THEN
                        EXIT
                      ELSE
                        TYPECASE mySelf OF
                        | ObValue.ValRemObj, ObValue.ValSimpleObj =>
                          Thread.Wait(mySync.mutex, cond.condition);
                        | ObValue.ValReplObj (replObj) =>
                          SharedObj.Wait(replObj.replica, cond.condition);
                        ELSE
                          <*ASSERT FALSE*>  (*can't get here*)
                        END;
                      END;
                  ELSE
                    ObValue.RaiseError(
                      "Argument 2 of watch-until must be a boolean",
                      term.location);
                  END;
                END;
                result := ObValue.valOk;
          ELSE
            ObValue.RaiseError(
              "Argument 1 of watch-until must be a condition",
              term.location);
          END;
        END;
    ELSE                         <*ASSERT FALSE*>
    END;
    RETURN result;
  END Term;

PROCEDURE Call (         clos: ObValue.ValFun;
                READONLY args: ObValue.Vals;
                         swr : SynWr.T;
                         loc : SynLocation.T    := NIL): ObValue.Val
  RAISES {ObValue.Error, ObValue.Exception} =
  VAR
    env    : ObValue.Env;
    binders: ObTree.IdeList;
  BEGIN
    IF clos.fun.bindersNo # NUMBER(args) THEN
      ObValue.RaiseError(ObValue.BadArgsNoMsg(
                           clos.fun.bindersNo, NUMBER(args), "", ""), loc);
    END;
    env := NIL;
    binders := clos.fun.binders;
    FOR i := 0 TO NUMBER(args) - 1 DO
      env := NEW(ObValue.LocalEnv, name := binders.first, val := args[i],
                 rest := env);
      binders := binders.rest;
    END;
    RETURN Term(swr, clos.fun.body, (*in-out*) env, clos.global, NIL);
  END Call;

PROCEDURE CallEngine (engine: ObValue.ValEngine;
                      arg   : ObValue.Val;
                      loc   : SynLocation.T       := NIL): ObValue.Val
  RAISES {ObValue.Error, ObValue.Exception} =
  BEGIN
    TRY
      RETURN engine.remote.Eval(arg, NIL);
    EXCEPT
    | ObValue.ServerError (msg) =>
        ObValue.RaiseError(msg, loc); <*ASSERT FALSE*>
    | NetObj.Error (atoms) =>
        ObValue.RaiseNetException("on remote engine execution", atoms, loc); <*ASSERT FALSE*>
    | Thread.Alerted =>
        ObValue.RaiseException(
          ObValue.threadAlerted, "on remote engine execution", loc); <*ASSERT FALSE*>
    END;
  END CallEngine;

BEGIN
END ObEval.
