(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

MODULE ObEval;
IMPORT Text, SynLocation, ObTree, ObValue, ObLib, ObBuiltIn, 
NetObj, Thread;

  PROCEDURE Setup() =
    BEGIN 
    END Setup;

  PROCEDURE LookupIde(name: ObTree.IdeName; place: ObTree.IdePlace; 
    lValue: BOOLEAN; env: ObValue.Env; glob: ObValue.GlobalEnv; 
    loc: SynLocation.T) : ObValue.Val RAISES {ObValue.Exception} =
  VAR i: INTEGER; val: ObValue.Val;
  BEGIN
    TYPECASE place OF
    | ObTree.IdePlaceGlobal(node) => 
      val := glob^[node.index-1];
    | ObTree.IdePlaceLocal(node) =>
      i := node.index;
      LOOP
	    (* IF i<0 THEN ObErr.Fault("Eval.LookupIde") END; *)
	    TYPECASE env OF
            (*
	    | NULL => 
	        ObErr.Fault("Eval.LookupIde: Unbound var: " 
	          & ObTree.FmtIde(name, place, NIL));
            *)
	    | ObValue.LocalEnv(node) =>
	        IF i=1 THEN
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
        ELSE <*ASSERT FALSE*>
	    END;
      END;
    ELSE <*ASSERT FALSE*>
    END;
    IF lValue THEN RETURN val;
    ELSE
      TYPECASE val OF
      | ObValue.ValVar(node) => 
        TRY RETURN node.remote.Get();
        EXCEPT
        | NetObj.Error(atoms) =>
          ObValue.RaiseNetException(
            "on remote access to variable '" & name.text & "'", atoms, loc);
          <*ASSERT FALSE*>
        | Thread.Alerted =>
          ObValue.RaiseException(ObValue.threadAlerted,
            "on remote access to variable '" & name.text & "'", loc);
          <*ASSERT FALSE*>
        END;
      ELSE RETURN val;
      END;
    END;
  END LookupIde;

  PROCEDURE TermBindingSeq(binding: ObTree.TermBinding; var: BOOLEAN; 
      initEnv, env: ObValue.Env; glob: ObValue.GlobalEnv;
      mySelf: ObValue.RemObj): ObValue.Env 
      RAISES {ObValue.Error, ObValue.Exception} =
    VAR val: ObValue.Val; env1: ObValue.Env;
    BEGIN
      TYPECASE binding OF
      | NULL => RETURN env;
      | ObTree.TermBinding(node) =>
          env1 := initEnv;
	  val:=Term(node.term, (*in-out*)env1, glob, mySelf);
	  IF var THEN val := ObValue.NewVar(val)  END;
	  RETURN 
	    TermBindingSeq(node.rest, var, initEnv,
	      NEW(ObValue.LocalEnv, name:=node.binder, val:=val, rest:=env),
	      glob, mySelf);
      END;
    END TermBindingSeq;

  PROCEDURE TermBindingRec(binding: ObTree.TermBinding; var: BOOLEAN; 
    env: ObValue.LocalEnv; glob: ObValue.GlobalEnv;
      mySelf: ObValue.RemObj): ObValue.Env 
      RAISES {ObValue.Error, ObValue.Exception} =
  (* Executes definitions backwards, but it's ok since they are all 
     functions. *)
    VAR val: ObValue.Val; dumFun: ObValue.ValFun; recEnv,recEnv1: ObValue.Env;
    BEGIN
      TYPECASE binding OF
      | NULL => RETURN env;
      | ObTree.TermBinding(node) =>
	  dumFun:=NEW(ObValue.ValFun, fun:=NIL, global:=NIL);
	  IF var
	  THEN val := ObValue.NewVar(dumFun);
          ELSE val:=dumFun;
	  END;
	  recEnv :=
	    TermBindingRec(node.rest, var,
	      NEW(ObValue.LocalEnv, name:=node.binder, val:=val, rest:=env),
              glob, mySelf);
          recEnv1 := recEnv;
          TYPECASE Term(node.term, (*in-out*)recEnv1, glob, mySelf) OF
          | ObValue.ValFun(valFun) =>
            dumFun.fun := valFun.fun;
            dumFun.global := valFun.global;
          ELSE ObValue.RaiseError("Recursive definition of a non-function", 
                 binding.location);
          END;
          RETURN recEnv;
      END;
    END TermBindingRec;

  PROCEDURE Term(term: ObTree.Term; 
      VAR (*in-out*)env: ObValue.Env; glob: ObValue.GlobalEnv;
      mySelf: ObValue.RemObj): ObValue.Val 
      RAISES {ObValue.Error, ObValue.Exception} =
    TYPE Vals = REF ARRAY OF ObValue.Val;  
    VAR  result: ObValue.Val;
    BEGIN
      IF interrupt THEN
        interrupt := FALSE;
        ObValue.RaiseError("Interrupt", term.location);
      END;
      TYPECASE term OF
      (* | NULL => ObErr.Fault("Eval.Term NIL"); *)
      | ObTree.TermIde(node) =>
	  result := 
            LookupIde(node.name, node.place, FALSE, env, glob, term.location);
      | ObTree.TermOk =>
	  result := ObValue.valOk;
      | ObTree.TermBool(node) =>
	  IF node.cache=NIL THEN
	    node.cache := NEW(ObValue.ValBool, bool:=node.bool);
	  END;
	  result := node.cache;
      | ObTree.TermChar(node) =>
	  IF node.cache=NIL THEN
	    node.cache := NEW(ObValue.ValChar, char:=node.char);
	  END;
	  result := node.cache;
      | ObTree.TermText(node) =>
	  IF node.cache=NIL THEN
	    node.cache := ObValue.NewText(node.text);
	  END;
	  result := node.cache;
      | ObTree.TermInt(node) =>
	  IF node.cache=NIL THEN
	    node.cache := NEW(ObValue.ValInt, int:=node.int, temp:=FALSE);
	  END;
	  result := node.cache;
      | ObTree.TermReal(node) =>
	  IF node.cache=NIL THEN
	    node.cache := NEW(ObValue.ValReal, real:=node.real, temp:=FALSE);
	  END;
	  result := node.cache;
      | ObTree.TermOption(node) =>
          VAR
            env1: ObValue.Env;
          BEGIN
            env1 := env;
	    result := 
              NEW(ObValue.ValOption, 
                tag:=node.tag.text,
                val:=Term(node.term, (*in-out*)env1, glob, mySelf));
          END;
      | ObTree.TermAlias(node) =>
          VAR
            env1: ObValue.Env; val: ObValue.Val;
          BEGIN
            env1 := env;
            val := Term(node.term, (*in-out*)env1, glob, mySelf);
            TYPECASE val OF
            | ObValue.ValObj(obj) =>
              result := ObValue.NewAlias(obj, node.label.text, term.location);
            ELSE ObValue.RaiseError("Aliasing must operate on an object",
                    term.location);
            END;
          END;
      | ObTree.TermArray(node) =>
          VAR 
            vals := NEW(Vals, node.elemsNo);
            argList := node.elems;
            env1: ObValue.Env;
          BEGIN
            FOR i := 0 TO node.elemsNo-1 DO
              env1 := env;
              vals[i]:=Term(argList.first, (*in-out*)env1, glob, mySelf);
              argList := argList.rest;
            END;
            result := ObValue.NewArrayFromVals(vals);
          END;
      | ObTree.TermOp(node) =>
        VAR
          argList := node.args;
          opCode := NARROW(node.opCode, ObLib.OpCode);
          argArray: ObValue.ArgArray;
          env1: ObValue.Env;
          msg: TEXT;
        BEGIN
          IF (opCode.arity >= -1) AND (node.argsNo # opCode.arity) THEN
            IF opCode.arity = -1 THEN
              msg := "Not expecting an argument list for procedure: " &
                         node.pkg.text & "_" & node.op.text;
            ELSIF node.argsNo = -1 THEN
              msg := "Expecting an argument list for procedure: " &
                         node.pkg.text & "_" & node.op.text;
            ELSE
              msg := ObValue.BadArgsNoMsg(opCode.arity, node.argsNo,
                         "procedure", node.pkg.text & "_" & node.op.text);
            END;
            ObValue.RaiseError(msg, term.location);
          END;
          IF node.argsNo > NUMBER(argArray) THEN 
            ObValue.RaiseError("Too many arguments", term.location); 
          END;
          FOR i:=1 TO node.argsNo DO
            env1 := env;
            argArray[i]:=Term(argList.first, (*in-out*)env1, glob, mySelf);
            argList := argList.rest;
          END;
          result :=
            NARROW(node.package, ObLib.T)
              .Eval(opCode, node.argsNo, argArray, node.temp, term.location);
        END;
      | ObTree.TermFun(node) =>
          VAR
            newGlob := NEW(ObValue.GlobalEnv, node.globalsNo);
            globals := node.globals;
          BEGIN
            FOR i:=0 TO node.globalsNo-1 DO
              newGlob^[i] := 
                LookupIde(globals.name, globals.place, TRUE, env, glob, 
                  term.location);
              globals := globals.rest;
            END;
	    result := NEW(ObValue.ValFun, fun:=node, global:=newGlob);
          END;
      | ObTree.TermMeth(node) =>
          VAR
            newGlob := NEW(ObValue.GlobalEnv, node.globalsNo);
            globals := node.globals;
          BEGIN
            FOR i:=0 TO node.globalsNo-1 DO
              newGlob^[i] := 
                LookupIde(globals.name, globals.place, TRUE, env, glob,
                  term.location);
              globals := globals.rest;
            END;
	    result := NEW(ObValue.ValMeth, meth:=node, global:=newGlob);
          END;
      | ObTree.TermAppl(node) =>
        VAR
          env1, newEnv: ObValue.Env;
          newGlob: ObValue.GlobalEnv;
          binderList: ObTree.IdeList;
          argList: ObTree.TermList;
          val: ObValue.Val;
        BEGIN
          env1 := env;
          TYPECASE Term(node.fun, (*in-out*)env1, glob, mySelf) OF
          | ObValue.ValFun(clos) =>
              IF node.argsNo # clos.fun.bindersNo THEN
                ObValue.RaiseError(ObValue.BadArgsNoMsg(clos.fun.bindersNo,
                  node.argsNo, "", ""), term.location);
              END;
              newGlob := clos.global;
              newEnv := NIL;
              binderList := clos.fun.binders;
              argList := node.args;
              FOR i:=1 TO node.argsNo DO
                env1 := env;
                newEnv :=
	          NEW(ObValue.LocalEnv, 
	            name:=binderList.first, 
	            val:=Term(argList.first, (*in-out*)env1, glob, mySelf), 
	            rest:=newEnv);
	        binderList := binderList.rest;
                argList := argList.rest;
              END;         
	      result := Term(clos.fun.body, (*in-out*)newEnv, newGlob, mySelf);
          | ObValue.ValEngine(engine) =>
              IF node.argsNo # 1 THEN
                ObValue.RaiseError(ObValue.BadArgsNoMsg(1,
                  node.argsNo, "", ""), term.location);
              END;
              env1 := env;
	      val:=Term(node.args.first, (*in-out*)env1, glob, mySelf);
	      TRY result := engine.remote.Eval(val, mySelf);
              EXCEPT 
              | ObValue.ServerError(msg) =>
                  ObValue.RaiseError(msg, term.location);
              | NetObj.Error(atoms) =>
                  ObValue.RaiseNetException(
                    "on remote engine execution", atoms, term.location);
              | Thread.Alerted =>
                  ObValue.RaiseException(ObValue.threadAlerted,
                    "on remote engine execution", term.location);
              END;
          ELSE ObValue.RaiseError("Application of a non-procedure", 
                  term.location);
          END;
        END;
      | ObTree.TermObj(node) =>
        VAR
          sync: ObValue.Sync;
          fields := NEW(REF ObValue.ObjFields, node.fieldsNo);
          fieldList := node.fields;
          env1: ObValue.Env;
        BEGIN
          CASE node.sync OF
          | ObTree.Sync.None => 
            sync:=NIL;
          | ObTree.Sync.Monitored => 
            sync := NEW(ObValue.Sync, mutex := NEW(Thread.Mutex));
          ELSE <*ASSERT FALSE*>
          END;
          FOR i:=0 TO node.fieldsNo-1 DO
            env1:=env;
            fields^[i].label := fieldList.label.text;
            fields^[i].field:=
              Term(fieldList.term, (*in-out*)env1, glob, mySelf);
            fieldList := fieldList.rest;
          END;
          result := ObValue.NewObjectFromFields(fields, "",
              node.protected, sync);
        END;
      | ObTree.TermClone(node) =>
        VAR
          env1: ObValue.Env;
          objs: ObTree.TermList;
          remObjs: REF ARRAY OF ObValue.RemObj;
        BEGIN
          TRY 
            IF node.objsNo=1 THEN
              env1 := env;
              TYPECASE Term(node.objs.first, (*in-out*)env1, glob, mySelf) OF
              | ObValue.ValObj(obj) =>
                  result := ObValue.ObjClone1(obj.remote, mySelf);
              ELSE ObValue.RaiseError("Arguments of clone must be objects",
                     term.location);
              END;
            ELSE
              objs := node.objs;
              remObjs := NEW(REF ARRAY OF ObValue.RemObj, node.objsNo);
              FOR i:=0 TO node.objsNo-1 DO
                env1 := env;
                TYPECASE Term(objs.first, (*in-out*)env1, glob, mySelf) OF
                | ObValue.ValObj(obj) =>
                    remObjs^[i] := obj.remote;
                ELSE ObValue.RaiseError("Arguments of clone must be objects",
                       term.location);
                END;
                objs := objs.rest;
              END;
              result := ObValue.ObjClone((*readonly*) remObjs^, mySelf);
            END;
          EXCEPT 
          | ObValue.ServerError(msg) =>
              ObValue.RaiseError(msg, term.location);
          | NetObj.Error(atoms) =>
              ObValue.RaiseNetException(
                "on remote object cloning", atoms, term.location);
          | Thread.Alerted =>
              ObValue.RaiseException(ObValue.threadAlerted,
                "on remote object cloning", term.location);
          END;
        END;
      | ObTree.TermRedirect(node) =>
        VAR
          env1: ObValue.Env;
          toObj: ObValue.Val;
        BEGIN
          env1 := env;
          TYPECASE Term(node.obj, (*in-out*)env1, glob, mySelf) OF
          | ObValue.ValObj(obj) =>
              env1 := env;
              toObj:=Term(node.toObj, (*in-out*)env1, glob, mySelf);
              TRY 
                obj.remote.Redirect(toObj, obj.remote=mySelf);
              EXCEPT 
              | ObValue.ServerError(msg) =>
                  ObValue.RaiseError(msg, term.location);
              | NetObj.Error(atoms) =>
                  ObValue.RaiseNetException(
                      "on remote object invocation", atoms, term.location);
              | Thread.Alerted =>
                  ObValue.RaiseException(ObValue.threadAlerted,
                      "on remote object invocation", term.location);
              END;
              result := ObValue.valOk;
          ELSE ObValue.RaiseError("Redirection must operate on an object",
                  term.location);
          END;
        END;
      | ObTree.TermSelect(node) =>
        VAR
          env1: ObValue.Env;
          argList: ObTree.TermList;
          argArray: ObValue.ArgArray;
        BEGIN
          IF node.argsNo > NUMBER(argArray) THEN 
            ObValue.RaiseError("Too many arguments.", term.location); 
          END;
          env1 := env;
          TYPECASE Term(node.obj, (*in-out*)env1, glob, mySelf) OF
          | ObValue.ValObj(obj) =>
              argList := node.args;
              FOR i:=1 TO node.argsNo DO
                env1 := env;
                argArray[i]:=Term(argList.first, (*in-out*)env1, glob, mySelf);
                argList := argList.rest;
              END;
              TRY 
                IF node.invoke THEN
                  FOR i:=node.argsNo+1 TO NUMBER(argArray) DO
                    argArray[i] := NIL; (* Clear for transmission *)
                  END;
                  result := obj.remote.Invoke(node.label.text, 
                    node.argsNo, argArray, obj.remote=mySelf,
                      (*var*) node.labelIndexHint);
                ELSE
                  result := 
                    obj.remote.Select(node.label.text, obj.remote=mySelf,
                       (*var*) node.labelIndexHint);
                END;
              EXCEPT 
              | ObValue.ServerError(msg) =>
                  ObValue.RaiseError(msg, term.location);
              | NetObj.Error(atoms) =>
                  ObValue.RaiseNetException(
                      "on remote object invocation", atoms, term.location);
              | Thread.Alerted =>
                  ObValue.RaiseException(ObValue.threadAlerted,
                      "on remote object invocation", term.location);
              END;
          ELSE ObValue.RaiseError("Selection must operate on an object",
                  term.location);
          END;
        END;
      | ObTree.TermUpdate(node) =>
        VAR
          env1: ObValue.Env;
          val: ObValue.Val;
        BEGIN
          env1 := env;
          TYPECASE Term(node.obj, (*in-out*)env1, glob, mySelf) OF
          | ObValue.ValObj(obj) =>
              env1 := env;
              val := Term(node.term, (*in-out*)env1, glob, mySelf);
              TRY 
                obj.remote.Update(node.label.text, val, obj.remote=mySelf,
                  (*var*) node.labelIndexHint);
              EXCEPT
              | ObValue.ServerError(msg) =>
                  ObValue.RaiseError(msg, term.location);
              | NetObj.Error(atoms) =>
                  ObValue.RaiseNetException(
                      "on remote object update", atoms, term.location);
              | Thread.Alerted =>
                  ObValue.RaiseException(ObValue.threadAlerted,
                      "on remote object update", term.location);
              END;
              result := ObValue.valOk;
          ELSE ObValue.RaiseError("Update must operate on an object", 
                 term.location);
          END;
        END;
      | ObTree.TermSeq =>
        VAR
          term1 := term;
          env1 := env;
        BEGIN
          LOOP
            TYPECASE term1 OF
            | ObTree.TermSeq(seq) =>
              EVAL Term(seq.before, (*in-out*) env1, glob, mySelf);
              term1 := seq.after;
            ELSE 
              result := Term(term1, (*in-out*) env1, glob, mySelf);
              EXIT;
            END;
          END;
        END;
      | ObTree.TermLet(node) =>
          IF node.rec THEN
            env := 
              TermBindingRec(node.binding, node.var, env, glob, mySelf);
          ELSE
            env := 
              TermBindingSeq(node.binding, node.var, env, env, glob, mySelf);
          END;
          result := ObValue.valOk;
      | ObTree.TermAssign(node) =>
        VAR
          env1: ObValue.Env;
          val: ObValue.Val;
        BEGIN
          TYPECASE LookupIde(node.name, node.place, TRUE, env, glob, 
                             term.location) OF
          | ObValue.ValVar(var) =>
            env1 := env;
            val := Term(node.val, (*in-out*)env1, glob, mySelf);
            TRY var.remote.Set(val);
            EXCEPT
            | NetObj.Error(atoms) =>
              ObValue.RaiseNetException(
                  "on remote assigment to variable '" & node.name.text & "'",
                  atoms, term.location);
            | Thread.Alerted =>
              ObValue.RaiseException(ObValue.threadAlerted,
                  "on remote assigment to variable '" & node.name.text & "'",
                  term.location);
            END;
          ELSE ObValue.RaiseError("Assigment must operate on a variable", 
                 term.location);
          END;
          result := ObValue.valOk;
        END;
      | ObTree.TermIf(node) =>
        VAR
          env1: ObValue.Env;
        BEGIN
          env1 := env;
          TYPECASE Term(node.test, (*in-out*)env1, glob, mySelf) OF
          | ObValue.ValBool(bool) => 
              IF bool.bool THEN
                env1 := env;
                result := Term(node.ifTrue, (*in-out*)env1, glob, mySelf);
              ELSIF node.ifFalse=NIL THEN
                result := ObValue.valOk;
              ELSE
                env1 := env;
                result := Term(node.ifFalse, (*in-out*)env1, glob, mySelf);
              END;
          ELSE ObValue.RaiseError("Conditional test must be a boolean", 
                 term.location);
          END;
        END;
      | ObTree.TermCase(node) =>
        VAR
          env1: ObValue.Env;
          caseList: ObTree.TermCaseList;
        BEGIN
          env1 := env;
          TYPECASE Term(node.option, (*in-out*)env1, glob, mySelf) OF
          | ObValue.ValOption(option) =>
              caseList := node.caseList;
              LOOP
                IF caseList = NIL THEN 
                  ObValue.RaiseError("No case branch applies to tag: " &
                    option.tag, term.location);
                END;
                IF caseList.tag = NIL THEN (* "else" case *)
                  env1 := env;
                  result := Term(caseList.body, (*in-out*)env1, glob, mySelf);
                  EXIT;
                END;
                IF Text.Equal(option.tag, caseList.tag.text) THEN
                  IF caseList.binder = NIL THEN
                    env1 := env;
                  ELSE
                    env1 := NEW(ObValue.LocalEnv, name:=caseList.binder, 
	              val:=option.val, rest:=env);
                  END;
                  result := Term(caseList.body, (*in-out*)env1, glob, mySelf);
                  EXIT;
                END;
                caseList := caseList.rest;
             END;
          ELSE 
            ObValue.RaiseError("Case over a non-option value", term.location);
          END;
        END;
      | ObTree.TermLoop(node) =>
        VAR
          env1: ObValue.Env;
        BEGIN
          TRY
            LOOP 
              env1 := env;
              EVAL Term(node.loop, (*in-out*)env1, glob, mySelf);
            END;
          EXCEPT
          | ObValue.Error(pkt) =>
              IF NOT Text.Equal(pkt.msg, "exit") THEN
                RAISE ObValue.Error(pkt);
              END;
          END;
          result := ObValue.valOk;
        END;
      | ObTree.TermExit(node) =>
          RAISE 
            ObValue.Error(
              NEW(ObValue.ErrorPacket, 
                  msg:="exit", location:=node.location));
      | ObTree.TermFor(node) =>
        VAR
          env1: ObValue.Env;
          forEnv: ObValue.LocalEnv;
          lbVal, ubVal: ObValue.Val;
          i, ub: INTEGER;
        BEGIN
          env1 := env;
          lbVal := Term(node.lb, (*in-out*)env1, glob, mySelf);
          TYPECASE lbVal OF | ObValue.ValInt(node) => i:=node.int;
          ELSE ObValue.RaiseError("Lower bound of 'for' must be an integer", 
                 term.location);
          END;
          env1 := env;
          ubVal := Term(node.ub, (*in-out*)env1, glob, mySelf);
          TYPECASE ubVal OF | ObValue.ValInt(node) => ub:=node.int;
          ELSE ObValue.RaiseError("Upper bound of 'for' must be an integer", 
                 term.location);
          END;
          forEnv := 
            NEW(ObValue.LocalEnv, name:=node.binder, val:=NIL, rest:=env);
          TRY
            LOOP
              IF i>ub THEN EXIT END;
              forEnv.val := NEW(ObValue.ValInt, int:=i, temp:=FALSE);
              env1 := forEnv;
              EVAL Term(node.body, (*in-out*)env1, glob, mySelf);
              INC(i);
            END;
          EXCEPT
          | ObValue.Error(pkt) =>
              IF NOT Text.Equal(pkt.msg, "exit") THEN
                RAISE ObValue.Error(pkt);
              END;
          END;
          result := ObValue.valOk;
        END;
      | ObTree.TermForeach(node) =>
        VAR
          env1: ObValue.Env;
          forEnv: ObValue.LocalEnv;
          val, rangeVal: ObValue.Val;
          vals, oldVals, array1: Vals;
          i, ub: INTEGER;
        BEGIN
          env1 := env;
          rangeVal := Term(node.range, (*in-out*)env1, glob, mySelf);
          TYPECASE rangeVal OF 
          | ObValue.ValArray(node) => 
            TRY array1:=node.remote.Obtain();
            EXCEPT
            | NetObj.Error(atoms) =>
              ObValue.RaiseNetException(
                  "on remote array access", atoms, term.location);
            | Thread.Alerted =>
              ObValue.RaiseException(ObValue.threadAlerted,
                  "on remote array access", term.location);
            END;
          ELSE ObValue.RaiseError("Range of 'for' must be an array", 
                 term.location);
          END;
          i := 0;
          forEnv := 
            NEW(ObValue.LocalEnv, name:=node.binder, val:=NIL, rest:=env);
          TRY
            ub := NUMBER(array1^);
            IF node.map THEN
              vals := NEW(Vals, ub);
            END;
            LOOP
              IF i>=ub THEN EXIT END;
              forEnv.val := array1^[i];
              env1 := forEnv;
              val := Term(node.body, (*in-out*)env1, glob, mySelf);
              IF node.map THEN vals^[i] := val END;
              INC(i);
            END;
          EXCEPT
          | ObValue.Error(pkt) =>
              IF NOT Text.Equal(pkt.msg, "exit") THEN
                RAISE ObValue.Error(pkt);
              ELSIF node.map THEN
                oldVals := vals;
                vals:=NEW(Vals, i);
                vals^ := SUBARRAY(oldVals^,0,i);
              END;
          END;
          IF node.map THEN
            result := ObValue.NewArrayFromVals(vals);
          ELSE
            result := ObValue.valOk;
          END;
        END;
      | ObTree.TermException(node) =>
        VAR
          env1: ObValue.Env;
        BEGIN
          env1 := env;
          TYPECASE Term(node.name, (*in-out*)env1, glob, mySelf) OF
          | ObValue.ValText(str) =>
            result := NEW(ObValue.ValException, name:=str.text);
          ELSE ObValue.RaiseError("Argument of exception must be a text", 
                 term.location);
          END;
        END;
      | ObTree.TermRaise(node) =>
        VAR
          env1: ObValue.Env;
        BEGIN
          env1 := env;
          TYPECASE Term(node.exception, (*in-out*)env1, glob, mySelf) OF
          | ObValue.ValException(exc) => 
              ObValue.RaiseException(exc, "", node.location);
          ELSE ObValue.RaiseError("Argument of raise must be an exception", 
                 term.location);
          END;
        END;
      | ObTree.TermTry(node) =>
        VAR
          env1: ObValue.Env;
          tryList: ObTree.TermTryList;
        BEGIN
          TRY 
            env1 := env;
            result := Term(node.body, (*in-out*)env1, glob, mySelf);
          EXCEPT
          | ObValue.Exception(packet) =>
             tryList := node.tryList;
             LOOP
               IF tryList = NIL THEN RAISE ObValue.Exception(packet) END;
               IF tryList.exception = NIL THEN (* "else" case *)
                 env1 := env;
                 result := Term(tryList.recover, (*in-out*)env1, glob, mySelf);
                 EXIT;
               END;
               env1 := env;
               TYPECASE Term(tryList.exception, (*in-out*)env1, glob, mySelf) OF
               | ObValue.ValException(exc) => 
                   IF ObValue.SameException(exc, packet.exception) THEN
                     env1 := env;
                     result := 
                       Term(tryList.recover, (*in-out*)env1, glob, mySelf);
                     EXIT;
                   END;
                   tryList := tryList.rest;
               ELSE ObValue.RaiseError("Guard of try must be an exception", 
                   term.location);
               END;
             END;
          | ObValue.Error(packet) =>
             tryList := node.tryList;
             LOOP
               IF tryList = NIL THEN RAISE ObValue.Error(packet);END;
               IF tryList.exception = NIL THEN (* "else" case *)
                 env1 := env;
                 result := Term(tryList.recover, (*in-out*)env1, glob, mySelf);
                 EXIT;
               END;
               tryList := tryList.rest;
             END;
          END;
        END;
      | ObTree.TermTryFinally(node) =>
        VAR
          env1: ObValue.Env;
        BEGIN
          TRY 
            env1 := env;
            result := Term(node.body, (*in-out*)env1, glob, mySelf);
          FINALLY
            env1 := env;
            result := Term(node.finally, (*in-out*)env1, glob, mySelf);
          END;
        END;
      | ObTree.TermWatch(node) =>
        VAR
          env1: ObValue.Env;
          myLocalSelf: ObValue.RemObjServer;
        BEGIN
          TYPECASE mySelf OF
          | NULL => myLocalSelf := NIL;
          | ObValue.RemObjServer(remObjServer) => 
              myLocalSelf := remObjServer;
          ELSE ObValue.RaiseError(
            "watch-until does not work on remote objects", term.location);
          END;
          env1 := env;
          TYPECASE Term(node.condition, (*in-out*)env1, glob, mySelf) OF
          | ObBuiltIn.ValCondition(cond) => 
              IF myLocalSelf=NIL THEN
                ObValue.RaiseError("watch-until must be used inside a method",
                  term.location);
              ELSIF myLocalSelf.sync=NIL THEN
                ObValue.RaiseError(
                  "watch-until must be used inside a protected object",
                  term.location);
              ELSE
                LOOP
                  env1 := env;
                  TYPECASE Term(node.guard, (*in-out*)env1, glob, mySelf) OF
                  | ObValue.ValBool(guard) =>
                      IF guard.bool THEN EXIT
                      ELSE Thread.Wait(myLocalSelf.sync.mutex, cond.condition);
                      END;
                  ELSE ObValue.RaiseError(
                    "Argument 2 of watch-until must be a boolean", 
                     term.location);
                  END;
                END;
                result := ObValue.valOk;
              END;
          ELSE ObValue.RaiseError(
            "Argument 1 of watch-until must be a condition", 
             term.location);
          END;
        END;
      ELSE <*ASSERT FALSE*>
      END;
      RETURN result;
    END Term;

  PROCEDURE Call(clos: ObValue.ValFun;
    READONLY args: ObValue.Vals; loc: SynLocation.T:=NIL): ObValue.Val 
    RAISES {ObValue.Error, ObValue.Exception} =
  VAR env: ObValue.Env; binders: ObTree.IdeList;
  BEGIN
        IF clos.fun.bindersNo # NUMBER(args) THEN
          ObValue.RaiseError(ObValue.BadArgsNoMsg(clos.fun.bindersNo,
            NUMBER(args), "", ""), loc);
        END;
        env := NIL;
        binders := clos.fun.binders;
        FOR i := 0 TO NUMBER(args)-1 DO
          env := NEW(ObValue.LocalEnv,
                name := binders.first, val := args[i],
                rest := env);
          binders := binders.rest;
        END;
        RETURN Term(clos.fun.body, (*in-out*)env, clos.global, NIL);
  END Call;

  PROCEDURE CallEngine(engine: ObValue.ValEngine; arg: ObValue.Val;
    loc: SynLocation.T:=NIL): ObValue.Val 
    RAISES {ObValue.Error, ObValue.Exception} =
   BEGIN
     TRY RETURN engine.remote.Eval(arg, NIL);
     EXCEPT 
     | ObValue.ServerError(msg) =>
       ObValue.RaiseError(msg, loc); <*ASSERT FALSE*>
     | NetObj.Error(atoms) =>
       ObValue.RaiseNetException(
           "on remote engine execution", atoms, loc); <*ASSERT FALSE*>
     | Thread.Alerted =>
       ObValue.RaiseException(ObValue.threadAlerted,
           "on remote engine execution", loc); <*ASSERT FALSE*>
     END;
  END CallEngine;

BEGIN
END ObEval.

