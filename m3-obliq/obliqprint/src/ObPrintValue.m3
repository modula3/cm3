(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

MODULE ObPrintValue;
IMPORT Text, ObErr, SynWr, ObCommand, ObTree, ObPrintTree, Process,
       ObCheck, NetObj, ObValue, ObLib, Thread, SharedObj;

VAR 
  printClosureGlobals: BOOLEAN;

  PROCEDURE Setup() =
    BEGIN
      printClosureGlobals := FALSE;
      ObCommand.Register(ObTree.doCommandSet,
        NEW(ObCommand.T, name:="ShowClosureGlobals", 
            sortingName:="ShowClosureGlobals",
            Exec:=PrintClosureGlobals));
    END Setup;

  PROCEDURE PrintValArray(swr: SynWr.T; array: REF ObValue.Vals; 
    libEnv: ObLib.Env; printEnv: ObTree.Env; depth: INTEGER) =
  VAR sep: TEXT; size: INTEGER;
  BEGIN
      sep := "";
      size := NUMBER(array^);
      FOR i:=0 TO size-1 DO
        SynWr.Text(swr, sep); sep := ", ";
        SynWr.Break(swr);
        SynWr.Beg(swr, 2);
        PrintVal(swr, array^[i], libEnv, printEnv, depth-1);
        SynWr.End(swr);
      END;
  END PrintValArray;

  PROCEDURE PrintVal(swr: SynWr.T; val: ObValue.Val; 
    libEnv: ObLib.Env; printEnv: ObTree.Env; depth: INTEGER) =
  VAR val1: ObValue.Val; protected, serialized: BOOLEAN; who: TEXT;
    fields: REF ObValue.ObjFields;
  BEGIN
    IF val=NIL THEN SynWr.Char(swr, '_'); RETURN END;
    TYPECASE val OF
    | ObValue.ValRemVar(node) => 
      TRY val1 := node.remote.Get();
      EXCEPT
      | NetObj.Error, Thread.Alerted => 
        SynWr.Text(swr, "<remote variable disconnected>");
      END;
      PrintVal(swr, val1, libEnv, printEnv, depth);
    | ObValue.ValReplVar(node) => 
      TRY val1 := node.replica.Get();
      EXCEPT
      | SharedObj.Error => 
        SynWr.Text(swr, "<stale shared variable>");
      END;
      SynWr.Text(swr, "replicated ");
      PrintVal(swr, val1, libEnv, printEnv, depth);
    | ObValue.ValSimpleVar(node) => 
      TRY val1 := node.Get();
      EXCEPT
      | NetObj.Error, Thread.Alerted => 
        SynWr.Text(swr, "<remote variable disconnected>");
      | SharedObj.Error => 
        SynWr.Text(swr, "<stale shared variable>");
      END;
      SynWr.Text(swr, "simple ");
      PrintVal(swr, val1, libEnv, printEnv, depth);
    | ObValue.ValOk => ObPrintTree.PrintOk(swr);
    | ObValue.ValBool(node) => ObPrintTree.PrintBool(swr, node.bool);
    | ObValue.ValChar(node) => ObPrintTree.PrintChar(swr, node.char);
    | ObValue.ValText(node) => ObPrintTree.PrintText(swr, node.text);
    | ObValue.ValInt(node) => 
	ObPrintTree.PrintInt(swr, node.int);
	IF node.temp THEN SynWr.Text(swr, "<TEMP!>") END;
    | ObValue.ValReal(node) => 
	ObPrintTree.PrintReal(swr, node.real);
	IF node.temp THEN SynWr.Text(swr, "<TEMP!>") END;
    | ObValue.ValOption(node) => 
        IF depth <= 0 THEN SynWr.Text(swr, "..."); RETURN END;
        SynWr.Beg(swr);
          SynWr.Beg(swr, 2);
	    SynWr.Text(swr, "option ");
	  SynWr.Break(swr);
            SynWr.Beg(swr, 4);
              ObPrintTree.PrintText(swr, node.tag);
	      SynWr.Text(swr, " => ");
            SynWr.End(swr);
	  SynWr.Break(swr);
	    PrintVal(swr, node.val, libEnv, printEnv, depth-1);
	    SynWr.Char(swr, ' ');
	  SynWr.End(swr);
	SynWr.Break(swr);
	  SynWr.Text(swr, "end");
	SynWr.End(swr);
    | ObValue.ValAlias(node) => 
        IF depth <= 0 THEN SynWr.Text(swr, "..."); RETURN END;
        SynWr.Beg(swr);
          SynWr.Beg(swr, 2);
	    SynWr.Text(swr, "alias ");
	  SynWr.Break(swr);
            SynWr.Beg(swr, 4);
	      SynWr.Text(swr, node.label);
	      SynWr.Text(swr, " of ");
            SynWr.End(swr);
	  SynWr.Break(swr);
	    PrintVal(swr, node.obj, libEnv, printEnv, depth-1);
	    SynWr.Char(swr, ' ');
	  SynWr.End(swr);
	SynWr.Break(swr);
	  SynWr.Text(swr, "end");
	SynWr.End(swr);
    | ObValue.ValRemArray(node) => 
        IF depth <= 0 THEN SynWr.Text(swr, "..."); RETURN END;
        SynWr.Beg(swr, 1);
          SynWr.Char(swr, '[');
          TRY
            PrintValArray(swr, node.remote.Obtain(), libEnv, printEnv, depth);
          EXCEPT
          | NetObj.Error, Thread.Alerted => 
            SynWr.Text(swr, "<remote array disconnected>");
          END;
          SynWr.Char(swr, ']');
	SynWr.End(swr);
    | ObValue.ValReplArray(node) => 
        IF depth <= 0 THEN SynWr.Text(swr, "..."); RETURN END;
        SynWr.Beg(swr, 1);
          SynWr.Text(swr, "replicated [");
          TRY
            PrintValArray(swr, node.replica.Obtain(), libEnv, printEnv, depth);
          EXCEPT
          | SharedObj.Error => 
            SynWr.Text(swr, "<stale shared array>");
          END;
          SynWr.Char(swr, ']');
	SynWr.End(swr);
    | ObValue.ValSimpleArray(node) => 
        IF depth <= 0 THEN SynWr.Text(swr, "..."); RETURN END;
        SynWr.Beg(swr, 1);
          SynWr.Text(swr, "simple [");
          TRY
            PrintValArray(swr, node.simple.Obtain(), libEnv, printEnv, depth);
          EXCEPT
          | NetObj.Error, Thread.Alerted => 
            SynWr.Text(swr, "<remote array disconnected>");
          | SharedObj.Error => 
            SynWr.Text(swr, "<stale shared array>");
          END;
          SynWr.Char(swr, ']');
	SynWr.End(swr);
    | ObValue.ValAnything(node) => 
        SynWr.Text(swr, node.Print());
    | ObValue.ValFun(node) => 
        IF depth <= 0 THEN SynWr.Text(swr, "..."); RETURN END;
        PrintClosure(swr, node.fun, node.fun.globals, node.global, 
          libEnv, printEnv, depth);
    | ObValue.ValMeth(node) => 
        IF depth <= 0 THEN SynWr.Text(swr, "..."); RETURN END;
        PrintClosure(swr, node.meth, node.meth.globals, node.global, 
          libEnv, printEnv, depth);
   | ObValue.ValRemObj(node) => 
        IF depth <= 0 THEN SynWr.Text(swr, "..."); RETURN END;
        TYPECASE node.remote OF
        | ObValue.RemObjServer(remObj) => 
          TRY
            who := remObj.Who((*out*)protected, (*out*)serialized);
    	    SynWr.Beg(swr, 1); 
    	    SynWr.Text(swr, "{");
            PrintProtected(swr, protected);
            PrintSerialized(swr, serialized);
            TRY
              fields := remObj.Obtain(TRUE);
              PrintValObjFields(swr, fields, libEnv, printEnv, depth, FALSE);
            EXCEPT
            | ObValue.ServerError =>
              SynWr.Text(swr, "<cannot obtain fields of protected object>");
            END;
    	    SynWr.Char(swr, '}');
    	    SynWr.End(swr);
          EXCEPT
          | NetObj.Error, Thread.Alerted => 
            SynWr.Text(swr, "<remote object disconnected>");
          END;
        ELSE 
          SynWr.Beg(swr, 1); SynWr.Text(swr, "{");
          TRY 
            who := node.remote.Who((*out*)protected, (*out*)serialized);
            IF Text.Empty(who) THEN SynWr.Text(swr, "<unknown>");
            ELSE SynWr.Text(swr, who);
            END;
          EXCEPT NetObj.Error, Thread.Alerted => 
            SynWr.Text(swr, "<disconnected>");
          END;
          SynWr.Char(swr, '}'); SynWr.End(swr);
        END;
   | ObValue.ValSimpleObj(node) => 
        IF depth <= 0 THEN SynWr.Text(swr, "..."); RETURN END;
        TRY
          who := node.Who((*out*)protected, (*out*)serialized);
          SynWr.Beg(swr, 1); 
          SynWr.Text(swr, "{simple, ");
          PrintProtected(swr, protected);
          PrintSerialized(swr, serialized);
          TRY
            fields := node.Obtain(TRUE);
            PrintValObjFields(swr, fields, libEnv, printEnv, depth, FALSE);
          EXCEPT
          | ObValue.ServerError =>
            SynWr.Text(swr, "<cannot obtain fields of protected object>");
          END;
          SynWr.Char(swr, '}');
          SynWr.End(swr);
        EXCEPT
        | SharedObj.Error => 
          SynWr.Text(swr, "<replicated object invalidated>");
        | NetObj.Error, Thread.Alerted => 
          SynWr.Text(swr, "<remote object disconnected>");
        END;
   | ObValue.ValReplObj(node) => 
        IF depth <= 0 THEN SynWr.Text(swr, "..."); RETURN END;
        TRY
          who := node.Who((*out*)protected, (*out*)serialized);
          SynWr.Beg(swr, 1); 
          SynWr.Text(swr, "{replicated, ");
          PrintProtected(swr, protected);
          PrintSerialized(swr, serialized);
          TRY
            fields := node.Obtain(TRUE);
            PrintValObjFields(swr, fields, libEnv, printEnv, depth, TRUE);
          EXCEPT
          | ObValue.ServerError =>
            SynWr.Text(swr, "<cannot obtain fields of protected object>");
          END;
          SynWr.Char(swr, '}');
          SynWr.End(swr);
        EXCEPT
        | SharedObj.Error => 
          SynWr.Text(swr, "<replicated object invalidated>");
        | NetObj.Error, Thread.Alerted => 
          SynWr.Text(swr, "<remote object disconnected>");
        END;
   | ObValue.ValEngine(node) => 
        IF depth <= 0 THEN SynWr.Text(swr, "..."); RETURN END;
        SynWr.Beg(swr, 1); SynWr.Text(swr, "<Engine ");
        TRY SynWr.Text(swr, node.remote.Who())
        EXCEPT NetObj.Error, Thread.Alerted => 
          SynWr.Text(swr, "<disconnected>");
        END;
        SynWr.Char(swr, '>'); SynWr.End(swr);
    | ObValue.ValException(node) => 
        SynWr.Text(swr, "<the exception '" & node.name & "'>");
    ELSE SynWr.Text(swr, "<?>");
    END;
  END PrintVal;

  PROCEDURE PrintValSummary(swr: SynWr.T; val: ObValue.Val; 
    libEnv: ObLib.Env; printEnv: ObTree.Env) =
  VAR val1: ObValue.Val; protected, serialized: BOOLEAN; who: TEXT;
    fields: REF ObValue.ObjFields;
  BEGIN
    IF val=NIL THEN SynWr.Char(swr, '_'); RETURN END;
    TYPECASE val OF
    | ObValue.ValRemVar(node) => 
      TRY val1 := node.remote.Get();
      EXCEPT NetObj.Error, Thread.Alerted => 
        SynWr.Text(swr, "<remote variable disconnected>");
      END;
      PrintValSummary(swr, val1, libEnv, printEnv);
    | ObValue.ValReplVar(node) => 
      TRY val1 := node.replica.Get();
      EXCEPT
      | SharedObj.Error => 
        SynWr.Text(swr, "<stale shared variable>");
      END;
      SynWr.Text(swr, "replicated ");
      PrintValSummary(swr, val1, libEnv, printEnv);
    | ObValue.ValSimpleVar(node) => 
      TRY val1 := node.simple.Get();
      EXCEPT
      | NetObj.Error, Thread.Alerted => 
        SynWr.Text(swr, "<remote variable disconnected>");
      | SharedObj.Error => 
        SynWr.Text(swr, "<stale shared variable>");
      END;
      SynWr.Text(swr, "simple ");
      PrintValSummary(swr, val1, libEnv, printEnv);
    | ObValue.ValOk, ObValue.ValBool, ObValue.ValChar,
      ObValue.ValInt, ObValue.ValReal => 
        PrintVal(swr, val, libEnv, printEnv, 10);
    | ObValue.ValText => SynWr.Text(swr, "\" ... \"");
    | ObValue.ValOption(node) => 
        SynWr.Beg(swr);
          SynWr.Beg(swr, 2);
	    SynWr.Text(swr, "option ");
	  SynWr.Break(swr);
            SynWr.Beg(swr, 4);
	      ObPrintTree.PrintText(swr, node.tag);
	      SynWr.Text(swr, " => ... end");
            SynWr.End(swr);
	  SynWr.End(swr);
	SynWr.End(swr);
    | ObValue.ValAlias(node) => 
        SynWr.Beg(swr);
          SynWr.Beg(swr, 2);
	    SynWr.Text(swr, "alias ");
	  SynWr.Break(swr);
            SynWr.Beg(swr, 4);
	      SynWr.Text(swr, node.label);
	      SynWr.Text(swr, " of ... end");
            SynWr.End(swr);
	  SynWr.End(swr);
	SynWr.End(swr);
    | ObValue.ValRemArray => 
	SynWr.Text(swr, "[ ... ]");
    | ObValue.ValReplArray => 
	SynWr.Text(swr, "replicated [ ... ]");
    | ObValue.ValSimpleArray => 
	SynWr.Text(swr, "simple [ ... ]");
    | ObValue.ValAnything(node) => 
        SynWr.Text(swr, node.Print());
    | ObValue.ValFun(node) => 
        ObPrintTree.PrintSignature(swr, node.fun, libEnv, printEnv);
    | ObValue.ValMeth(node) => 
        ObPrintTree.PrintSignature(swr, node.meth, libEnv, printEnv);
   | ObValue.ValRemObj(node) => 
        TYPECASE node.remote OF
        | ObValue.RemObjServer(remObj) => 
          TRY
            who := remObj.Who((*out*)protected, (*out*)serialized);
    	    SynWr.Beg(swr, 1); 
    	    SynWr.Text(swr, "{");
            PrintProtected(swr, protected);
            PrintSerialized(swr, serialized);
            TRY
              fields := remObj.Obtain(TRUE);
              PrintValObjFieldsSummary(swr, fields, libEnv, printEnv, FALSE);
            EXCEPT
            | ObValue.ServerError =>
              SynWr.Text(swr, "<cannot obtain fields of protected object>");
            END;
    	    SynWr.Char(swr, '}');
    	    SynWr.End(swr);
          EXCEPT
          | NetObj.Error, Thread.Alerted => 
            SynWr.Text(swr, "<remote object disconnected>");
          END;
        ELSE 
          SynWr.Beg(swr, 1); SynWr.Text(swr, "{");
          TRY 
            who := node.remote.Who((*out*)protected, (*out*)serialized);
            IF Text.Empty(who) THEN SynWr.Text(swr, "<unknown>");
            ELSE SynWr.Text(swr, who);
            END;
          EXCEPT NetObj.Error, Thread.Alerted => 
            SynWr.Text(swr, "<disconnected>");
          END;
          SynWr.Char(swr, '}'); SynWr.End(swr);
        END;
   | ObValue.ValSimpleObj(node) => 
        TRY
          who := node.Who((*out*)protected, (*out*)serialized);
          SynWr.Beg(swr, 1); 
          SynWr.Text(swr, "{simple, ");
          PrintProtected(swr, protected);
          PrintSerialized(swr, serialized);
          TRY
            fields := node.Obtain(TRUE);
            PrintValObjFieldsSummary(swr, fields, libEnv, printEnv, FALSE);
          EXCEPT
          | ObValue.ServerError =>
            SynWr.Text(swr, "<cannot obtain fields of protected object>");
          END;
          SynWr.Char(swr, '}');
          SynWr.End(swr);
        EXCEPT
        | SharedObj.Error => 
          SynWr.Text(swr, "<replicated object invalidated>");
        | NetObj.Error, Thread.Alerted => 
          SynWr.Text(swr, "<remote object disconnected>");
        END;
   | ObValue.ValReplObj(node) => 
        TRY
          who := node.Who((*out*)protected, (*out*)serialized);
          SynWr.Beg(swr, 1); 
          SynWr.Text(swr, "{replicated, ");
          PrintProtected(swr, protected);
          PrintSerialized(swr, serialized);
          TRY
            fields := node.Obtain(TRUE);
            PrintValObjFieldsSummary(swr, fields, libEnv, printEnv, TRUE);
          EXCEPT
          | ObValue.ServerError =>
            SynWr.Text(swr, "<cannot obtain fields of protected object>");
          END;
          SynWr.Char(swr, '}');
          SynWr.End(swr);
        EXCEPT
        | SharedObj.Error => 
          SynWr.Text(swr, "<replicated object invalidated>");
        | NetObj.Error, Thread.Alerted => 
          SynWr.Text(swr, "<remote object disconnected>");
        END;
   | ObValue.ValEngine(node) => 
        SynWr.Beg(swr, 1); SynWr.Text(swr, "<Engine ");
        TRY SynWr.Text(swr, node.remote.Who())
        EXCEPT NetObj.Error, Thread.Alerted => 
          SynWr.Text(swr, "<disconnected>");
        END;
        SynWr.Char(swr, '>'); SynWr.End(swr);
    | ObValue.ValException(node) => 
        SynWr.Text(swr, "<the exception '" & node.name & "'>");
    ELSE SynWr.Text(swr, "<?>");
    END;
  END PrintValSummary;

  PROCEDURE PrintClosure(swr: SynWr.T; fun: ObTree.Term;
    globalsList: ObTree.Globals; globalsEnv: ObValue.GlobalEnv;
    libEnv: ObLib.Env; printEnv: ObTree.Env; depth: INTEGER) =
  VAR sep: TEXT;
  BEGIN
    IF NUMBER(globalsEnv^)=0 THEN
      ObPrintTree.PrintTerm(swr, fun, libEnv, printEnv, depth);
    ELSIF printClosureGlobals THEN
      SynWr.Beg(swr, 2);
        ObPrintTree.PrintTerm(swr, fun, libEnv, printEnv, depth);
        SynWr.Char(swr, ' ');
      SynWr.Break(swr);
        SynWr.Text(swr, "where ");
        sep := "";
        FOR i:=0 TO NUMBER(globalsEnv^)-1 DO
          SynWr.Text(swr, sep); sep:=", ";
          SynWr.Break(swr);
          SynWr.Beg(swr, 2);
            SynWr.Beg(swr, 4);
              ObPrintTree.PrintIdeName(swr, globalsList.name, printEnv);
              SynWr.Text(swr, " = ");
            SynWr.End(swr);
          SynWr.Break(swr);
            PrintVal(swr, globalsEnv[i], libEnv, printEnv, depth-1);
          SynWr.End(swr);
          globalsList := globalsList.rest;
        END;
        SynWr.Char(swr, ' ');
      SynWr.Break(swr);
        SynWr.Text(swr, "end");
      SynWr.End(swr);
    ELSE
      SynWr.Beg(swr, 2);
        SynWr.Beg(swr, 4);
          SynWr.Text(swr, "global(");
          sep := "";
          FOR i:=0 TO NUMBER(globalsEnv^)-1 DO
            SynWr.Text(swr, sep); sep:=",";
            SynWr.Break(swr);
            ObPrintTree.PrintIdeName(swr, globalsList.name, printEnv);
            globalsList := globalsList.rest;
          END;
          SynWr.Text(swr, ") ");
        SynWr.End(swr);
      SynWr.Break(swr);
        ObPrintTree.PrintTerm(swr, fun, libEnv, printEnv, depth);
      SynWr.End(swr);
    END;
  END PrintClosure;

  PROCEDURE PrintProtected(swr: SynWr.T; protected: BOOLEAN) =
  BEGIN
    IF protected THEN
      SynWr.Break(swr);
      SynWr.Beg(swr, 2);
      SynWr.Text(swr, "protected, "); 
      SynWr.End(swr);
    END;
  END PrintProtected;

  PROCEDURE PrintSerialized(swr: SynWr.T; serialized: BOOLEAN) =
  BEGIN
    IF serialized THEN
      SynWr.Break(swr);
      SynWr.Beg(swr, 2);
      SynWr.Text(swr, "serialized, ");
      SynWr.End(swr);
    END;
  END PrintSerialized;

  PROCEDURE PrintValObjFields(swr: SynWr.T; fields: REF ObValue.ObjFields; 
    libEnv: ObLib.Env; printEnv: ObTree.Env; depth: INTEGER; 
    <*UNUSED*>isReplicated: BOOLEAN) =
  VAR sep: TEXT;
  BEGIN
    sep := "";
    FOR i:=0 TO NUMBER(fields^)-1 DO
	SynWr.Text(swr, sep); sep := ", ";
        SynWr.Break(swr);
        SynWr.Beg(swr, 2);
	  SynWr.Beg(swr, 4);
	    SynWr.Text(swr, fields^[i].label);
	    SynWr.Text(swr, " => ");
            (*
            IF isReplicated AND fields^[i].meth.update THEN
              SynWr.Text(swr, "update ");
            END;
            *)
	  SynWr.End(swr);
	SynWr.Break(swr);
	  PrintVal(swr, fields^[i].field, libEnv, printEnv, depth-1);
	SynWr.End(swr);
    END;
  END PrintValObjFields;

  PROCEDURE PrintValObjFieldsSummary(swr: SynWr.T; 
    fields: REF ObValue.ObjFields; 
    <*UNUSED*>libEnv: ObLib.Env; <*UNUSED*>printEnv: ObTree.Env; 
    <*UNUSED*>isReplicated: BOOLEAN) =
  VAR sep: TEXT;
  BEGIN
    sep := "";
    FOR i:=0 TO NUMBER(fields^)-1 DO
	SynWr.Text(swr, sep); sep := ", ";
        SynWr.Break(swr);
        SynWr.Beg(swr, 2);
	  SynWr.Beg(swr, 4);
	    SynWr.Text(swr, fields^[i].label);
	    SynWr.Text(swr, "=> ");
            (*
            IF isReplicated AND fields^[i].meth.update THEN
              SynWr.Text(swr, "update ");
            END;
            *)
	    SynWr.Text(swr, "... ");
	  SynWr.End(swr);
	SynWr.End(swr);
    END;
  END PrintValObjFieldsSummary;

PROCEDURE PrintPhraseLet (swr                   : SynWr.T;
                          checkEnv, checkEnvStop: ObCheck.Env;
                          env, envStop          : ObValue.Env;
                          var                   : BOOLEAN;
                          libEnv                : ObLib.Env;
                          depth                 : INTEGER      ) =
  BEGIN
    SynWr.Beg(swr, 2);
    IF var THEN SynWr.Text(swr, "var ") ELSE SynWr.Text(swr, "let ") END;
    PrintTermBinding(
      swr, checkEnv, checkEnvStop, env, envStop, libEnv, depth);
    SynWr.End(swr);
    SynWr.NewLine(swr);
  END PrintPhraseLet;

PROCEDURE PrintTermBinding(swr: SynWr.T; checkEnv, checkEnvStop: ObCheck.Env; 
    env, envStop: ObValue.Env; libEnv: ObLib.Env; depth: INTEGER) =
  BEGIN
    TRY
    IF (checkEnv=checkEnvStop) AND (env=envStop) THEN RETURN END;
    IF (checkEnv=checkEnvStop) OR (env=envStop) OR
	NOT ObTree.SameIdeName(checkEnv.name, env.name) THEN
	ObErr.Fault(swr, "Envs do not match. (1)"); (* NOWARN *)
    END;
    PrintTermBinding(swr, checkEnv.rest, checkEnvStop, 
	env.rest, envStop, libEnv, depth);
    TYPECASE checkEnv OF
    | ObCheck.TermEnv(checkNode) =>
	TYPECASE env OF
	| ObValue.LocalEnv(valueNode) =>
            IF env.rest#envStop THEN SynWr.Text(swr, ", ") END;
            SynWr.Break(swr);
	    SynWr.Beg(swr, 2);
	      SynWr.Beg(swr, 4);
	        ObPrintTree.PrintIdeName(swr, checkNode.name, checkEnv);
                SynWr.Text(swr, " = ");
	      SynWr.End(swr);
	    SynWr.Break(swr);
              (* PrintVal(swr, valueNode.val, libEnv, checkNode.rest, depth-1); *)
              PrintValSummary(swr, valueNode.val, libEnv, checkNode.rest);
	    SynWr.End(swr);
	ELSE ObErr.Fault(swr, "Envs do not match. (2)"); (* NOWARN *)
	END;
    ELSE ObErr.Fault(swr, "PrintTermBinding"); (* NOWARN *)
    END;
    EXCEPT
    | ObErr.Fail => Process.Crash("Unexpected failure in PrintTermBinding");
    END;
  END PrintTermBinding;

  PROCEDURE PrintClosureGlobals(wr: SynWr.T; self: ObCommand.T; arg: TEXT; 
                                <*UNUSED*>data: REFANY:=NIL) =
    BEGIN
      IF Text.Equal(arg, "!") OR Text.Equal(arg, "?") THEN
	SynWr.Text(wr , self.name & " {On Off} is ");
	IF printClosureGlobals THEN SynWr.Text(wr , "On");
	ELSE SynWr.Text(wr , "Off"); END;
	SynWr.NewLine(wr );
      ELSIF Text.Equal(arg, "On") THEN printClosureGlobals:=TRUE;
      ELSIF Text.Equal(arg, "Off") THEN printClosureGlobals:=FALSE;
      ELSE
	SynWr.Text(wr , "Command " & self.name 
	  & ": bad argument: " & arg);
	SynWr.NewLine(wr );
      END;
    END PrintClosureGlobals;

BEGIN
END ObPrintValue.
