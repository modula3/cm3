(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

MODULE ObScope;
IMPORT Text, SynLocation, SynWr, ObErr, ObTree, ObLib;

REVEAL

  Env = BRANDED OBJECT END;

  TermEnv =
    Env BRANDED OBJECT
      location: SynLocation.T;
      name: ObTree.IdeName;
      rest: Env;
    END;

PROCEDURE ScopeErrorMsg(msg: TEXT; location: SynLocation.T) =
    BEGIN
      SynWr.Text(SynWr.out, msg, loud:=TRUE); 
      SynWr.NewLine(SynWr.out, loud:=TRUE);
      SynWr.Text(SynWr.out, "  ", loud:=TRUE);
      SynLocation.PrintLocation(SynWr.out, location);
      SynWr.NewLine(SynWr.out, loud:=TRUE);
      SynWr.Flush(SynWr.out, loud:=TRUE);
   END ScopeErrorMsg;

PROCEDURE ScopeError(msg: TEXT; location: SynLocation.T) 
    RAISES {ObErr.Fail} =
  BEGIN
    ScopeErrorMsg(msg, location);
    RAISE ObErr.Fail;
  END ScopeError;

PROCEDURE UnboundIdeMsg(name: ObTree.IdeName; location: SynLocation.T) =
  BEGIN
    ScopeErrorMsg("Unbound term identifier: " & name.text, location);
  END UnboundIdeMsg;

PROCEDURE UnboundIdes(freeEnv: Env) RAISES {ObErr.Fail} =
  BEGIN
    IF freeEnv=NIL THEN RETURN END;
    LOOP
      TYPECASE freeEnv OF
      | NULL => EXIT;
      | TermEnv(node) =>
          UnboundIdeMsg(node.name, node.location);
          freeEnv := node.rest;
      ELSE <*ASSERT FALSE*>
      END
    END; 
    RAISE ObErr.Fail;
  END UnboundIdes;

PROCEDURE NewTermEnv(location: SynLocation.T; name: ObTree.IdeName;
     libEnv: ObLib.Env; rest: TermEnv): TermEnv RAISES {ObErr.Fail} =
  BEGIN
    CheckBuiltInIde(name, libEnv, location);
    RETURN NEW(TermEnv, location:=location, name:=name, rest:=rest);
  END NewTermEnv;

PROCEDURE ExtendEnv(binders: ObTree.IdeList; libEnv: ObLib.Env; env: Env; 
    VAR(*in-out*)no: INTEGER): Env RAISES {ObErr.Fail} =
  BEGIN
    IF binders = NIL THEN RETURN env;
    ELSE
      INC(no);
      RETURN 
        ExtendEnv(binders.rest, libEnv,
          NewTermEnv(binders.location, binders.first, libEnv, env),
          (*in-out*)no);
    END;
  END ExtendEnv;

PROCEDURE EnvLength(env: TermEnv): INTEGER =
VAR i: INTEGER;
BEGIN
  i:=0; WHILE env#NIL DO env:=env.rest; INC(i); END; RETURN i;
END EnvLength;

PROCEDURE CheckBuiltInIde(name: ObTree.IdeName; libEnv: ObLib.Env;
    location: SynLocation.T) RAISES {ObErr.Fail} =
  VAR pkgName: TEXT;
  BEGIN
      IF ObLib.Lookup(name.text, libEnv)#NIL THEN
          ScopeError("Identifier '" & name.text & 
            "' is reserved as the name of a module",
            location);
      END;
      CASE ObLib.LookupFixity(name.text, libEnv, (*out*)pkgName) OF
      | ObLib.OpFixity.Undefined, ObLib.OpFixity.Qualified =>
      | ObLib.OpFixity.Infix, ObLib.OpFixity.Prefix =>
          ScopeError("Identifier '" & name.text &
            "' is reserved by module '" & pkgName & "'",
            location);
      END;
  END CheckBuiltInIde;

PROCEDURE LookupTermIdeGlobal(name: ObTree.IdeName; libEnv: ObLib.Env;
    location: SynLocation.T; VAR (*in-out*)global: Env): ObTree.IdePlace 
    RAISES {ObErr.Fail}=
  VAR env: TermEnv;
  BEGIN
    env := global;
    LOOP
      IF env=NIL THEN
        global := NewTermEnv(location, name, libEnv, global);
        RETURN NEW(ObTree.IdePlaceGlobal, index:=EnvLength(global));
      ELSE
	IF ObTree.SameIdeName(name, env.name) THEN
	  RETURN NEW(ObTree.IdePlaceGlobal, index:=EnvLength(env));
	ELSE env := env.rest;
	END;
      END;
    END;
  END LookupTermIdeGlobal;

PROCEDURE LookupTermIde(name: ObTree.IdeName; libEnv: ObLib.Env;
        location: SynLocation.T;
	local: Env; VAR (*in-out*)global: Env): 
	ObTree.IdePlace RAISES {ObErr.Fail} =
  VAR index: INTEGER;
  BEGIN
    index := 1;
    LOOP
      TYPECASE local OF
      | NULL => 
        RETURN LookupTermIdeGlobal(name, libEnv, location, (*in-out*)global);
      | TermEnv(node) =>
	    IF ObTree.SameIdeName(name, node.name) THEN
	      RETURN NEW(ObTree.IdePlaceLocal, index:=index);
	    ELSE
	      INC(index);
	      local := node.rest;
	    END;
      ELSE <*ASSERT FALSE*>
      END;
    END;
  END LookupTermIde;

PROCEDURE LookupTermGlobals(free: Env; local: Env; libEnv: ObLib.Env;
    VAR (*in-out*)global: Env; VAR (*in-out*)globalsNo: INTEGER)
    : ObTree.Globals RAISES {ObErr.Fail}=
  VAR closure: ObTree.Globals;
  BEGIN
    closure := NIL;
    LOOP
      TYPECASE free OF
      | NULL => RETURN closure;
      | TermEnv(node) =>
        closure := 
          NEW(ObTree.Globals,
            location := node.location,
            name := node.name,
            place := 
              LookupTermIde(node.name, libEnv, 
                            node.location, local, (*in-out*)global),
            rest := closure);
        INC(globalsNo);
        free := node.rest;
	  ELSE <*ASSERT FALSE*>
      END;
    END;
  END LookupTermGlobals;

PROCEDURE ScopeTermBinding(binding: ObTree.TermBinding; libEnv: ObLib.Env;
    initEnv,env: Env;  VAR (*in-out*)global: Env): Env RAISES {ObErr.Fail} =
  VAR local1: Env;
  BEGIN
    TYPECASE binding OF
    | NULL => RETURN env;
    | ObTree.TermBinding(node) =>
        local1 := initEnv;
	ScopeTerm(node.term, libEnv, (*in-out*)local1, (*in-out*)global);
	RETURN ScopeTermBinding(node.rest, libEnv, initEnv,
	  NewTermEnv(binding.location,
	      node.binder, libEnv, env), (*in-out*)global);
    END;
  END ScopeTermBinding;

PROCEDURE ScopeTermBindingRec1(binding: ObTree.TermBinding; libEnv: ObLib.Env;
    env: Env): Env RAISES {ObErr.Fail} =
  BEGIN
    TYPECASE binding OF
    | NULL => RETURN env;
    | ObTree.TermBinding(node) =>
        IF NOT ISTYPE(node.term, ObTree.TermFun) THEN
          ScopeError("Non-function found in recursive definition: " & 
            node.binder.text, node.location);
        END;
	RETURN ScopeTermBindingRec1(node.rest, libEnv, 
	  NewTermEnv(binding.location, node.binder, libEnv, env));
    END;
  END ScopeTermBindingRec1;

PROCEDURE ScopeTermBindingRec2(binding: ObTree.TermBinding; libEnv: ObLib.Env;
    recEnv: Env; VAR (*in-out*)global: Env) RAISES {ObErr.Fail} =
  VAR local1: Env;
  BEGIN
    TYPECASE binding OF
    | NULL => 
    | ObTree.TermBinding(node) =>
        local1 := recEnv;
	ScopeTerm(node.term, libEnv, (*in-out*)local1, (*in-out*)global);
	ScopeTermBindingRec2(node.rest, libEnv, recEnv, (*in-out*)global);
    END;
  END ScopeTermBindingRec2;

PROCEDURE ScopeTermList(list: ObTree.TermList; libEnv: ObLib.Env; local: Env; 
    VAR (*in-out*)global: Env; temp: BOOLEAN:=FALSE): INTEGER 
    RAISES {ObErr.Fail} =
  VAR local1: Env;
  BEGIN
    TYPECASE list OF
    | NULL => RETURN 0;
    | ObTree.TermList(node) =>
        local1 := local;
        ScopeTerm(node.first, libEnv, (*in-out*)local1, (*in-out*)global, temp);
        RETURN 1 + ScopeTermList(node.rest, libEnv, local, 
			(*in-out*)global, temp);
    END;
  END ScopeTermList;

(* Temporary value analysis done for constants, and conditionals. 
   Could be extended to case and let *)

PROCEDURE ScopeTerm(term: ObTree.Term; libEnv: ObLib.Env; 
    VAR (*in-out*)local,global: Env; temp: BOOLEAN:=FALSE) RAISES {ObErr.Fail} =
  VAR local1, newLocal, newGlobal: Env; libName: TEXT;
      lib: ObLib.Env; opCode: ObLib.OpCode;
      optimizeTemps: BOOLEAN;
  BEGIN
    TYPECASE term OF
    | NULL => ObErr.Fault(SynWr.out, "ScopeTerm NIL");
    | ObTree.TermIde(node) =>
	node.place := 
	  LookupTermIde(node.name, libEnv, 
                        node.location, local, (*in-out*)global);
    | ObTree.TermConstant =>
    | ObTree.TermArray(node) =>
        node.elemsNo := 
	  ScopeTermList(node.elems, libEnv, local, (*in-out*)global);
    | ObTree.TermOption(node) =>
        local1 := local;
        ScopeTerm(node.term, libEnv, local1, (*in-out*)global);
    | ObTree.TermAlias(node) =>
        local1 := local;
        ScopeTerm(node.term, libEnv, local1, (*in-out*)global);
    | ObTree.TermOp(node) =>
	libName := node.pkg.text;
        lib := ObLib.Lookup(libName, libEnv);
	IF lib = NIL THEN
          ScopeError("Unknown module: " & node.pkg.text, term.location);
        END;
	optimizeTemps :=
	  Text.Equal(libName, "int") OR Text.Equal(libName, "real")
	    OR  Text.Equal(libName, "math");
        LOOP
          IF lib.library.Encode(node.op.text, (*out*)opCode, term.location)
          THEN
            IF node.argsNo#-1 (* not a TermOpConst *) THEN
              node.argsNo := 
                ScopeTermList(node.args, libEnv, local, (*in-out*)global, 
		  optimizeTemps);
            END;
	    node.temp := temp;
            node.package := lib.library;
            node.opCode := opCode;
            EXIT;
          END;
          lib := ObLib.Lookup(node.pkg.text, lib.rest);          
          IF lib=NIL THEN
            ScopeError("Unknown operation: " & 
              node.pkg.text & "_" & node.op.text, term.location);
          END;
        END;
    | ObTree.TermFun(node) =>
        node.bindersNo:=0;
        newLocal := 
            ExtendEnv(node.binders, libEnv, NIL, (*in-out*)node.bindersNo);
        newGlobal := NIL;
	ScopeTerm(node.body, libEnv, (*in-out*)newLocal, (*in-out*)newGlobal);
	node.globalsNo := 0;
	node.globals := 
	  LookupTermGlobals(newGlobal, local, libEnv,
	    (*in-out*)global, (*in-out*)node.globalsNo)
    | ObTree.TermAppl(node) =>
        local1 := local;
	ScopeTerm(node.fun, libEnv, (*in-out*)local1, (*in-out*)global);
	node.argsNo := ScopeTermList(node.args, libEnv, local, (*in-out*)global);
    | ObTree.TermObj(node) =>
        node.fieldsNo := 
          ScopeObjFields(node.fields, libEnv, local, (*in-out*)global);
    | ObTree.TermMeth(node) =>
        node.bindersNo:=0;
        newLocal := 
            ExtendEnv(node.binders, libEnv, NIL, (*in-out*)node.bindersNo);
        newGlobal := NIL;
	ScopeTerm(node.body, libEnv, (*in-out*)newLocal, (*in-out*)newGlobal);
	node.globalsNo := 0;
	node.globals := 
	  LookupTermGlobals(newGlobal, local, libEnv,
	    (*in-out*)global, (*in-out*)node.globalsNo)
    | ObTree.TermClone(node) =>
        node.objsNo := ScopeTermList(node.objs, libEnv, local, (*in-out*)global);
    | ObTree.TermSelect(node) =>
        local1 := local;
        ScopeTerm(node.obj, libEnv, (*in-out*)local1, (*in-out*)global);
        IF node.invoke THEN
	  node.argsNo := ScopeTermList(node.args, libEnv, local, (*in-out*)global);
	END;
    | ObTree.TermRedirect(node) =>
        local1 := local;
	ScopeTerm(node.obj, libEnv, (*in-out*)local1, (*in-out*)global);
        local1 := local;
	ScopeTerm(node.toObj, libEnv, (*in-out*)local1, (*in-out*)global);
    | ObTree.TermUpdate(node) =>
        local1 := local;
	ScopeTerm(node.obj, libEnv, (*in-out*)local1, (*in-out*)global);
        local1 := local;
	ScopeTerm(node.term, libEnv, (*in-out*)local1, (*in-out*)global);
    | ObTree.TermLet(node) =>
        IF node.rec THEN
          local := ScopeTermBindingRec1(node.binding, libEnv, local);
          ScopeTermBindingRec2(node.binding, libEnv, local, (*in-out*)global);
        ELSE
          local := 
            ScopeTermBinding(node.binding, libEnv, local, local, (*in-out*)global);
        END;
    | ObTree.TermSeq(node) =>
        local1 := local;
	ScopeTerm(node.before, libEnv, (*in-out*)local1, (*in-out*)global);
	ScopeTerm(node.after, libEnv, (*in-out*)local1, (*in-out*)global);
    | ObTree.TermAssign(node) =>
	node.place := 
	  LookupTermIde((*mod*)node.name, libEnv, node.location, 
	    local, (*in-out*)global);
        local1 := local;
        ScopeTerm(node.val, libEnv, (*in-out*)local1, (*in-out*)global);
    | ObTree.TermIf(node) =>
        local1 := local;
	ScopeTerm(node.test, libEnv, (*in-out*)local1, (*in-out*)global);
        local1 := local;
	ScopeTerm(node.ifTrue, libEnv, 
		(*in-out*)local1, (*in-out*)global, temp);
        IF node.ifFalse # NIL THEN
          local1 := local;
	  ScopeTerm(node.ifFalse, libEnv, 
		(*in-out*)local1, (*in-out*)global, temp);
        END;
    | ObTree.TermCase(node) =>
        local1 := local;
	ScopeTerm(node.option, libEnv, (*in-out*)local1, (*in-out*)global);
	ScopeTermCaseList(node.caseList, libEnv, local, (*in-out*)global);
    | ObTree.TermLoop(node) =>
        local1 := local;
	ScopeTerm(node.loop, libEnv, (*in-out*)local1, (*in-out*)global);
    | ObTree.TermExit =>
    | ObTree.TermFor(node) =>
        local1 := local;
	ScopeTerm(node.lb, libEnv, (*in-out*)local1, (*in-out*)global);
        local1 := local;
	ScopeTerm(node.ub, libEnv, (*in-out*)local1, (*in-out*)global);
        local1 := NewTermEnv(node.location, node.binder, libEnv, local);
	ScopeTerm(node.body, libEnv, (*in-out*)local1, (*in-out*)global);
    | ObTree.TermForeach(node) =>
        local1 := local;
	ScopeTerm(node.range, libEnv, (*in-out*)local1, (*in-out*)global);
        local1 := NewTermEnv(node.location, node.binder, libEnv, local);
	ScopeTerm(node.body, libEnv, (*in-out*)local1, (*in-out*)global);
    | ObTree.TermException(node) =>
        local1 := local;
	ScopeTerm(node.name, libEnv, (*in-out*)local1, (*in-out*)global);
    | ObTree.TermRaise(node) =>
        local1 := local;
	ScopeTerm(node.exception, libEnv, (*in-out*)local1, (*in-out*)global);
    | ObTree.TermTry(node) =>
        local1 := local;
	ScopeTerm(node.body, libEnv, (*in-out*)local1, (*in-out*)global);
	ScopeTermTryList(node.tryList, libEnv, local, (*in-out*)global);
    | ObTree.TermTryFinally(node) =>
        local1 := local;
	ScopeTerm(node.body, libEnv, (*in-out*)local1, (*in-out*)global);
        local1 := local;
	ScopeTerm(node.finally, libEnv, (*in-out*)local1, (*in-out*)global);
    | ObTree.TermWatch(node) =>
        local1 := local;
	ScopeTerm(node.condition, libEnv, (*in-out*)local1, (*in-out*)global);
        local1 := local;
	ScopeTerm(node.guard, libEnv, (*in-out*)local1, (*in-out*)global);
    ELSE ObErr.Fault(SynWr.out, "ScopeTerm");
    END;
  END ScopeTerm;

PROCEDURE ScopeObjFields(fields: ObTree.TermObjFields; libEnv: ObLib.Env;
    local: Env; VAR (*in-out*)global: Env): INTEGER RAISES {ObErr.Fail} =
  VAR local1: Env;
  BEGIN
    TYPECASE fields OF
    | NULL => RETURN 0;
    | ObTree.TermObjFields(node) =>
        local1 := local;
        ScopeTerm(node.term, libEnv, (*in-out*)local1, (*in-out*)global);
        RETURN 1 + ScopeObjFields(node.rest, libEnv, local, (*in-out*)global);
    END
  END ScopeObjFields;

PROCEDURE ScopeTermCaseList(list: ObTree.TermCaseList; libEnv: ObLib.Env;
    local: Env; VAR (*in-out*)global: Env) RAISES {ObErr.Fail} =
  VAR local1: Env;
  BEGIN
    TYPECASE list OF
    | NULL =>
    | ObTree.TermCaseList(node) =>
        IF node.binder=NIL THEN
          local1 := local;
        ELSE
          local1 := NewTermEnv(node.location, node.binder, libEnv, local);
        END;
        ScopeTerm(node.body, libEnv, (*in-out*)local1, (*in-out*)global);
        ScopeTermCaseList(node.rest, libEnv, local, (*in-out*)global);
    END;
  END ScopeTermCaseList;

PROCEDURE ScopeTermTryList(list: ObTree.TermTryList; libEnv: ObLib.Env;
    local: Env; VAR (*in-out*)global: Env) RAISES {ObErr.Fail} =
  VAR local1: Env;
  BEGIN
    TYPECASE list OF
    | NULL =>
    | ObTree.TermTryList(node) =>
        IF node.exception#NIL THEN 
          local1 := local;
          ScopeTerm(node.exception, libEnv, (*in-out*)local1, 
                    (*in-out*)global) 
        END;
        local1 := local;
        ScopeTerm(node.recover, libEnv, (*in-out*)local1, (*in-out*)global);
        ScopeTermTryList(node.rest, libEnv, local, (*in-out*)global);
    END;
  END ScopeTermTryList;

PROCEDURE Setup() =
  BEGIN
  END Setup;

BEGIN
END ObScope.
