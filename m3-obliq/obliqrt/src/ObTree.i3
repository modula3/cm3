(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

INTERFACE ObTree;
IMPORT ObCommand, SynLocation;

TYPE

  IdeName =
    SynLocation.Located BRANDED "IdeName" OBJECT
      text: TEXT;
      variant: INTEGER;
    END;

  IdePlace = BRANDED "IdePlace" OBJECT END;
  IdePlaceLocal = IdePlace BRANDED "IdePlaceLocal" OBJECT 
      index: INTEGER; (* > 0 *) 
    END;
  IdePlaceGlobal = IdePlace BRANDED "IdePlaceGlobal" OBJECT 
      index: INTEGER; (* > 0 *)  
    END;
  
  Globals =
    SynLocation.Located BRANDED "Globals" OBJECT
      name: IdeName;
      place: IdePlace;
      rest: Globals;
    END;

  Phrase =
    SynLocation.Located BRANDED OBJECT END;

  PhraseCommand =
    Phrase BRANDED OBJECT
      set: ObCommand.Set;
      name, arg: TEXT;
    END; 

  PhraseTerm =
    Phrase BRANDED OBJECT
      term: Term; 
      printDepth: INTEGER;
    END; 

  TermBinding =
    SynLocation.Located BRANDED "TermBinding" OBJECT
      binder: IdeName;
      term: Term;
      rest: TermBinding;
    END; 

  Term = 
    SynLocation.Located BRANDED "Term" OBJECT END;

  TermConstant =
    Term BRANDED "TermConstant" OBJECT
      cache: REFANY:=NIL;
    END;

  TermIde =
    Term BRANDED "TermIde" OBJECT
      name: IdeName;
      place: IdePlace;
    END;

  TermOk =
    TermConstant BRANDED "TermOk" OBJECT
    END;

  TermBool =
    TermConstant BRANDED "TermBool" OBJECT
      bool: BOOLEAN;
    END;

  TermChar =
    TermConstant BRANDED "TermChar" OBJECT
      char: CHAR;
    END;

  TermText =
    TermConstant BRANDED "TermText" OBJECT
      text: TEXT;
    END;

  TermInt =
    TermConstant BRANDED "TermInt" OBJECT
      int: INTEGER;
    END;

  TermReal =
    TermConstant BRANDED "TermReal" OBJECT
      real: LONGREAL
    END;

  TermArray =
    Term BRANDED "TermArray" OBJECT
      elems: TermList;
      elemsNo: INTEGER;
    END;

  TermOption =
    Term BRANDED "TermOption" OBJECT
      tag: IdeName;
      term: Term;
    END;

  TermOp =
    Term BRANDED "TermOp" OBJECT
      pkg, op: IdeName;
      args: TermList;
      argsNo: INTEGER;
      temp: BOOLEAN; (* Is the result a temporary value? *)
      package: ROOT (* ObPkg.T *);
      opCode: ROOT (* ObPkg.OpCode *);
    END;

  TermFun =
    Term BRANDED "TermFun" OBJECT
      binders: IdeList;
      bindersNo: INTEGER;
      body: Term;
      globals: Globals;
      globalsNo: INTEGER;
    END;

  TermAppl =
    Term BRANDED "TermAppl" OBJECT
      fun: Term; 
      args: TermList;
      argsNo: INTEGER;
    END;

  TermMeth =
    Term BRANDED "TermMeth" OBJECT
      binders: IdeList;
      bindersNo: INTEGER;
      body: Term;
      globals: Globals;
      globalsNo: INTEGER;
    END;

  TermAlias =
    Term BRANDED "TermAlias" OBJECT
      label: IdeName;
      term: Term;
    END;

  TermObj =
    Term BRANDED "TermObj" OBJECT
      protected: BOOLEAN; 
      sync: Sync; (* NIL if not synchronized. *)
      fields: TermObjFields;
      fieldsNo: INTEGER;
    END;

  Sync = {None, Monitored, Reentrant};
    
  TermObjFields =
    SynLocation.Located BRANDED "TermObjFields" OBJECT
      label: IdeName;
      term: Term;
      rest: TermObjFields;
    END;

  TermClone =
    Term BRANDED "TermClone" OBJECT
      objs: TermList;
      objsNo: INTEGER;
    END;

  TermRedirect =
    Term BRANDED "TermRedirect" OBJECT
      obj: Term;
      toObj: Term;
    END;

  TermSelect =
    Term BRANDED "TermSelect" OBJECT
      obj: Term;
      label: IdeName;
      labelIndexHint: INTEGER;
      invoke: BOOLEAN;
      args: TermList;
      argsNo: INTEGER;
    END;

  TermUpdate =
    Term BRANDED "TermUpdate" OBJECT
      obj: Term;
      label: IdeName;
      labelIndexHint: INTEGER;
      term: Term;
    END;

  TermSeq =
    Term BRANDED "TermSeq" OBJECT
      before,after: Term;
    END;

  TermLet =
    Term BRANDED "TermLet" OBJECT
      var, rec: BOOLEAN;
      binding: TermBinding;
    END;

  TermAssign =
    Term BRANDED "TermAssign" OBJECT
      name: IdeName;
      place: IdePlace;
      val: Term;
    END;

  TermIf =
    Term BRANDED "TermIf" OBJECT
      test,ifTrue: Term;
      ifFalse: Term; (* NIL if no else branch *)
    END;

  TermCase =
    Term BRANDED "TermCase" OBJECT
      option: Term;
      caseList: TermCaseList;
    END;

  TermLoop =
    Term BRANDED "TermLoop" OBJECT
      loop: Term;
    END;

  TermExit =
    Term BRANDED "TermExit" OBJECT
    END;

  TermFor =
    Term BRANDED "TermFor" OBJECT
      binder: IdeName;
      lb,ub,body: Term;
    END;

  TermForeach =
    Term BRANDED "TermForeach" OBJECT
      binder: IdeName; 
      range, body: Term;
      map: BOOLEAN;
    END;

  TermException =
    Term BRANDED "TermException" OBJECT
      name: Term;
    END;

  TermRaise =
    Term BRANDED "TermRaise" OBJECT
      exception: Term;
    END;

  TermTry =
    Term BRANDED "TermTry" OBJECT
      body: Term;
      tryList: TermTryList;
    END;

  TermTryFinally =
    Term BRANDED "TermFinally" OBJECT
      body: Term;
      finally: Term;
    END;

  TermWatch =
    Term BRANDED "TermWatch" OBJECT
      condition, guard: Term;
    END;

  TermCaseList =
    SynLocation.Located BRANDED "TermCaseList" OBJECT
      tag: IdeName;       (* NIL for "else" *)
      binder: IdeName;    (* NIL for "else" or for no binder *)
      body: Term;
      rest: TermCaseList; (* NIL for "else" *)
    END;

  TermTryList =
    SynLocation.Located BRANDED "TermTryList" OBJECT
      exception: Term;    (* NIL for "else" *)
      recover: Term;
      rest: TermTryList;  (* NIL for "else" *)
    END;

  IdeList =
    SynLocation.Located BRANDED "TermIdeList" OBJECT
      first: IdeName;
      rest: IdeList;
    END;

  TermList =
    SynLocation.Located BRANDED "TermList" OBJECT
      first: Term;
      rest: TermList;
    END;

  Env =
     OBJECT
        name: IdeName;
        decoration: INTEGER;
        rest: Env;
     END;
    
  PROCEDURE Setup();
  (* To be called before any other use of this module *)

  PROCEDURE ExtendEnv(binders: IdeList; env: Env): Env;
  PROCEDURE NewEnv(name: IdeName; rest: Env): Env;
  PROCEDURE BeEnv(env: Env; name: IdeName; rest: Env);
  PROCEDURE FreshDecoration(name: IdeName; env: Env): INTEGER;

  VAR 
    noName: IdeName;
    doCommandSet: ObCommand.Set;

  PROCEDURE SameIdeName(name1, name2: IdeName): BOOLEAN;

  PROCEDURE FmtBool(bool: BOOLEAN): TEXT;

  PROCEDURE FmtInt(int: INTEGER): TEXT;

  PROCEDURE FmtReal(real: LONGREAL): TEXT;

END ObTree.
