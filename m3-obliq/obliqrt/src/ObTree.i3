(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(*                                                                           *)
(* Parts Copyright (C) 1997, Columbia University                             *)
(* All rights reserved.                                                      *)
(*
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Mon Aug  4 14:55:17 1997
 *)

INTERFACE ObTree;
IMPORT ObCommand, SynLocation, RegEx;

TYPE

  IdeName =
    SynLocation.Located BRANDED "ObTree.IdeName" OBJECT
      text: TEXT;
      variant: INTEGER;
    END;

  IdePlace = BRANDED "ObTree.IdePlace" OBJECT END;
  IdePlaceLocal = IdePlace BRANDED "ObTree.IdePlaceLocal" OBJECT 
      index: INTEGER; (* > 0 *) 
    END;
  IdePlaceGlobal = IdePlace BRANDED "ObTree.IdePlaceGlobal" OBJECT 
      index: INTEGER; (* > 0 *)  
    END;
  
  Globals =
    SynLocation.Located BRANDED "ObTree.Globals" OBJECT
      name: IdeName;
      place: IdePlace;
      rest: Globals;
    END;

  Phrase =
    SynLocation.Located BRANDED "ObTree.Phrase" OBJECT END;

  PhraseCommand =
    Phrase BRANDED "ObTree.PhraseCommand" OBJECT
      set: ObCommand.Set;
      name, arg: TEXT;
    END; 

  PhraseTerm =
    Phrase BRANDED "ObTree.PhraseTerm" OBJECT
      term: Term; 
      printDepth: INTEGER;
    END; 

  TermBinding =
    SynLocation.Located BRANDED "ObTree.TermBinding" OBJECT
      binder: IdeName;
      term: Term;
      rest: TermBinding;
    END; 

  Term = 
    SynLocation.Located BRANDED "ObTree.Term" OBJECT END;

  TermConstant =
    Term BRANDED "ObTree.TermConstant" OBJECT
      cache: REFANY:=NIL;
    END;

  TermIde =
    Term BRANDED "ObTree.TermIde" OBJECT
      name: IdeName;
      place: IdePlace;
    END;

  TermOk =
    TermConstant BRANDED "ObTree.TermOk" OBJECT
    END;

  TermBool =
    TermConstant BRANDED "ObTree.TermBool" OBJECT
      bool: BOOLEAN;
    END;

  TermChar =
    TermConstant BRANDED "ObTree.TermChar" OBJECT
      char: CHAR;
    END;

  TermText =
    TermConstant BRANDED "ObTree.TermText" OBJECT
      text: TEXT;
    END;

  TermInt =
    TermConstant BRANDED "ObTree.TermInt" OBJECT
      int: INTEGER;
    END;

  TermReal =
    TermConstant BRANDED "ObTree.TermReal" OBJECT
      real: LONGREAL
    END;

  TermOption =
    Term BRANDED "ObTree.TermOption" OBJECT
      tag: Term;
      term: Term;
    END;

  TermOp =
    Term BRANDED "ObTree.TermOp" OBJECT
      pkg, op: IdeName;
      args: TermList;
      argsNo: INTEGER;
      temp: BOOLEAN; (* Is the result a temporary value? *)
      package: ROOT (* ObPkg.T *);
      opCode: ROOT (* ObPkg.OpCode *);
    END;

  TermFun =
    Term BRANDED "ObTree.TermFun" OBJECT
      binders: IdeList;
      bindersNo: INTEGER;
      body: Term;
      globals: Globals;
      globalsNo: INTEGER;
    END;

  TermAppl =
    Term BRANDED "ObTree.TermAppl" OBJECT
      fun: Term; 
      args: TermList;
      argsNo: INTEGER;
    END;

  TermMeth =
    Term BRANDED "ObTree.TermMeth" OBJECT
      binders: IdeList;
      bindersNo: INTEGER;
      body: Term;
      globals: Globals;
      globalsNo: INTEGER;
      update: BOOLEAN := FALSE;  (* will be set true if created as "umeth" *)
    END;

  TermAlias =
    Term BRANDED "ObTree.TermAlias" OBJECT
      label: IdeName;
      term: Term;
    END;

  TermLet =
    Term BRANDED "ObTree.TermLet" OBJECT
      semantics: SharingSemantics;
      var, rec: BOOLEAN;
      binding: TermBinding;
    END;

  TermArray =
    Term BRANDED "ObTree.TermArray" OBJECT
      semantics: SharingSemantics;
      elems: TermList;
      elemsNo: INTEGER;
    END;

  TermObj =
    Term BRANDED "ObTree.TermObj" OBJECT
      protected: BOOLEAN; 
      sync: Sync; (* NIL if not synchronized. *)
      semantics: SharingSemantics;
      fields: TermObjFields;
      fieldsNo: INTEGER;
    END;

  Sync = {None, Monitored, Reentrant};
  SharingSemantics = {Remote, Replicated, Simple};

  TermObjFields =
    SynLocation.Located BRANDED "ObTree.TermObjFields" OBJECT
      label: IdeName;
      term: Term;
      rest: TermObjFields;
    END;

  TermNotify =
    Term BRANDED "ObTree.TermNotify" OBJECT
      obj: Term;
      withObj: Term;
    END;

  TermPickler =
    Term BRANDED "ObTree.TermPickler" OBJECT
      obj: Term;
      pklIn: Term;
      pklOut: Term;
    END;

  TermClone =
    Term BRANDED "ObTree.TermClone" OBJECT
      objs: TermList;
      objsNo: INTEGER;
    END;

  TermReplicate =
    Term BRANDED "ObTree.TermReplicate" OBJECT
      args: TermList;
      argsNo: INTEGER;
    END;

  TermRemote =
    Term BRANDED "ObTree.TermRemote" OBJECT
      obj: Term;
    END;

  TermSimple =
    Term BRANDED "ObTree.TermSimple" OBJECT
      obj: Term;
    END;

  TermRedirect =
    Term BRANDED "ObTree.TermRedirect" OBJECT
      obj: Term;
      toObj: Term;
    END;

  TermSelect =
    Term BRANDED "ObTree.TermSelect" OBJECT
      obj: Term;
      label: IdeName;
      labelIndexHint: INTEGER;
      invoke: BOOLEAN;
      args: TermList;
      argsNo: INTEGER;
    END;

  TermUpdate =
    Term BRANDED "ObTree.TermUpdate" OBJECT
      obj: Term;
      label: IdeName;
      labelIndexHint: INTEGER;
      term: Term;
    END;

  TermSeq =
    Term BRANDED "ObTree.TermSeq" OBJECT
      before,after: Term;
    END;

  TermAssign =
    Term BRANDED "ObTree.TermAssign" OBJECT
      name: IdeName;
      place: IdePlace;
      val: Term;
    END;

  TermIf =
    Term BRANDED "ObTree.TermIf" OBJECT
      test,ifTrue: Term;
      ifFalse: Term; (* NIL if no else branch *)
    END;

  TermCase =
    Term BRANDED "ObTree.TermCase" OBJECT
      option: Term;
      caseList: TermCaseList;
    END;

  TermLoop =
    Term BRANDED "ObTree.TermLoop" OBJECT
      loop: Term;
    END;

  TermExit =
    Term BRANDED "ObTree.TermExit" OBJECT
    END;

  TermFor =
    Term BRANDED "ObTree.TermFor" OBJECT
      binder: IdeName;
      lb,ub,body: Term;
    END;

  TermForeach =
    Term BRANDED "ObTree.TermForeach" OBJECT
      binder: IdeName; 
      range, body: Term;
      map: BOOLEAN;
    END;

  TermException =
    Term BRANDED "ObTree.TermException" OBJECT
      name: Term;
    END;

  TermRaise =
    Term BRANDED "ObTree.TermRaise" OBJECT
      exception: Term;
    END;

  TermTry =
    Term BRANDED "ObTree.TermTry" OBJECT
      body: Term;
      tryList: TermTryList;
    END;

  TermTryFinally =
    Term BRANDED "ObTree.TermFinally" OBJECT
      body: Term;
      finally: Term;
    END;

  TermWatch =
    Term BRANDED "ObTree.TermWatch" OBJECT
      condition, guard: Term;
    END;

  TermCaseList =
    SynLocation.Located BRANDED "ObTree.TermCaseList" OBJECT
      pattern: Term;         (* NIL for "else" *)
      compiled: RegEx.Pattern; 
      binder: IdeName;    (* NIL for "else" or for no binder *)
      binderMatch: IdeName;    (* NIL if no binder the the regex matches *)
      body: Term;
      rest: TermCaseList; (* NIL for "else" *)
    END;

  TermTryList =
    SynLocation.Located BRANDED "ObTree.TermTryList" OBJECT
      exception: Term;    (* NIL for "else" *)
      recover: Term;
      rest: TermTryList;  (* NIL for "else" *)
    END;

  IdeList =
    SynLocation.Located BRANDED "ObTree.TermIdeList" OBJECT
      first: IdeName;
      rest: IdeList;
    END;

  TermList =
    SynLocation.Located BRANDED "ObTree.TermList" OBJECT
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
