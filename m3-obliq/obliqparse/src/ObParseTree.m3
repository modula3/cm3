(* Copyright 1991 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(*                                                                           *)
(* Parts Copyright (C) 1997, Columbia University                             *)
(* All rights reserved.                                                      *)
(*
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Mon Aug  4 15:04:23 1997
 *)

MODULE ObParseTree;
IMPORT SynLocation, SynScan, Text, SynParse, ObLib, ObTree, MetaParser;

PROCEDURE SelectText (p: SynParse.T; index: INTEGER): TEXT =
  BEGIN
    RETURN NARROW(p.stack[index], MetaParser.TextTemp).text;
  END SelectText;

PROCEDURE SelectInt (p: SynParse.T; index: INTEGER): INTEGER =
  BEGIN
    RETURN NARROW(p.stack[index], MetaParser.IntegerTemp).int;
  END SelectInt;

PROCEDURE SelectReal (p: SynParse.T; index: INTEGER): LONGREAL =
  BEGIN
    RETURN NARROW(p.stack[index], MetaParser.RealTemp).real;
  END SelectReal;

PROCEDURE Select1 (<*UNUSED*>          self: SynParse.Action;
                                       p   : SynParse.T;
                                       base: INTEGER;
                   <*UNUSED*> READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN p.stack[base + 1];
  END Select1;

PROCEDURE Select2 (<*UNUSED*>          self: SynParse.Action;
                                       p   : SynParse.T;
                                       base: INTEGER;
                   <*UNUSED*> READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN p.stack[base + 2];
  END Select2;

PROCEDURE Select3 (<*UNUSED*>          self: SynParse.Action;
                                       p   : SynParse.T;
                                       base: INTEGER;
                   <*UNUSED*> READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN p.stack[base + 3];
  END Select3;

PROCEDURE Select4 (<*UNUSED*>          self: SynParse.Action;
                                       p   : SynParse.T;
                                       base: INTEGER;
                   <*UNUSED*> READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN p.stack[base + 4];
  END Select4;

PROCEDURE Select5 (<*UNUSED*>          self: SynParse.Action;
                                       p   : SynParse.T;
                                       base: INTEGER;
                   <*UNUSED*> READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN p.stack[base + 5];
  END Select5;

PROCEDURE Select6 (<*UNUSED*>          self: SynParse.Action;
                                       p   : SynParse.T;
                                       base: INTEGER;
                   <*UNUSED*> READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN p.stack[base + 6];
  END Select6;

PROCEDURE Select7 (<*UNUSED*>          self: SynParse.Action;
                                       p   : SynParse.T;
                                       base: INTEGER;
                   <*UNUSED*> READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN p.stack[base + 7];
  END Select7;

PROCEDURE BuildIdeName (p: SynParse.T; index: INTEGER): ObTree.IdeName =
  BEGIN
    RETURN NEW(ObTree.IdeName, text := SelectText(p, index), variant := 0);
  END BuildIdeName;

PROCEDURE BuildPhraseEmpty (<*UNUSED*>          self: SynParse.Action;
                            <*UNUSED*>          p   : SynParse.T;
                            <*UNUSED*>          base: INTEGER;
                            <*UNUSED*> READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN NIL;
  END BuildPhraseEmpty;

PROCEDURE BuildPhraseFlag (<*UNUSED*>          self: SynParse.Action;
                                               p   : SynParse.T;
                                               base: INTEGER;
                                      READONLY info: SynLocation.Info ):
  SynParse.Tree =
  VAR name, arg: TEXT;
  BEGIN
    IF p.stack[base + 1] = NIL THEN
      name := "?";
      arg := "?";
    ELSE
      name := SelectText(p, base + 1);
      IF p.stack[base + 2] = NIL THEN
        arg := "?";
      ELSE
        arg := SelectText(p, base + 2);
      END;
    END;
    RETURN NEW(ObTree.PhraseCommand,
               location := SynLocation.NewLineLocation(info),
               set := ObTree.doCommandSet, name := name, arg := arg);
  END BuildPhraseFlag;

PROCEDURE BuildPhraseHelp (<*UNUSED*>          self: SynParse.Action;
                                               p   : SynParse.T;
                                               base: INTEGER;
                                      READONLY info: SynLocation.Info ):
  SynParse.Tree =
  VAR name, arg: TEXT;
  BEGIN
    IF p.stack[base + 1] = NIL THEN
      name := "?";
      arg := "?";
    ELSE
      name := SelectText(p, base + 1);
      IF p.stack[base + 2] = NIL THEN
        arg := "?";
      ELSE
        arg := SelectText(p, base + 2);
      END;
    END;
    RETURN NEW(ObTree.PhraseCommand,
               location := SynLocation.NewLineLocation(info),
               set := ObLib.helpCommandSet, name := name, arg := arg);
  END BuildPhraseHelp;

PROCEDURE BuildPhraseTerm (<*UNUSED*>          self: SynParse.Action;
                                               p   : SynParse.T;
                                               base: INTEGER;
                                      READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN
      NEW(ObTree.PhraseTerm, location := SynLocation.NewLineLocation(info),
          term := p.stack[base + 1], printDepth := -1);
  END BuildPhraseTerm;

PROCEDURE BuildPhraseTermDeep (<*UNUSED*>          self: SynParse.Action;
                                                   p   : SynParse.T;
                                                   base: INTEGER;
                                          READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN
      NEW(ObTree.PhraseTerm, location := SynLocation.NewLineLocation(info),
          term := p.stack[base + 1], printDepth := 40);
  END BuildPhraseTermDeep;

PROCEDURE BuildPhraseTermDepth (<*UNUSED*>          self: SynParse.Action;
                                                    p   : SynParse.T;
                                                    base: INTEGER;
                                           READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN
      NEW(ObTree.PhraseTerm, location := SynLocation.NewLineLocation(info),
          term := p.stack[base + 1], printDepth := SelectInt(p, base + 2));
  END BuildPhraseTermDepth;

PROCEDURE BuildTermBinding (<*UNUSED*>          self: SynParse.Action;
                                                p   : SynParse.T;
                                                base: INTEGER;
                                       READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN NEW(ObTree.TermBinding,
               location := SynLocation.NewLineLocation(info),
               binder := BuildIdeName(p, base + 1),
               term := p.stack[base + 2], rest := p.stack[base + 3]);
  END BuildTermBinding;

PROCEDURE BuildTermBindingSingle (<*UNUSED*> self: SynParse.Action;
                                             p   : SynParse.T;
                                             base: INTEGER;
                                  READONLY info: SynLocation.Info):
  SynParse.Tree =
  BEGIN
    RETURN NEW(ObTree.TermBinding,
               location := SynLocation.NewLineLocation(info),
               binder := BuildIdeName(p, base + 1),
               term := p.stack[base + 2], rest := NIL);
  END BuildTermBindingSingle;

PROCEDURE BuildTermBindingNil (<*UNUSED*>          self: SynParse.Action;
                               <*UNUSED*>          p   : SynParse.T;
                               <*UNUSED*>          base: INTEGER;
                               <*UNUSED*> READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN NIL;
  END BuildTermBindingNil;

PROCEDURE BuildTermIde (<*UNUSED*>          self: SynParse.Action;
                                            p   : SynParse.T;
                                            base: INTEGER;
                                   READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN
      NEW(ObTree.TermIde, location := SynLocation.NewLineLocation(info),
          name := BuildIdeName(p, base + 1), place := NIL);
  END BuildTermIde;

PROCEDURE BuildTermOk (<*UNUSED*>          self: SynParse.Action;
                       <*UNUSED*>          p   : SynParse.T;
                       <*UNUSED*>          base: INTEGER;
                                  READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN
      NEW(ObTree.TermOk, location := SynLocation.NewLineLocation(info));
  END BuildTermOk;

PROCEDURE BuildTermBoolTrue (<*UNUSED*>          self: SynParse.Action;
                             <*UNUSED*>          p   : SynParse.T;
                             <*UNUSED*>          base: INTEGER;
                                        READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN
      NEW(ObTree.TermBool, location := SynLocation.NewLineLocation(info),
          bool := TRUE);
  END BuildTermBoolTrue;

PROCEDURE BuildTermBoolFalse (<*UNUSED*>          self: SynParse.Action;
                              <*UNUSED*>          p   : SynParse.T;
                              <*UNUSED*>          base: INTEGER;
                                         READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN
      NEW(ObTree.TermBool, location := SynLocation.NewLineLocation(info),
          bool := FALSE);
  END BuildTermBoolFalse;

PROCEDURE BuildTermChar (<*UNUSED*>          self: SynParse.Action;
                                             p   : SynParse.T;
                                             base: INTEGER;
                                    READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN
      NEW(ObTree.TermChar, location := SynLocation.NewLineLocation(info),
          char := Text.GetChar(SelectText(p, base + 1), 0));
  END BuildTermChar;

PROCEDURE BuildTermText (<*UNUSED*>          self: SynParse.Action;
                                             p   : SynParse.T;
                                             base: INTEGER;
                                    READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN
      NEW(ObTree.TermText, location := SynLocation.NewLineLocation(info),
          text := SelectText(p, base + 1));
  END BuildTermText;

PROCEDURE BuildTermInt (<*UNUSED*>          self: SynParse.Action;
                                            p   : SynParse.T;
                                            base: INTEGER;
                                   READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN
      NEW(ObTree.TermInt, location := SynLocation.NewLineLocation(info),
          int := SelectInt(p, base + 1));
  END BuildTermInt;

PROCEDURE BuildTermReal (<*UNUSED*>          self: SynParse.Action;
                                             p   : SynParse.T;
                                             base: INTEGER;
                                    READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN
      NEW(ObTree.TermReal, location := SynLocation.NewLineLocation(info),
          real := SelectReal(p, base + 1));
  END BuildTermReal;

PROCEDURE BuildTermArray (<*UNUSED*>          self: SynParse.Action;
                                              p   : SynParse.T;
                                              base: INTEGER;
                                     READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN
      NEW(ObTree.TermArray, location := SynLocation.NewLineLocation(info),
          elems := p.stack[base + 1], 
          semantics := ObTree.SharingSemantics.Remote);
  END BuildTermArray;

PROCEDURE BuildTermSimpleArray (<*UNUSED*>          self: SynParse.Action;
                                              p   : SynParse.T;
                                              base: INTEGER;
                                     READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN
      NEW(ObTree.TermArray, location := SynLocation.NewLineLocation(info),
          elems := p.stack[base + 1], 
          semantics := ObTree.SharingSemantics.Simple);
  END BuildTermSimpleArray; 

PROCEDURE BuildTermRemoteArray (<*UNUSED*>          self: SynParse.Action;
                                              p   : SynParse.T;
                                              base: INTEGER;
                                     READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN
      NEW(ObTree.TermArray, location := SynLocation.NewLineLocation(info),
          elems := p.stack[base + 1], 
          semantics := ObTree.SharingSemantics.Remote);
  END BuildTermRemoteArray; 

PROCEDURE BuildTermReplicatedArray (<*UNUSED*>          self: SynParse.Action;
                                              p   : SynParse.T;
                                              base: INTEGER;
                                     READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN
      NEW(ObTree.TermArray, location := SynLocation.NewLineLocation(info),
          elems := p.stack[base + 1], 
          semantics := ObTree.SharingSemantics.Replicated);
  END BuildTermReplicatedArray; 

PROCEDURE BuildTermOption (<*UNUSED*>          self: SynParse.Action;
                                               p   : SynParse.T;
                                               base: INTEGER;
                                      READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN
      NEW(ObTree.TermOption, location := SynLocation.NewLineLocation(info),
          tag := p.stack[base + 1], term := p.stack[base + 2]);
  END BuildTermOption;

PROCEDURE BuildTermAlias (<*UNUSED*>          self: SynParse.Action;
                                              p   : SynParse.T;
                                              base: INTEGER;
                                     READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN
      NEW(ObTree.TermAlias, location := SynLocation.NewLineLocation(info),
          label := BuildIdeName(p, base + 1), term := p.stack[base + 2]);
  END BuildTermAlias;

PROCEDURE BuildTermOp (<*UNUSED*>          self: SynParse.Action;
                                           p   : SynParse.T;
                                           base: INTEGER;
                                  READONLY info: SynLocation.Info ):
  SynParse.Tree RAISES {SynParse.Fail} =
  VAR pkg: ObTree.IdeName;
  BEGIN
    TYPECASE p.stack[base + 1] OF
    | ObTree.TermIde (node) => pkg := node.name;
    ELSE
      SynScan.SyntaxMsg(p.Scanner(), "Identifier expected before '_'", "");
      RAISE SynParse.Fail;
    END;
    RETURN NEW(ObTree.TermOp,
               location := SynLocation.NewLineLocation(info), pkg := pkg,
               op := BuildIdeName(p, base + 2), args := p.stack[base + 3],
               (* the rest is setup in Scope.Term *)
               argsNo := 0, package := NIL, opCode := NIL);
  END BuildTermOp;

PROCEDURE BuildTermOpConst (<*UNUSED*>          self: SynParse.Action;
                                                p   : SynParse.T;
                                                base: INTEGER;
                                       READONLY info: SynLocation.Info ):
  SynParse.Tree RAISES {SynParse.Fail} =
  VAR pkg: ObTree.IdeName;
  BEGIN
    TYPECASE p.stack[base + 1] OF
    | ObTree.TermIde (node) => pkg := node.name;
    ELSE
      SynScan.SyntaxMsg(p.Scanner(), "Identifier expected before '_'", "");
      RAISE SynParse.Fail;
    END;
    RETURN NEW(ObTree.TermOp,
               location := SynLocation.NewLineLocation(info), pkg := pkg,
               op := BuildIdeName(p, base + 2), args := NIL, argsNo := -1,
               (* the rest is setup in Scope.Term *)
               package := NIL, opCode := NIL);
  END BuildTermOpConst;

PROCEDURE BuildIdeListNil (<*UNUSED*>          self: SynParse.Action;
                           <*UNUSED*>          p   : SynParse.T;
                           <*UNUSED*>          base: INTEGER;
                           <*UNUSED*> READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN NIL;
  END BuildIdeListNil;

PROCEDURE BuildIdeListSingle (<*UNUSED*>          self: SynParse.Action;
                                                  p   : SynParse.T;
                                                  base: INTEGER;
                                         READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN
      NEW(ObTree.IdeList, location := SynLocation.NewLineLocation(info),
          first := BuildIdeName(p, base + 1), rest := NIL);
  END BuildIdeListSingle;

PROCEDURE BuildIdeListCons (<*UNUSED*>          self: SynParse.Action;
                                                p   : SynParse.T;
                                                base: INTEGER;
                                       READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN
      NEW(ObTree.IdeList, location := SynLocation.NewLineLocation(info),
          first := BuildIdeName(p, base + 1), rest := p.stack[base + 2]);
  END BuildIdeListCons;

PROCEDURE BuildTermListNil (<*UNUSED*>          self: SynParse.Action;
                            <*UNUSED*>          p   : SynParse.T;
                            <*UNUSED*>          base: INTEGER;
                            <*UNUSED*> READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN NIL;
  END BuildTermListNil;

PROCEDURE BuildTermListSingle (<*UNUSED*>          self: SynParse.Action;
                                                   p   : SynParse.T;
                                                   base: INTEGER;
                                          READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN
      NEW(ObTree.TermList, location := SynLocation.NewLineLocation(info),
          first := p.stack[base + 1], rest := NIL);
  END BuildTermListSingle;

PROCEDURE BuildTermListCons (<*UNUSED*>          self: SynParse.Action;
                                                 p   : SynParse.T;
                                                 base: INTEGER;
                                        READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN
      NEW(ObTree.TermList, location := SynLocation.NewLineLocation(info),
          first := p.stack[base + 1], rest := p.stack[base + 2]);
  END BuildTermListCons;

PROCEDURE BuildTermProc (<*UNUSED*>          self: SynParse.Action;
                                             p   : SynParse.T;
                                             base: INTEGER;
                                    READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN
      NEW(ObTree.TermFun, location := SynLocation.NewLineLocation(info),
          binders := p.stack[base + 1], bindersNo := -1,
          body := p.stack[base + 2], globals := NIL, globalsNo := -1);
  END BuildTermProc;

PROCEDURE BuildTermAppl (<*UNUSED*>          self: SynParse.Action;
                                             p   : SynParse.T;
                                             base: INTEGER;
                                    READONLY info: SynLocation.Info ):
  SynParse.Tree =
  VAR
    fun    : ObTree.Term;
    args   : ObTree.TermList;
    loc    : SynLocation.T;
    pkgName: TEXT;
  BEGIN
    fun := p.stack[base + 1];
    args := p.stack[base + 2];
    loc := SynLocation.NewLineLocation(info);
    TYPECASE fun OF
    | ObTree.TermIde (ide) =>
        CASE ObLib.LookupFixity(ide.name.text, ObLib.libraries,
                                (*out*) pkgName) OF
        | ObLib.OpFixity.Undefined, ObLib.OpFixity.Qualified =>
            RETURN NEW(ObTree.TermAppl, location := loc, fun := fun,
                       args := args);
        | ObLib.OpFixity.Prefix, ObLib.OpFixity.Infix =>
            RETURN NEW(
                     ObTree.TermOp, location := loc,
                     pkg :=
                       NEW(ObTree.IdeName, text := pkgName, variant := 0),
                     op := ide.name, args := args, package := NIL,
                     opCode := NIL);
        END;
    ELSE
      RETURN
        NEW(ObTree.TermAppl, location := loc, fun := fun, args := args);
    END;
  END BuildTermAppl;

PROCEDURE BuildTermInfix (<*UNUSED*>          self: SynParse.Action;
                                              p   : SynParse.T;
                                              base: INTEGER;
                                     READONLY info: SynLocation.Info ):
  SynParse.Tree =
  VAR
    opName : ObTree.IdeName;
    pkgName: TEXT;
    args   : ObTree.TermList;
    loc    : SynLocation.T;
  BEGIN
    opName := BuildIdeName(p, base + 2);
    loc := SynLocation.NewLineLocation(info);
    args :=
      NEW(ObTree.TermList, location := loc, first := p.stack[base + 1],
          rest := NEW(ObTree.TermList, location := loc,
                      first := p.stack[base + 3], rest := NIL));
    CASE ObLib.LookupFixity(opName.text, ObLib.libraries, (*out*) pkgName) OF
    | ObLib.OpFixity.Infix,
        ObLib.OpFixity.Prefix (*will give an error leater*) =>
        RETURN
          NEW(ObTree.TermOp, location := loc,
              pkg := NEW(ObTree.IdeName, text := pkgName, variant := 0),
              op := opName, args := args, package := NIL, opCode := NIL);
    | ObLib.OpFixity.Undefined, ObLib.OpFixity.Qualified =>
        RETURN NEW(ObTree.TermAppl, location := loc,
                   fun := NEW(ObTree.TermIde, location := loc,
                              name := opName, place := NIL), args := args);
    END;
  END BuildTermInfix;

PROCEDURE BuildTermSeq (<*UNUSED*>          self: SynParse.Action;
                                            p   : SynParse.T;
                                            base: INTEGER;
                                   READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN
      NEW(ObTree.TermSeq, location := SynLocation.NewLineLocation(info),
          before := p.stack[base + 1], after := p.stack[base + 2]);
  END BuildTermSeq;

PROCEDURE BuildTermObj (<*UNUSED*>          self: SynParse.Action;
                                            p   : SynParse.T;
                                            base: INTEGER;
                                   READONLY info: SynLocation.Info ):
  SynParse.Tree =
  VAR
    protected : BOOLEAN := FALSE;
    serialized: ObTree.Sync := ObTree.Sync.None;
    semantics               := ObTree.SharingSemantics.Remote;
  BEGIN
    IF p.stack[base+1] # NIL THEN
      WITH opt = NARROW(p.stack[base+1], ObjOptionsTemp).options DO
        protected := ObjOption.Protected IN opt;
        IF ObjOption.Serialized IN opt THEN
          serialized := ObTree.Sync.Monitored;
        END;
        IF ObjOption.Replicated IN opt THEN
          semantics := ObTree.SharingSemantics.Replicated;
        ELSIF ObjOption.Simple IN opt THEN
          semantics := ObTree.SharingSemantics.Simple;
        END;
      END;
    END;
    RETURN
      NEW(ObTree.TermObj, location := SynLocation.NewLineLocation(info),
          protected := protected, sync := serialized,
          semantics := semantics, fields := p.stack[base + 2]);
  END BuildTermObj;

TYPE
  SemanticsTemp =
    SynParse.Tree BRANDED "ObParseTree.SemanticsTemp" OBJECT
    semantics: ObTree.SharingSemantics;  END; 

PROCEDURE BuildOptionReplicated (<*UNUSED*> self: SynParse.Action;
                                 <*UNUSED*> p   : SynParse.T;
                                 <*UNUSED*> base: INTEGER;
                                 <*UNUSED*> READONLY info: SynLocation.Info):
  SynParse.Tree =
  BEGIN
    RETURN NEW(SemanticsTemp, location := NIL, 
               semantics := ObTree.SharingSemantics.Replicated);
  END BuildOptionReplicated;

PROCEDURE BuildOptionRemote (<*UNUSED*>          self: SynParse.Action;
                             <*UNUSED*>          p   : SynParse.T;
                             <*UNUSED*>          base: INTEGER;
                             <*UNUSED*> READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN NEW(SemanticsTemp, location := NIL,
               semantics := ObTree.SharingSemantics.Remote);
  END BuildOptionRemote;

PROCEDURE BuildOptionSimple (<*UNUSED*>          self: SynParse.Action;
                             <*UNUSED*>          p   : SynParse.T;
                             <*UNUSED*>          base: INTEGER;
                             <*UNUSED*> READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN NEW(SemanticsTemp, location := NIL,
               semantics := ObTree.SharingSemantics.Simple);
  END BuildOptionSimple;

PROCEDURE BuildOptionNil (<*UNUSED*>          self: SynParse.Action;
                          <*UNUSED*>          p   : SynParse.T;
                          <*UNUSED*>          base: INTEGER;
                          <*UNUSED*> READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN NIL;
  END BuildOptionNil;

TYPE
  ObjOption = {Protected, Serialized, Simple, Replicated};
  ObjOptions = SET OF ObjOption;
  ObjOptionsTemp = SynParse.Tree BRANDED "ObParseTree.ObjOptionsTemp" OBJECT
    options: ObjOptions;
  END;

PROCEDURE BuildObjOptionReplicated (<*UNUSED*> self: SynParse.Action;
                                    <*UNUSED*> p   : SynParse.T;
                                    <*UNUSED*> base: INTEGER;
                                    <*UNUSED*> READONLY info:SynLocation.Info):
  SynParse.Tree =
  BEGIN
    RETURN NEW(ObjOptionsTemp, location := NIL, 
               options := ObjOptions{ObjOption.Replicated});
  END BuildObjOptionReplicated; 

PROCEDURE BuildObjOptionProtected (<*UNUSED*> self: SynParse.Action;
                                   <*UNUSED*> p   : SynParse.T;
                                   <*UNUSED*> base: INTEGER;
                                   <*UNUSED*> READONLY info:SynLocation.Info):
  SynParse.Tree =
  BEGIN
    RETURN NEW(ObjOptionsTemp, location := NIL, 
               options := ObjOptions{ObjOption.Protected});
  END BuildObjOptionProtected; 

PROCEDURE BuildObjOptionSerialized (<*UNUSED*> self: SynParse.Action;
                                    <*UNUSED*> p   : SynParse.T;
                                    <*UNUSED*> base: INTEGER;
                                    <*UNUSED*> READONLY info:SynLocation.Info):
  SynParse.Tree =
  BEGIN
    RETURN NEW(ObjOptionsTemp, location := NIL, 
               options := ObjOptions{ObjOption.Serialized});
  END BuildObjOptionSerialized; 

PROCEDURE BuildObjOptionSimple (<*UNUSED*> self: SynParse.Action;
                                <*UNUSED*> p   : SynParse.T;
                                <*UNUSED*> base: INTEGER;
                                <*UNUSED*> READONLY info:SynLocation.Info):
  SynParse.Tree =
  BEGIN
    RETURN NEW(ObjOptionsTemp, location := NIL, 
               options := ObjOptions{ObjOption.Simple});
  END BuildObjOptionSimple; 

PROCEDURE BuildObjOptionNil (<*UNUSED*>          self: SynParse.Action;
                             <*UNUSED*>          p   : SynParse.T;
                             <*UNUSED*>          base: INTEGER;
                             <*UNUSED*> READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN NEW(ObjOptionsTemp, location := NIL, options := ObjOptions{});
  END BuildObjOptionNil; 

PROCEDURE BuildObjOptionCons (<*UNUSED*>          self: SynParse.Action;
                                                  p   : SynParse.T;
                                                  base: INTEGER;
                              <*UNUSED*> READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    WITH opt1 = NARROW(p.stack[base + 1], ObjOptionsTemp).options,
         opt2 = NARROW(p.stack[base + 2], ObjOptionsTemp).options  DO
      RETURN NEW(ObjOptionsTemp, location := NIL, options := opt1 + opt2);
    END;
  END BuildObjOptionCons;

PROCEDURE BuildTermObjFieldNil (<*UNUSED*>          self: SynParse.Action;
                                <*UNUSED*>          p   : SynParse.T;
                                <*UNUSED*>          base: INTEGER;
                                <*UNUSED*> READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN NIL;
  END BuildTermObjFieldNil;

PROCEDURE BuildTermObjFieldSingle (<*UNUSED*> self: SynParse.Action;
                                              p   : SynParse.T;
                                              base: INTEGER;
                                   READONLY info: SynLocation.Info):
  SynParse.Tree =
  BEGIN
    RETURN NEW(ObTree.TermObjFields,
               location := SynLocation.NewLineLocation(info),
               label := BuildIdeName(p, base + 1),
               term := p.stack[base + 2], rest := NIL);
  END BuildTermObjFieldSingle;

PROCEDURE BuildTermObjField (<*UNUSED*>          self: SynParse.Action;
                                                 p   : SynParse.T;
                                                 base: INTEGER;
                                        READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN NEW(ObTree.TermObjFields,
               location := SynLocation.NewLineLocation(info),
               label := BuildIdeName(p, base + 1),
               term := p.stack[base + 2], rest := p.stack[base + 3]);
  END BuildTermObjField;

PROCEDURE BuildTermMeth (<*UNUSED*>          self: SynParse.Action;
                                             p   : SynParse.T;
                                             base: INTEGER;
                                    READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN
      NEW(ObTree.TermMeth, location := SynLocation.NewLineLocation(info),
          binders := p.stack[base + 1], bindersNo := -1,
          body := p.stack[base + 2], globals := NIL, globalsNo := -1);
  END BuildTermMeth;

PROCEDURE BuildTermUpdateMeth (<*UNUSED*>          self: SynParse.Action;
                                                   p   : SynParse.T;
                                                   base: INTEGER;
                                          READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN
      NEW(ObTree.TermMeth, location := SynLocation.NewLineLocation(info),
          binders := p.stack[base + 1], bindersNo := -1,
          body := p.stack[base + 2], globals := NIL, globalsNo := -1,
          update := TRUE);
  END BuildTermUpdateMeth;

PROCEDURE BuildTermClone (<*UNUSED*>          self: SynParse.Action;
                                              p   : SynParse.T;
                                              base: INTEGER;
                                     READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN
      NEW(ObTree.TermClone, location := SynLocation.NewLineLocation(info),
          objs := p.stack[base + 1]);
  END BuildTermClone;

PROCEDURE BuildTermNotify (<*UNUSED*>          self: SynParse.Action;
                                               p   : SynParse.T;
                                               base: INTEGER;
                                      READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN
      NEW(ObTree.TermNotify, location := SynLocation.NewLineLocation(info),
          obj := p.stack[base + 1], withObj := p.stack[base + 2]);
  END BuildTermNotify;

PROCEDURE BuildTermPickler (<*UNUSED*>          self: SynParse.Action;
                                                p   : SynParse.T;
                                                base: INTEGER;
                                       READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN NEW(ObTree.TermPickler,
               location := SynLocation.NewLineLocation(info),
               obj := p.stack[base + 1], pklIn := p.stack[base + 2],
               pklOut := p.stack[base + 3]);
  END BuildTermPickler;

PROCEDURE BuildTermReplicate (<*UNUSED*>          self: SynParse.Action;
                                                  p   : SynParse.T;
                                                  base: INTEGER;
                                         READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN NEW(ObTree.TermReplicate,
               location := SynLocation.NewLineLocation(info),
               args := p.stack[base + 1]);
  END BuildTermReplicate;

PROCEDURE BuildTermRemote (<*UNUSED*>          self: SynParse.Action;
                                               p   : SynParse.T;
                                               base: INTEGER;
                                      READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN
      NEW(ObTree.TermRemote, location := SynLocation.NewLineLocation(info),
          obj := p.stack[base + 1]);
  END BuildTermRemote;

PROCEDURE BuildTermSimple (<*UNUSED*>          self: SynParse.Action;
                                               p   : SynParse.T;
                                               base: INTEGER;
                                      READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN
      NEW(ObTree.TermSimple, location := SynLocation.NewLineLocation(info),
          obj := p.stack[base + 1]);
  END BuildTermSimple;

PROCEDURE BuildTermRedirect (<*UNUSED*>          self: SynParse.Action;
                                                 p   : SynParse.T;
                                                 base: INTEGER;
                                        READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN NEW(ObTree.TermRedirect,
               location := SynLocation.NewLineLocation(info),
               obj := p.stack[base + 1], toObj := p.stack[base + 2]);
  END BuildTermRedirect;

PROCEDURE BuildTermSelect (<*UNUSED*>          self: SynParse.Action;
                                               p   : SynParse.T;
                                               base: INTEGER;
                                      READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN
      NEW(ObTree.TermSelect, location := SynLocation.NewLineLocation(info),
          obj := p.stack[base + 1], label := BuildIdeName(p, base + 2),
          labelIndexHint := -1, invoke := FALSE, argsNo := 0, args := NIL);
  END BuildTermSelect;

PROCEDURE BuildTermInvoke (<*UNUSED*>          self: SynParse.Action;
                                               p   : SynParse.T;
                                               base: INTEGER;
                                      READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN
      NEW(ObTree.TermSelect, location := SynLocation.NewLineLocation(info),
          obj := p.stack[base + 1], label := BuildIdeName(p, base + 2),
          labelIndexHint := -1, invoke := TRUE, argsNo := 0,
          args := p.stack[base + 3]);
  END BuildTermInvoke;

PROCEDURE BuildTermUpdate (<*UNUSED*>          self: SynParse.Action;
                                               p   : SynParse.T;
                                               base: INTEGER;
                                      READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN
      NEW(ObTree.TermUpdate, location := SynLocation.NewLineLocation(info),
          obj := p.stack[base + 1], label := BuildIdeName(p, base + 2),
          labelIndexHint := -1, term := p.stack[base + 3]);
  END BuildTermUpdate;

PROCEDURE BuildTermArrayGet (<*UNUSED*>          self: SynParse.Action;
                                                 p   : SynParse.T;
                                                 base: INTEGER;
                                        READONLY info: SynLocation.Info ):
  SynParse.Tree =
  VAR loc: SynLocation.T;
  BEGIN
    loc := SynLocation.NewLineLocation(info);
    RETURN NEW(ObTree.TermOp, location := loc,
               pkg := NEW(ObTree.IdeName, text := "array", variant := 0),
               op := NEW(ObTree.IdeName, text := "get", variant := 0),
               args :=
                 NEW(ObTree.TermList, location := loc,
                     first := p.stack[base + 1],
                     rest := NEW(ObTree.TermList, location := loc,
                                 first := p.stack[base + 2], rest := NIL)),
               (* the rest is setup in Scope.Term *)
               package := NIL, opCode := NIL);
  END BuildTermArrayGet;

PROCEDURE BuildTermArraySub (<*UNUSED*>          self: SynParse.Action;
                                                 p   : SynParse.T;
                                                 base: INTEGER;
                                        READONLY info: SynLocation.Info ):
  SynParse.Tree =
  VAR loc: SynLocation.T;
  BEGIN
    loc := SynLocation.NewLineLocation(info);
    RETURN
      NEW(
        ObTree.TermOp, location := loc,
        pkg := NEW(ObTree.IdeName, text := "array", variant := 0),
        op := NEW(ObTree.IdeName, text := "sub", variant := 0),
        args :=
          NEW(ObTree.TermList, location := loc, first := p.stack[base + 1],
              rest :=
                NEW(ObTree.TermList, location := loc,
                    first := p.stack[base + 2],
                    rest := NEW(ObTree.TermList, location := loc,
                                first := p.stack[base + 3], rest := NIL))),
        (* the rest is setup in Scope.Term *)
        package := NIL, opCode := NIL);
  END BuildTermArraySub;

PROCEDURE BuildTermArraySet (<*UNUSED*>          self: SynParse.Action;
                                                 p   : SynParse.T;
                                                 base: INTEGER;
                                        READONLY info: SynLocation.Info ):
  SynParse.Tree =
  VAR loc: SynLocation.T;
  BEGIN
    loc := SynLocation.NewLineLocation(info);
    RETURN
      NEW(
        ObTree.TermOp, location := loc,
        pkg := NEW(ObTree.IdeName, text := "array", variant := 0),
        op := NEW(ObTree.IdeName, text := "set", variant := 0),
        args :=
          NEW(ObTree.TermList, location := loc, first := p.stack[base + 1],
              rest :=
                NEW(ObTree.TermList, location := loc,
                    first := p.stack[base + 2],
                    rest := NEW(ObTree.TermList, location := loc,
                                first := p.stack[base + 3], rest := NIL))),
        (* the rest is setup in Scope.Term *)
        package := NIL, opCode := NIL);
  END BuildTermArraySet;

PROCEDURE BuildTermArrayUpd (<*UNUSED*>          self: SynParse.Action;
                                                 p   : SynParse.T;
                                                 base: INTEGER;
                                        READONLY info: SynLocation.Info ):
  SynParse.Tree =
  VAR loc: SynLocation.T;
  BEGIN
    loc := SynLocation.NewLineLocation(info);
    RETURN
      NEW(
        ObTree.TermOp, location := loc,
        pkg := NEW(ObTree.IdeName, text := "array", variant := 0),
        op := NEW(ObTree.IdeName, text := "upd", variant := 0),
        args :=
          NEW(
            ObTree.TermList, location := loc, first := p.stack[base + 1],
            rest :=
              NEW(ObTree.TermList, location := loc,
                  first := p.stack[base + 2],
                  rest := NEW(ObTree.TermList, location := loc,
                              first := p.stack[base + 3],
                              rest := NEW(ObTree.TermList, location := loc,
                                          first := p.stack[base + 4],
                                          rest := NIL)))),
        (* the rest is setup in Scope.Term *)
        package := NIL, opCode := NIL);
  END BuildTermArrayUpd;

PROCEDURE BuildTermMinus (<*UNUSED*>          self: SynParse.Action;
                                              p   : SynParse.T;
                                              base: INTEGER;
                                     READONLY info: SynLocation.Info ):
  SynParse.Tree =
  VAR loc: SynLocation.T;
  BEGIN
    loc := SynLocation.NewLineLocation(info);
    RETURN NEW(ObTree.TermOp, location := loc,
               pkg := NEW(ObTree.IdeName, text := "real", variant := 0),
               op := NEW(ObTree.IdeName, text := "minus", variant := 0),
               args := NEW(ObTree.TermList, location := loc,
                           first := p.stack[base + 1], rest := NIL),
               (* the rest is setup in Scope.Term *)
               package := NIL, opCode := NIL);
  END BuildTermMinus;

PROCEDURE BuildTermLet (<*UNUSED*>          self: SynParse.Action;
                                            p   : SynParse.T;
                                            base: INTEGER;
                                   READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN NEW(ObTree.TermLet,
               location := SynLocation.NewLineLocation(info), var := FALSE,
               rec := FALSE, semantics := ObTree.SharingSemantics.Remote,
               binding := p.stack[base + 1]);
  END BuildTermLet;

PROCEDURE BuildTermVar (<*UNUSED*>          self: SynParse.Action;
                                            p   : SynParse.T;
                                            base: INTEGER;
                                   READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    WITH opt = NARROW(p.stack[base+1],SemanticsTemp).semantics DO
      RETURN NEW(ObTree.TermLet,
               location := SynLocation.NewLineLocation(info), var := TRUE,
               rec := FALSE, semantics := opt,
               binding := p.stack[base + 2]);
    END;
  END BuildTermVar;

PROCEDURE BuildTermLetRec (<*UNUSED*>          self: SynParse.Action;
                                               p   : SynParse.T;
                                               base: INTEGER;
                                      READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN NEW(ObTree.TermLet,
               location := SynLocation.NewLineLocation(info), var := FALSE,
               rec := TRUE, semantics := ObTree.SharingSemantics.Remote,
               binding := p.stack[base + 1]);
  END BuildTermLetRec;

PROCEDURE BuildTermVarRec (<*UNUSED*>          self: SynParse.Action;
                                               p   : SynParse.T;
                                               base: INTEGER;
                                      READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    WITH opt = NARROW(p.stack[base+1],SemanticsTemp).semantics DO
      RETURN NEW(ObTree.TermLet,
               location := SynLocation.NewLineLocation(info), var := TRUE,
               rec := TRUE, semantics := opt,
               binding := p.stack[base + 2]);
    END;
  END BuildTermVarRec;

PROCEDURE BuildTermAssign (<*UNUSED*>          self: SynParse.Action;
                                               p   : SynParse.T;
                                               base: INTEGER;
                                      READONLY info: SynLocation.Info ):
  SynParse.Tree RAISES {SynParse.Fail} =
  VAR name: ObTree.IdeName;
  BEGIN
    TYPECASE p.stack[base + 1] OF
    | ObTree.TermIde (node) => name := node.name;
    ELSE
      SynScan.SyntaxMsg(p.Scanner(), "Identifier expected before ':='", "");
      RAISE SynParse.Fail;
    END;
    RETURN
      NEW(ObTree.TermAssign, location := SynLocation.NewLineLocation(info),
          name := name, place := NIL, val := p.stack[base + 2]);
  END BuildTermAssign;

PROCEDURE BuildTermIf (<*UNUSED*>          self: SynParse.Action;
                                           p   : SynParse.T;
                                           base: INTEGER;
                                  READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN
      NEW(ObTree.TermIf, location := SynLocation.NewLineLocation(info),
          test := p.stack[base + 1], ifTrue := p.stack[base + 2],
          ifFalse := p.stack[base + 3]);
  END BuildTermIf;

PROCEDURE BuildTermIfEnd (<*UNUSED*>          self: SynParse.Action;
                          <*UNUSED*>          p   : SynParse.T;
                          <*UNUSED*>          base: INTEGER;
                          <*UNUSED*> READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN NIL;
  END BuildTermIfEnd;

PROCEDURE BuildTermAndif (<*UNUSED*>          self: SynParse.Action;
                                              p   : SynParse.T;
                                              base: INTEGER;
                                     READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN
      NEW(ObTree.TermIf, location := SynLocation.NewLineLocation(info),
          test := p.stack[base + 1], ifTrue := p.stack[base + 2],
          ifFalse := NEW(ObTree.TermBool,
                         location := SynLocation.NewLineLocation(info),
                         bool := FALSE));
  END BuildTermAndif;

PROCEDURE BuildTermOrif (<*UNUSED*>          self: SynParse.Action;
                                             p   : SynParse.T;
                                             base: INTEGER;
                                    READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN
      NEW(ObTree.TermIf, location := SynLocation.NewLineLocation(info),
          test := p.stack[base + 1],
          ifTrue := NEW(ObTree.TermBool,
                        location := SynLocation.NewLineLocation(info),
                        bool := TRUE), ifFalse := p.stack[base + 2]);
  END BuildTermOrif;

PROCEDURE BuildTermCase (<*UNUSED*>          self: SynParse.Action;
                                             p   : SynParse.T;
                                             base: INTEGER;
                                    READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN
      NEW(ObTree.TermCase, location := SynLocation.NewLineLocation(info),
          option := p.stack[base + 1], caseList := p.stack[base + 2]);
  END BuildTermCase; 

PROCEDURE BuildTermLoop (<*UNUSED*>          self: SynParse.Action;
                                             p   : SynParse.T;
                                             base: INTEGER;
                                    READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN
      NEW(ObTree.TermLoop, location := SynLocation.NewLineLocation(info),
          loop := p.stack[base + 1]);
  END BuildTermLoop;

PROCEDURE BuildTermExit (<*UNUSED*>          self: SynParse.Action;
                         <*UNUSED*>          p   : SynParse.T;
                         <*UNUSED*>          base: INTEGER;
                                    READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN
      NEW(ObTree.TermExit, location := SynLocation.NewLineLocation(info));
  END BuildTermExit;

PROCEDURE BuildTermFor (<*UNUSED*>          self: SynParse.Action;
                                            p   : SynParse.T;
                                            base: INTEGER;
                                   READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN
      NEW(ObTree.TermFor, location := SynLocation.NewLineLocation(info),
          binder := BuildIdeName(p, base + 1), lb := p.stack[base + 2],
          ub := p.stack[base + 3], body := p.stack[base + 4]);
  END BuildTermFor;

PROCEDURE BuildTermForeachDo (<*UNUSED*>          self: SynParse.Action;
                                                  p   : SynParse.T;
                                                  base: INTEGER;
                                         READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN
      NEW(
        ObTree.TermForeach, location := SynLocation.NewLineLocation(info),
        binder := BuildIdeName(p, base + 1), range := p.stack[base + 2],
        body := p.stack[base + 3], map := FALSE);
  END BuildTermForeachDo;

PROCEDURE BuildTermForeachMap (<*UNUSED*>          self: SynParse.Action;
                                                   p   : SynParse.T;
                                                   base: INTEGER;
                                          READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN
      NEW(
        ObTree.TermForeach, location := SynLocation.NewLineLocation(info),
        binder := BuildIdeName(p, base + 1), range := p.stack[base + 2],
        body := p.stack[base + 3], map := TRUE);
  END BuildTermForeachMap;

PROCEDURE BuildTermException (<*UNUSED*>          self: SynParse.Action;
                                                  p   : SynParse.T;
                                                  base: INTEGER;
                                         READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN NEW(ObTree.TermException,
               location := SynLocation.NewLineLocation(info),
               name := p.stack[base + 1]);
  END BuildTermException;

PROCEDURE BuildTermRaise (<*UNUSED*>          self: SynParse.Action;
                                              p   : SynParse.T;
                                              base: INTEGER;
                                     READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN
      NEW(ObTree.TermRaise, location := SynLocation.NewLineLocation(info),
          exception := p.stack[base + 1]);
  END BuildTermRaise;

PROCEDURE BuildTermTry (<*UNUSED*>          self: SynParse.Action;
                                            p   : SynParse.T;
                                            base: INTEGER;
                                   READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN
      NEW(ObTree.TermTry, location := SynLocation.NewLineLocation(info),
          body := p.stack[base + 1], tryList := p.stack[base + 2]);
  END BuildTermTry;

PROCEDURE BuildTermTryElse (<*UNUSED*>          self: SynParse.Action;
                                                p   : SynParse.T;
                                                base: INTEGER;
                                       READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN
      NEW(ObTree.TermTry, location := SynLocation.NewLineLocation(info),
          body := p.stack[base + 1],
          tryList := NEW(ObTree.TermTryList,
                         location := SynLocation.NewLineLocation(info),
                         exception := NIL, recover := p.stack[base + 2],
                         rest := NIL));
  END BuildTermTryElse;

PROCEDURE BuildTermTryFinally (<*UNUSED*>          self: SynParse.Action;
                                                   p   : SynParse.T;
                                                   base: INTEGER;
                                          READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN NEW(ObTree.TermTryFinally,
               location := SynLocation.NewLineLocation(info),
               body := p.stack[base + 1], finally := p.stack[base + 2]);
  END BuildTermTryFinally;

PROCEDURE BuildCaseListCons (<*UNUSED*>          self: SynParse.Action;
                                                 p   : SynParse.T;
                                                 base: INTEGER;
                                        READONLY info: SynLocation.Info ):
  SynParse.Tree =
  VAR bind, case: ObTree.IdeName;
  BEGIN
    IF p.stack[base + 2] = NIL THEN
      bind := NIL;
    ELSE
      bind := BuildIdeName(p, base + 2);
    END;
    IF p.stack[base + 3] = NIL THEN
      case := NIL;
    ELSE
      case := BuildIdeName(p, base + 3);
    END;
    RETURN NEW(ObTree.TermCaseList,
               location := SynLocation.NewLineLocation(info),
               pattern := p.stack[base+1], compiled := NIL,
               binder := bind, binderMatch := case,
               body := p.stack[base + 4], rest := p.stack[base + 5]);
  END BuildCaseListCons; 

PROCEDURE BuildCaseListElse (<*UNUSED*>          self: SynParse.Action;
                                                 p   : SynParse.T;
                                                 base: INTEGER;
                                        READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN NEW(ObTree.TermCaseList,
               location := SynLocation.NewLineLocation(info),
               pattern := NIL, compiled := NIL, binderMatch := NIL,
               binder := NIL, body := p.stack[base + 1], rest := NIL);
  END BuildCaseListElse;

PROCEDURE BuildCaseListNil (<*UNUSED*>          self: SynParse.Action;
                            <*UNUSED*>          p   : SynParse.T;
                            <*UNUSED*>          base: INTEGER;
                            <*UNUSED*> READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN NIL;
  END BuildCaseListNil;

PROCEDURE BuildTryListCons (<*UNUSED*>          self: SynParse.Action;
                                                p   : SynParse.T;
                                                base: INTEGER;
                                       READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN NEW(ObTree.TermTryList,
               location := SynLocation.NewLineLocation(info),
               exception := p.stack[base + 1],
               recover := p.stack[base + 2], rest := p.stack[base + 3]);
  END BuildTryListCons;

PROCEDURE BuildTryListConsElse (<*UNUSED*>          self: SynParse.Action;
                                                    p   : SynParse.T;
                                                    base: INTEGER;
                                           READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN NEW(
             ObTree.TermTryList,
             location := SynLocation.NewLineLocation(info),
             exception := p.stack[base + 1], recover := p.stack[base + 2],
             rest := NEW(ObTree.TermTryList,
                         location := SynLocation.NewLineLocation(info),
                         exception := NIL, recover := p.stack[base + 3],
                         rest := NIL));
  END BuildTryListConsElse;

PROCEDURE BuildTryListSingle (<*UNUSED*>          self: SynParse.Action;
                                                  p   : SynParse.T;
                                                  base: INTEGER;
                                         READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN NEW(ObTree.TermTryList,
               location := SynLocation.NewLineLocation(info),
               exception := p.stack[base + 1],
               recover := p.stack[base + 2], rest := NIL);
  END BuildTryListSingle;

PROCEDURE BuildTryListElse (<*UNUSED*>          self: SynParse.Action;
                                                p   : SynParse.T;
                                                base: INTEGER;
                                       READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN
      NEW(
        ObTree.TermTryList, location := SynLocation.NewLineLocation(info),
        exception := NIL, recover := p.stack[base + 1], rest := NIL);
  END BuildTryListElse;

PROCEDURE BuildTryListNil (<*UNUSED*>          self: SynParse.Action;
                           <*UNUSED*>          p   : SynParse.T;
                           <*UNUSED*>          base: INTEGER;
                           <*UNUSED*> READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN NIL;
  END BuildTryListNil;

PROCEDURE BuildTermLock (<*UNUSED*>          self: SynParse.Action;
                                             p   : SynParse.T;
                                             base: INTEGER;
                                    READONLY info: SynLocation.Info ):
  SynParse.Tree =
  VAR loc: SynLocation.T;
  BEGIN
    loc := SynLocation.NewLineLocation(info);
    RETURN
      NEW(
        ObTree.TermOp, location := loc,
        pkg := NEW(ObTree.IdeName, text := "thread", variant := 0),
        op := NEW(ObTree.IdeName, text := "lock", variant := 0),
        args :=
          NEW(ObTree.TermList, location := loc, first := p.stack[base + 1],
              rest :=
                NEW(ObTree.TermList, location := loc,
                    first :=
                      NEW(ObTree.TermFun, location := loc, binders := NIL,
                          bindersNo := -1, body := p.stack[base + 2],
                          globals := NIL, globalsNo := -1), rest := NIL)),
        (* the rest is setup in Scope.Term *)
        package := NIL, opCode := NIL);
  END BuildTermLock;

PROCEDURE BuildTermWatch (<*UNUSED*>          self: SynParse.Action;
                                              p   : SynParse.T;
                                              base: INTEGER;
                                     READONLY info: SynLocation.Info ):
  SynParse.Tree =
  BEGIN
    RETURN
      NEW(ObTree.TermWatch, location := SynLocation.NewLineLocation(info),
          condition := p.stack[base + 1], guard := p.stack[base + 2]);
  END BuildTermWatch;

PROCEDURE RegisterActions (actions: MetaParser.ActionTable) =
  BEGIN
    MetaParser.Register("Select1", Select1, actions);
    MetaParser.Register("Select2", Select2, actions);
    MetaParser.Register("Select3", Select3, actions);
    MetaParser.Register("Select4", Select4, actions);
    MetaParser.Register("Select5", Select5, actions);
    MetaParser.Register("Select6", Select6, actions);
    MetaParser.Register("Select7", Select7, actions);
    MetaParser.Register("BuildPhraseEmpty", BuildPhraseEmpty, actions);
    MetaParser.Register("BuildPhraseFlag", BuildPhraseFlag, actions);
    MetaParser.Register("BuildPhraseHelp", BuildPhraseHelp, actions);
    MetaParser.Register("BuildPhraseTerm", BuildPhraseTerm, actions);
    MetaParser.Register(
      "BuildPhraseTermDepth", BuildPhraseTermDepth, actions);
    MetaParser.Register(
      "BuildPhraseTermDeep", BuildPhraseTermDeep, actions);
    MetaParser.Register("BuildTermBinding", BuildTermBinding, actions);
    MetaParser.Register(
      "BuildTermBindingSingle", BuildTermBindingSingle, actions);
    MetaParser.Register(
      "BuildTermBindingNil", BuildTermBindingNil, actions);
    MetaParser.Register("BuildTermIde", BuildTermIde, actions);
    MetaParser.Register("BuildTermOk", BuildTermOk, actions);
    MetaParser.Register("BuildTermBoolTrue", BuildTermBoolTrue, actions);
    MetaParser.Register("BuildTermBoolFalse", BuildTermBoolFalse, actions);
    MetaParser.Register("BuildTermChar", BuildTermChar, actions);
    MetaParser.Register("BuildTermString", BuildTermText, actions);
    MetaParser.Register("BuildTermInt", BuildTermInt, actions);
    MetaParser.Register("BuildTermReal", BuildTermReal, actions);
    MetaParser.Register("BuildTermArray", BuildTermArray, actions);
    MetaParser.Register("BuildTermSimpleArray", BuildTermSimpleArray, actions);
    MetaParser.Register("BuildTermRemoteArray", BuildTermRemoteArray, actions);
    MetaParser.Register("BuildTermReplicatedArray",
                        BuildTermReplicatedArray, actions);
    MetaParser.Register("BuildTermOption", BuildTermOption, actions);
    MetaParser.Register("BuildTermAlias", BuildTermAlias, actions);
    MetaParser.Register("BuildTermOp", BuildTermOp, actions);
    MetaParser.Register("BuildTermOpConst", BuildTermOpConst, actions);
    MetaParser.Register("BuildTermAppl", BuildTermAppl, actions);
    MetaParser.Register("BuildTermInfix", BuildTermInfix, actions);
    MetaParser.Register("BuildTermSeq", BuildTermSeq, actions);
    MetaParser.Register("BuildTermLet", BuildTermLet, actions);
    MetaParser.Register("BuildTermVar", BuildTermVar, actions);
    MetaParser.Register("BuildTermLetRec", BuildTermLetRec, actions);
    MetaParser.Register("BuildTermVarRec", BuildTermVarRec, actions);
    MetaParser.Register(
      "BuildOptionReplicated", BuildOptionReplicated, actions);
    MetaParser.Register("BuildOptionRemote", BuildOptionRemote, actions);
    MetaParser.Register("BuildOptionSimple", BuildOptionSimple, actions);
    MetaParser.Register("BuildOptionNil", BuildOptionNil, actions);
    MetaParser.Register("BuildTermAssign", BuildTermAssign, actions);
    MetaParser.Register("BuildTermIf", BuildTermIf, actions);
    MetaParser.Register("BuildTermIfEnd", BuildTermIfEnd, actions);
    MetaParser.Register("BuildTermAndif", BuildTermAndif, actions);
    MetaParser.Register("BuildTermOrif", BuildTermOrif, actions);
    MetaParser.Register("BuildTermCase", BuildTermCase, actions);
    MetaParser.Register("BuildTermUpdate", BuildTermUpdate, actions);
    MetaParser.Register("BuildTermSelect", BuildTermSelect, actions);
    MetaParser.Register("BuildTermInvoke", BuildTermInvoke, actions);
    MetaParser.Register("BuildTermArrayGet", BuildTermArrayGet, actions);
    MetaParser.Register("BuildTermArraySet", BuildTermArraySet, actions);
    MetaParser.Register("BuildTermArraySub", BuildTermArraySub, actions);
    MetaParser.Register("BuildTermArrayUpd", BuildTermArrayUpd, actions);
    MetaParser.Register("BuildTermMinus", BuildTermMinus, actions);
    MetaParser.Register("BuildTermObj", BuildTermObj, actions);
    MetaParser.Register("BuildObjOptionCons", BuildObjOptionCons, actions);
    MetaParser.Register("BuildObjOptionNil", BuildObjOptionNil, actions);
    MetaParser.Register("BuildObjOptionProtected", 
                        BuildObjOptionProtected, actions);
    MetaParser.Register("BuildObjOptionSerialized", 
                        BuildObjOptionSerialized, actions);
    MetaParser.Register("BuildObjOptionSimple", 
                        BuildObjOptionSimple, actions);
    MetaParser.Register("BuildObjOptionReplicated", 
                        BuildObjOptionReplicated, actions);
    MetaParser.Register("BuildTermObjField", BuildTermObjField, actions);
    MetaParser.Register(
      "BuildTermObjFieldSingle", BuildTermObjFieldSingle, actions);
    MetaParser.Register(
      "BuildTermObjFieldNil", BuildTermObjFieldNil, actions);
    MetaParser.Register("BuildTermClone", BuildTermClone, actions);
    MetaParser.Register("BuildTermNotify", BuildTermNotify, actions);
    MetaParser.Register("BuildTermPickler", BuildTermPickler, actions);
    MetaParser.Register("BuildTermReplicate", BuildTermReplicate, actions);
    MetaParser.Register("BuildTermSimple", BuildTermSimple, actions);
    MetaParser.Register("BuildTermRemote", BuildTermRemote, actions);
    MetaParser.Register("BuildTermRedirect", BuildTermRedirect, actions);
    MetaParser.Register("BuildTermProc", BuildTermProc, actions);
    MetaParser.Register("BuildTermMeth", BuildTermMeth, actions);
    MetaParser.Register(
      "BuildTermUpdateMeth", BuildTermUpdateMeth, actions);
    MetaParser.Register("BuildTermLoop", BuildTermLoop, actions);
    MetaParser.Register("BuildTermExit", BuildTermExit, actions);
    MetaParser.Register("BuildTermFor", BuildTermFor, actions);
    MetaParser.Register("BuildTermForeachDo", BuildTermForeachDo, actions);
    MetaParser.Register(
      "BuildTermForeachMap", BuildTermForeachMap, actions);
    MetaParser.Register("BuildTermException", BuildTermException, actions);
    MetaParser.Register("BuildTermRaise", BuildTermRaise, actions);
    MetaParser.Register("BuildTermTry", BuildTermTry, actions);
    MetaParser.Register("BuildTermTryElse", BuildTermTryElse, actions);
    MetaParser.Register(
      "BuildTermTryFinally", BuildTermTryFinally, actions);
    MetaParser.Register("BuildTermLock", BuildTermLock, actions);
    MetaParser.Register("BuildTermWatch", BuildTermWatch, actions);
    MetaParser.Register("BuildCaseListCons", BuildCaseListCons, actions);
    MetaParser.Register("BuildCaseListElse", BuildCaseListElse, actions);
    MetaParser.Register("BuildCaseListNil", BuildCaseListNil, actions);
    MetaParser.Register("BuildTryListCons", BuildTryListCons, actions);
    MetaParser.Register(
      "BuildTryListConsElse", BuildTryListConsElse, actions);
    MetaParser.Register("BuildTryListSingle", BuildTryListSingle, actions);
    MetaParser.Register("BuildTryListElse", BuildTryListElse, actions);
    MetaParser.Register("BuildTryListNil", BuildTryListNil, actions);
    MetaParser.Register("BuildIdeListNil", BuildIdeListNil, actions);
    MetaParser.Register("BuildIdeListSingle", BuildIdeListSingle, actions);
    MetaParser.Register("BuildIdeListCons", BuildIdeListCons, actions);
    MetaParser.Register("BuildTermListNil", BuildTermListNil, actions);
    MetaParser.Register(
      "BuildTermListSingle", BuildTermListSingle, actions);
    MetaParser.Register("BuildTermListCons", BuildTermListCons, actions);
  END RegisterActions;

PROCEDURE Setup () =
  BEGIN
  END Setup;

BEGIN
END ObParseTree.
