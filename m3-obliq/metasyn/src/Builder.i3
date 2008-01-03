(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* Last modified on Tue Jan 25 15:03:42 1994 by luca               *)
(*      modified on Tue Jun 23 20:16:54 1992 by knaff          *)

INTERFACE Builder;

(* The Builder Module contains only the Build Routines to construct a new 
   client grammar *)

IMPORT SynLocation, SynParse, MetaParser;

VAR actionTable: MetaParser.ActionTable ;

TYPE
  GramInfo <: GramInfoBase;
  (* revealed in Builder.m3 *)
  GramInfoBase = 
    SynParse.Tree BRANDED OBJECT
    topGram: SynParse.Grammar;
    env: SynParse.GrammarEnv;
    adoptAsTopLevelGrammar: BOOLEAN;
  END;

  Params =
    SynParse.Tree BRANDED OBJECT
      first: SynParse.Tree;
      rest: Params;
    END;

  ActionProc = PROCEDURE (self: SynParse.Action; 
                          p: SynParse.T; base: INTEGER;
                          READONLY info: SynLocation.Info): 
                          SynParse.Tree RAISES {SynParse.Fail} ;


PROCEDURE LinkIn(table : MetaParser.ActionTable ) ;
(* Links in the action table for Builder *)


PROCEDURE Ide(self: SynParse.Identifier; p: SynParse.T; name: TEXT;
              READONLY info: SynLocation.Info): SynParse.Tree ;
PROCEDURE Name(self: SynParse.Name; p: SynParse.T; name: TEXT;
              READONLY info: SynLocation.Info): SynParse.Tree;

PROCEDURE Grammar(self:SynParse.Action; p: SynParse.T; base: INTEGER;
                  READONLY info: SynLocation.Info): SynParse.Tree RAISES {SynParse.Fail} ;
PROCEDURE ClauseList(self: SynParse.Action; p: SynParse.T; base: INTEGER;
                     READONLY info: SynLocation.Info): SynParse.Tree 
    RAISES {SynParse.Fail} ;

PROCEDURE ClauseExtendsChoice(self: SynParse.Action; p: SynParse.T; base: INTEGER;
                              READONLY info: SynLocation.Info): SynParse.Tree;

PROCEDURE ClauseExtendsIterPos(self: SynParse.Action; p: SynParse.T; base: INTEGER;
                               READONLY info: SynLocation.Info): SynParse.Tree 
    RAISES {SynParse.Fail} ;

PROCEDURE ClauseExtendsIterNoPos(self: SynParse.Action; p: SynParse.T; base: INTEGER;
                                 READONLY info: SynLocation.Info): SynParse.Tree;

PROCEDURE ClauseExtendsIter(self: SynParse.Action; p: SynParse.T; base: INTEGER;
                            READONLY info: SynLocation.Info): SynParse.Tree;

PROCEDURE ClauseExtendsNo(self: SynParse.Action; p: SynParse.T; base: INTEGER;
                          READONLY info: SynLocation.Info): SynParse.Tree;

PROCEDURE ClauseExtendsYes(self: SynParse.Action; p: SynParse.T; base: INTEGER;
                           READONLY info: SynLocation.Info): SynParse.Tree;

PROCEDURE GramIde2(self: SynParse.Action; p: SynParse.T; base: INTEGER;
                   READONLY info: SynLocation.Info): SynParse.Tree RAISES {SynParse.Fail} ;

PROCEDURE GramString(self: SynParse.QuotedString; p: SynParse.T; string: TEXT;
                     READONLY info: SynLocation.Info): SynParse.Tree 
    RAISES {SynParse.Fail} ;

PROCEDURE GramKeyIde(self: SynParse.GivenKeyword; p: SynParse.T; 
                     READONLY info: SynLocation.Info): SynParse.Tree 
    RAISES {SynParse.Fail} ;

PROCEDURE GramKeyName(self: SynParse.GivenKeyword; p: SynParse.T; 
                     READONLY info: SynLocation.Info): SynParse.Tree 
    RAISES {SynParse.Fail} ;

PROCEDURE GramKeyInt(self: SynParse.GivenKeyword; p: SynParse.T; 
                     READONLY info: SynLocation.Info): SynParse.Tree 
    RAISES {SynParse.Fail} ;

PROCEDURE GramKeyReal(self: SynParse.GivenKeyword; p: SynParse.T; 
                      READONLY info: SynLocation.Info): SynParse.Tree 
    RAISES {SynParse.Fail} ;

PROCEDURE GramKeyChar(self: SynParse.GivenKeyword; p: SynParse.T; 
                      READONLY info: SynLocation.Info): SynParse.Tree 
    RAISES {SynParse.Fail} ;

PROCEDURE GramKeyString(self: SynParse.GivenKeyword; p: SynParse.T; 
                        READONLY info: SynLocation.Info): SynParse.Tree 
    RAISES {SynParse.Fail} ;

PROCEDURE GramKeyEof(self: SynParse.GivenKeyword; p: SynParse.T; 
                     READONLY info: SynLocation.Info): SynParse.Tree 
    RAISES {SynParse.Fail} ;

PROCEDURE GramList(self: SynParse.Action; p: SynParse.T; base: INTEGER;
                   READONLY info: SynLocation.Info): SynParse.Tree RAISES {SynParse.Fail} ;

PROCEDURE Storage(self: SynParse.Action; p: SynParse.T; base: INTEGER;
                  READONLY info: SynLocation.Info): SynParse.Tree RAISES {SynParse.Fail} ;

PROCEDURE GramExpSequence(self: SynParse.Action; p: SynParse.T; base: INTEGER;
                          READONLY info: SynLocation.Info): SynParse.Tree 
    RAISES {SynParse.Fail} ;

PROCEDURE GramExpChoice(self: SynParse.Action; p: SynParse.T; base: INTEGER;
                        READONLY info: SynLocation.Info): SynParse.Tree 
    RAISES {SynParse.Fail} ;

PROCEDURE GramExpParens(self: SynParse.Action; p: SynParse.T; base: INTEGER;
                        READONLY info: SynLocation.Info): SynParse.Tree 
    RAISES {SynParse.Fail} ;

PROCEDURE GramExpBase(self: SynParse.Action; p: SynParse.T; base: INTEGER;
                      READONLY info: SynLocation.Info): SynParse.Tree 
    RAISES {SynParse.Fail} ;

PROCEDURE GramExpIter(self: SynParse.Action; p: SynParse.T; base: INTEGER;
                      READONLY info: SynLocation.Info): SynParse.Tree 
    RAISES {SynParse.Fail} ;

PROCEDURE GramExpIterNoPos(self: SynParse.Action; p: SynParse.T; base: INTEGER;
                           READONLY info: SynLocation.Info): SynParse.Tree 
    RAISES {SynParse.Fail} ;

PROCEDURE GramExpIterPos(self: SynParse.Action; p: SynParse.T; base: INTEGER;
                         READONLY info: SynLocation.Info): SynParse.Tree 
    RAISES {SynParse.Fail} ;

PROCEDURE ActionString(self: SynParse.Action; p: SynParse.T; base: INTEGER;
                       READONLY info: SynLocation.Info): 
                       SynParse.Tree RAISES {SynParse.Fail} ;

PROCEDURE ActionInteger(self: SynParse.Action; p: SynParse.T; base: INTEGER;
                        READONLY info: SynLocation.Info): 
                        SynParse.Tree RAISES {SynParse.Fail} ;

PROCEDURE AntiquotedAction(self: SynParse.Action; p: SynParse.T; base: INTEGER;
                           READONLY info: SynLocation.Info)
                           : SynParse.Tree RAISES {SynParse.Fail} ;

PROCEDURE Action(self: SynParse.Action; p: SynParse.T; base: INTEGER;
                 READONLY info: SynLocation.Info): SynParse.Tree RAISES {SynParse.Fail} ;

PROCEDURE Single(self: SynParse.Action; p: SynParse.T; base: INTEGER;
                 READONLY info: SynLocation.Info): SynParse.Tree;

PROCEDURE GramExp(self: SynParse.Action; p: SynParse.T; base: INTEGER;
                  READONLY info: SynLocation.Info): SynParse.Tree;

PROCEDURE ConsParam(self: SynParse.Action; p: SynParse.T; base: INTEGER;
               READONLY info: SynLocation.Info): SynParse.Tree;

END Builder.
