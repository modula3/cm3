(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* Last modified on Fri Jun  3 12:49:11 1994 by luca               *)
(*      modified on Tue Jun 23 20:16:54 1992 by knaff          *)
(*                                                                           *)
(* Parts Copyright (C) 1997, Columbia University                             *)
(* All rights reserved.                                                      *)
(*
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Mon Aug  4 15:05:17 1997
 *)

MODULE Builder;
IMPORT SynParse, SynLocation, SynScan, SynWr, MetaParser, Text;


TYPE
  ClauseExtends = 
    SynParse.Tree BRANDED "Builder.ClauseExtends" OBJECT
    extend, iter, iterPosPresent: BOOLEAN;
    iterPos: INTEGER;
  END;
  StringAction =
    SynParse.Action BRANDED "Builder.StringAction" OBJECT
    text: TEXT;
  END;
  IntegerAction =
    SynParse.Action BRANDED "Builder.IntegerAction" OBJECT
    int: INTEGER;
  END;
  ProcAction =
    SynParse.Action BRANDED "Builder.ProcAction" OBJECT
    proc: SynParse.Action;
  END;
    
REVEAL
  GramInfo =
    GramInfoBase BRANDED "Builder.GramInfo" OBJECT
    clauseList: MetaParser.ClauseList;
    oldKeySet, newKeySet: SynScan.KeywordSet;
  END;


(* symbol table *)


TYPE 
  H = MetaParser.ActionProcEntry ; 

VAR
  (* Sample Action Table *)
   sourceTable := ARRAY [0..27] OF MetaParser.ActionProcEntry 
                    { H { "ClauseList"            , ClauseList             },
                      H { "ClauseExtendsIterNoPos", ClauseExtendsIterNoPos },
                      H { "ClauseExtendsIterPos"  , ClauseExtendsIterPos   },
                      H { "ClauseExtendsIter"     , ClauseExtendsIter      },
                      H { "ClauseExtendsIter"     , ClauseExtendsIter      },
                      H { "ClauseExtendsChoice"   , ClauseExtendsChoice    },
                      H { "ClauseExtendsYes"      , ClauseExtendsYes       },
                      H { "ClauseExtendsNo"       , ClauseExtendsNo        },
                      H { "Storage"               , Storage                },
                      H { "Ide"                   , Ide2                   },
                      H { "Name"                  , Name2                  },
                      H { "GramIde"               , GramIde2               },
                      H { "GramString"            , GramString2            },
                      H { "GramKeyIde"            , GramKeyIde2            },
                      H { "GramKeyName"           , GramKeyName2           },
                      H { "GramKeyInt"            , GramKeyInt2            },
                      H { "GramKeyEof"            , GramKeyEof2            },
                      H { "GramKeyReal"           , GramKeyReal2           },
                      H { "GramKeyChar"           , GramKeyChar2           },
                      H { "GramKeyString"         , GramKeyString2         },
                      H { "GramExpSequence"       , GramExpSequence        },
                      H { "GramExpChoice"         , GramExpChoice          },
                      H { "GramExpIterPos"        , GramExpIterPos         },
                      H { "GramExpIterNoPos"      , GramExpIterNoPos       },
                      H { "GramExpIter"           , GramExpIter            },
                      H { "GramExpBase"           , GramExpBase            },
                      H { "GramExpParens"         , GramExpParens          },
                      H { "GramList"              , GramList               } };

PROCEDURE LinkIn(table : MetaParser.ActionTable ) =
BEGIN
  MetaParser.TableFromArray(sourceTable,table);
END LinkIn;



(* error printing routine *)



(* "getter"routines *)
PROCEDURE GClauseList(p: SynParse.T; loc: INTEGER): MetaParser.ClauseList 
    RAISES {SynParse.Fail}=
  BEGIN
    TYPECASE p.stack[loc] OF
    | MetaParser.ClauseList(node) => RETURN node ;
    ELSE
    END;    
    MetaParser.TypeError(p.Writer(),"clause-list", p.stack[loc]);
    <*ASSERT FALSE*>
  END GClauseList;

PROCEDURE GClauseExtends(p: SynParse.T; loc: INTEGER): ClauseExtends 
    RAISES {SynParse.Fail}=
  BEGIN
    TYPECASE p.stack[loc] OF
    | NULL=>
    | ClauseExtends(node) => RETURN node ;
    ELSE
    END;    
    MetaParser.TypeError(p.Writer(), "clause-extends", p.stack[loc]);
    <*ASSERT FALSE*>
  END GClauseExtends;

PROCEDURE GIdeNode(p: SynParse.T; loc: INTEGER): MetaParser.TextNode 
    RAISES {SynParse.Fail}=
  BEGIN
    TYPECASE p.stack[loc] OF
    | NULL=>
    | MetaParser.TextNode(node) => RETURN node ;
    ELSE
    END;    
    MetaParser.TypeError(p.Writer(), "n ide-node", p.stack[loc]);
    <*ASSERT FALSE*>
  END GIdeNode;

PROCEDURE GGrammar(p: SynParse.T; loc: INTEGER): SynParse.Grammar 
    RAISES {SynParse.Fail}=
  BEGIN
    TYPECASE p.stack[loc] OF
    | NULL=>
    | SynParse.Grammar(node) => RETURN node ;
    ELSE
    END;        
    MetaParser.TypeError(p.Writer(), "grammar",p.stack[loc]);
    <*ASSERT FALSE*>
  END GGrammar;

PROCEDURE GGramList(p: SynParse.T; loc: INTEGER): SynParse.GrammarList 
    RAISES {SynParse.Fail}=
  BEGIN
    TYPECASE p.stack[loc] OF
    | SynParse.GrammarList(node) => RETURN node ;
    ELSE
    END;    
    MetaParser.TypeError(p.Writer(), "grammar list",p.stack[loc]);
    <*ASSERT FALSE*>
  END GGramList;

PROCEDURE GArgs(p: SynParse.T; loc: INTEGER):SynParse.Args 
    RAISES {SynParse.Fail}=
  VAR
    n: INTEGER;
    args: SynParse.Tree;
    ret: SynParse.Args;
  BEGIN
    n:= 0;
    args := p.stack[loc];
    LOOP
      TYPECASE args OF
      | NULL=> EXIT
      | Params(node) => INC(n); args:=node.rest;
      ELSE <*ASSERT FALSE*>
      END;
    END;
    ret := NEW(SynParse.Args,n);
    args := p.stack[loc];
    FOR i := 0 TO n-1 DO
      TYPECASE args OF
      | NULL =>
      | Params(node) => 
          ret^[i] := MetaParser.XInt(p.Writer(), node.first); 
          args:=node.rest;
      ELSE <*ASSERT FALSE*>
      END;
    END;
    RETURN ret;
  END GArgs;

PROCEDURE Ide(<*UNUSED*>self: SynParse.Identifier;
              <*UNUSED*>p: SynParse.T; 
              name: TEXT;
              <*UNUSED*>READONLY info: SynLocation.Info): SynParse.Tree =
  BEGIN
    RETURN NEW(MetaParser.TextNode, text:=name);
  END Ide;
  
PROCEDURE Ide2(<*UNUSED*> self: SynParse.Action; 
               p: SynParse.T; base: INTEGER; 
               READONLY info: SynLocation.Info): SynParse.Tree 
               RAISES {SynParse.Fail}=
  BEGIN
    RETURN
      Ide(NIL, p, MetaParser.GText(p, base+1), info);
  END Ide2;

PROCEDURE Name(<*UNUSED*>self: SynParse.Name; 
               <*UNUSED*>p: SynParse.T; name: TEXT;
               <*UNUSED*>READONLY info: SynLocation.Info): SynParse.Tree =
  BEGIN
    RETURN NEW(MetaParser.TextNode, text:=name);
  END Name;
  
PROCEDURE Name2(<*UNUSED*>self: SynParse.Action; 
               p: SynParse.T; base: INTEGER; 
               READONLY info: SynLocation.Info): SynParse.Tree
               RAISES {SynParse.Fail}=
  BEGIN
    RETURN
      Name(NIL, p, MetaParser.GText(p, base+1), info);
  END Name2;

  
PROCEDURE Grammar(<*UNUSED*>self:SynParse.Action; p: SynParse.T; base: INTEGER;
                  <*UNUSED*>READONLY info: SynLocation.Info)
                  : SynParse.Tree 
                  RAISES {SynParse.Fail} =
  BEGIN
    (* -- check that names in list are unique, whether extensions or not. *)
    RETURN GClauseList(p, base+1);
  END Grammar;
  


PROCEDURE ClauseList(<*UNUSED*>self: SynParse.Action; 
                     p: SynParse.T; base: INTEGER;
                     READONLY info: SynLocation.Info): SynParse.Tree
  RAISES {SynParse.Fail} =
  VAR 
    clauseExtends: ClauseExtends;
  BEGIN
    clauseExtends:=GClauseExtends(p, base+2);
    
    RETURN 
      NEW(MetaParser.ClauseList, location:=SynLocation.NewLineLocation(info),
          ide:=GIdeNode(p, base+1),
          args:= GArgs(p, base+5),
          extend:=clauseExtends.extend,
          extendIter:=clauseExtends.iter,
          iterPosPresent:=clauseExtends.iterPosPresent,
          iterPos:=clauseExtends.iterPos,
          gram:=GGrammar(p, base+3),
          rest:=GClauseList(p, base+4));
  END ClauseList;
  
PROCEDURE ClauseExtendsChoice(<*UNUSED*>self: SynParse.Action; 
                              <*UNUSED*>p: SynParse.T; 
                              <*UNUSED*>base: INTEGER;
                              <*UNUSED*>READONLY info: SynLocation.Info): 
  SynParse.Tree =
  BEGIN
    RETURN NEW(ClauseExtends, extend:=TRUE, iter:=FALSE,
               iterPosPresent:=FALSE, iterPos:=0);
  END ClauseExtendsChoice;
  
PROCEDURE ClauseExtendsIterPos(<*UNUSED*>self: SynParse.Action; p: SynParse.T; 
                               base: INTEGER;
                               <*UNUSED*>READONLY info: SynLocation.Info): 
  SynParse.Tree RAISES {SynParse.Fail} =
  BEGIN
    RETURN NEW(ClauseExtends, extend:=TRUE, iter:=TRUE,
               iterPosPresent:=TRUE, 
               iterPos:=MetaParser.GInt(p, base+3));
  END ClauseExtendsIterPos;
  
PROCEDURE ClauseExtendsIterNoPos(<*UNUSED*>self: SynParse.Action; 
                                 <*UNUSED*>p: SynParse.T; 
                                 <*UNUSED*>base: INTEGER;
                                 <*UNUSED*>READONLY info: SynLocation.Info): 
  SynParse.Tree =
  BEGIN
    RETURN NEW(ClauseExtends, extend:=TRUE, iter:=TRUE,
               iterPosPresent:=FALSE, iterPos:=0);
  END ClauseExtendsIterNoPos;
  
PROCEDURE ClauseExtendsIter(<*UNUSED*>self: SynParse.Action; p: SynParse.T; 
                            base: INTEGER;
                            <*UNUSED*>READONLY info: SynLocation.Info): 
  SynParse.Tree =
  BEGIN
    RETURN p.stack[base+2];
  END ClauseExtendsIter;
  
PROCEDURE ClauseExtendsNo(<*UNUSED*>self: SynParse.Action; 
                          <*UNUSED*>p: SynParse.T; 
                          <*UNUSED*>base: INTEGER;
                          <*UNUSED*>READONLY info: SynLocation.Info): 
  SynParse.Tree =
  BEGIN
    RETURN NEW(ClauseExtends, extend:=FALSE, iter:=FALSE);
  END ClauseExtendsNo;
  
PROCEDURE ClauseExtendsYes(<*UNUSED*>self: SynParse.Action; 
                           p: SynParse.T; base: INTEGER;
                           <*UNUSED*>READONLY info: SynLocation.Info): 
  SynParse.Tree =
  BEGIN
    RETURN p.stack[base+1];
  END ClauseExtendsYes;
  


  (* ****************************** *)
  (*  teminals of client grammar    *)
  (* ****************************** *)

PROCEDURE GramIdeCm(name: TEXT;args: SynParse.Args;
                    READONLY info: SynLocation.Info): SynParse.Tree =
  BEGIN
    RETURN NEW(SynParse.NonTerminal, 
               location:=SynLocation.NewLineLocation(info),
               args:=args,
               name:=name);
  END GramIdeCm;

  
PROCEDURE GramIde2(<*UNUSED*>self: SynParse.Action; 
                   p: SynParse.T; base: INTEGER;
                   READONLY info: SynLocation.Info): SynParse.Tree RAISES {SynParse.Fail} =
  BEGIN
    RETURN
      GramIdeCm(MetaParser.GText(p, base+1),GArgs(p, base+2),info);
  END GramIde2;



  PROCEDURE GramString(<*UNUSED*>self: SynParse.QuotedString; p: SynParse.T; 
                       string: TEXT;
                       READONLY info: SynLocation.Info)
    : SynParse.Tree RAISES {SynParse.Fail} =
    VAR name: TEXT;
    BEGIN
      IF Text.Length(string)=0 THEN 
	SynParse.Fault(p, "Invalid token: \"\"") 
      END;
      IF (Text.Length(string)=1) AND 
          SynScan.IsDelimiter(p.Scanner(), Text.GetChar(string,0)) THEN
	RETURN 
	  NEW(SynParse.GivenDelimiter, location:=SynLocation.NewLineLocation(info),
	    delim:=Text.GetChar(string,0)); 
      ELSIF (Text.Length(string)>1) AND (Text.GetChar(string,0)='~') THEN
	name := Text.Sub(string, 1, Text.Length(string)-1);
        IF SynScan.IsIdentifier(p.Scanner(), name) THEN
          RETURN
            NEW(SynParse.GivenName, location:=SynLocation.NewLineLocation(info),
	      text:=name);
        ELSE 
	  SynParse.Fault(p, "Invalid token: "& string);
        <*ASSERT FALSE*>
        END;
      ELSIF SynScan.IsIdentifier(p.Scanner(), string) THEN
        RETURN 
	    NEW(SynParse.GivenKeyword, 
                location:=SynLocation.NewLineLocation(info),
                key:=string);
      ELSE 
	SynParse.Fault(p, "Invalid token: "& string);
        <*ASSERT FALSE*>
      END;
    END GramString;


(*
PROCEDURE GramString(self: SynParse.QuotedString; 
                     p: SynParse.T; string: String.T;
                     READONLY info: SynLocation.Info): SynParse.Tree RAISES {SynParse.Fail} =
  BEGIN
    IF String.Length(string)=0 THEN 
      SynParse.Fault(p, "Invalid token: \"\"") 
    END;
    IF (String.Length(string)=1) AND SynScan.IsDelimiter(p.sc, string[0]) THEN
      RETURN 
        NEW(SynParse.GivenDelimiter, location:=SynLocation.NewLineLocation(info),
            delim:=string[0]);
      (* don't initialise build, take default action ( i.e. put 
         NIL onto stack )
      *)                       
    ELSIF SynScan.IsIdentifier(p.sc, string) THEN
      RETURN 
        (* Fill the key field later; store it in ide for now. *)
        NEW(GivenNamedKeyword, location:=SynLocation.NewLineLocation(info),
            ide:=String.ToText(string), key:=NIL);
      (* don't initialise build, take default action ( i.e. put 
         NIL onto stack *)
    ELSE SynParse.Fault(p, "Invalid token: "&String.ToText(string));
    END;
  END GramString;
*)  

PROCEDURE GramString2(<*UNUSED*>self: SynParse.Action; 
                      p: SynParse.T; base: INTEGER;
                      READONLY info: SynLocation.Info): 
  SynParse.Tree RAISES {SynParse.Fail} =
  BEGIN
    RETURN
      GramString(NIL,p, MetaParser.GText(p, base+1),info);
  END GramString2;


PROCEDURE GramKeyIde(<*UNUSED*>self: SynParse.GivenKeyword; 
                     <*UNUSED*>p: SynParse.T; 
                     READONLY info: SynLocation.Info): SynParse.Tree =
  BEGIN
    RETURN NEW(SynParse.Identifier, 
               location:=SynLocation.NewLineLocation(info),
               Build:=MetaParser.IdentifierToTree);
  END GramKeyIde;
  
PROCEDURE GramKeyIde2(<*UNUSED*>self: SynParse.Action; p: SynParse.T; 
                      <*UNUSED*>base: INTEGER;
                      READONLY info: SynLocation.Info): SynParse.Tree =
  BEGIN
    RETURN
      GramKeyIde(NIL, p, info);
  END GramKeyIde2 ;


PROCEDURE GramKeyName(<*UNUSED*>self: SynParse.GivenKeyword; 
                      <*UNUSED*>p: SynParse.T; 
                      READONLY info: SynLocation.Info): SynParse.Tree =
  BEGIN
    RETURN NEW(SynParse.Name, 
               location:=SynLocation.NewLineLocation(info),
               Build:=MetaParser.NameToTree);
  END GramKeyName;
  
PROCEDURE GramKeyName2(<*UNUSED*>self: SynParse.Action; p: SynParse.T;
                       <*UNUSED*>base: INTEGER;
                       READONLY info: SynLocation.Info): SynParse.Tree =
  BEGIN
    RETURN
      GramKeyName(NIL, p, info);
  END GramKeyName2 ;


PROCEDURE GramKeyInt(<*UNUSED*>self: SynParse.GivenKeyword; 
                     <*UNUSED*>p: SynParse.T; 
                     READONLY info: SynLocation.Info): SynParse.Tree =
  BEGIN
    RETURN NEW(SynParse.Integer, 
               location:=SynLocation.NewLineLocation(info),
               Build:=MetaParser.IntegerToTree);
  END GramKeyInt;

PROCEDURE GramKeyInt2(<*UNUSED*>self: SynParse.Action; p: SynParse.T;
                      <*UNUSED*>base: INTEGER;
                      READONLY info: SynLocation.Info): SynParse.Tree =
  BEGIN
    RETURN
      GramKeyInt(NIL, p, info);
  END GramKeyInt2 ;

PROCEDURE GramKeyReal(<*UNUSED*>self: SynParse.GivenKeyword; 
                      <*UNUSED*>p: SynParse.T; 
                      READONLY info: SynLocation.Info): SynParse.Tree =
  BEGIN
    RETURN NEW(SynParse.Real, 
               location:=SynLocation.NewLineLocation(info),
               Build:=MetaParser.RealToTree);
  END GramKeyReal;

PROCEDURE GramKeyReal2(<*UNUSED*>self: SynParse.Action; p: SynParse.T;
                       <*UNUSED*>base: INTEGER;
                       READONLY info: SynLocation.Info): SynParse.Tree =
  BEGIN
    RETURN
      GramKeyReal(NIL, p, info);
  END GramKeyReal2 ;


  
PROCEDURE GramKeyChar(<*UNUSED*>self: SynParse.GivenKeyword; 
                      <*UNUSED*>p: SynParse.T; 
                      READONLY info: SynLocation.Info): SynParse.Tree =
  BEGIN
    RETURN NEW(SynParse.QuotedChar, 
               location:=SynLocation.NewLineLocation(info),
               Build:=MetaParser.CharToTree);
  END GramKeyChar;
  

PROCEDURE GramKeyChar2(<*UNUSED*>self: SynParse.Action; p: SynParse.T;
                       <*UNUSED*>base: INTEGER;
                      READONLY info: SynLocation.Info): SynParse.Tree =
  BEGIN
    RETURN      
      GramKeyChar(NIL, p, info);
  END GramKeyChar2 ;



PROCEDURE GramKeyString(<*UNUSED*>self: SynParse.GivenKeyword; 
                        <*UNUSED*>p: SynParse.T; 
                        READONLY info: SynLocation.Info): SynParse.Tree =
  BEGIN
    RETURN NEW(SynParse.QuotedString, 
               location:=SynLocation.NewLineLocation(info),
               Build:=MetaParser.StringToTree);
  END GramKeyString;


PROCEDURE GramKeyString2(<*UNUSED*>self: SynParse.Action; p: SynParse.T;
                         <*UNUSED*>base: INTEGER;
                         READONLY info: SynLocation.Info): SynParse.Tree =
  BEGIN
    RETURN      
      GramKeyString(NIL, p, info);
  END GramKeyString2 ;


PROCEDURE GramKeyEof(<*UNUSED*>self: SynParse.GivenKeyword; 
                     <*UNUSED*>p: SynParse.T; 
                     READONLY info: SynLocation.Info): SynParse.Tree =
  BEGIN
    RETURN NEW(SynParse.Eof, 
               location:=SynLocation.NewLineLocation(info));
  END GramKeyEof;
  

PROCEDURE GramKeyEof2(<*UNUSED*>self: SynParse.Action; p: SynParse.T; 
                      <*UNUSED*>base: INTEGER;
                      READONLY info: SynLocation.Info): SynParse.Tree =
  BEGIN
    RETURN      
      GramKeyEof(NIL, p, info);
  END GramKeyEof2 ;


  (* ************************************ *)



PROCEDURE GramActionString(self: SynParse.Action;
                           p: SynParse.T; <*UNUSED*>base: INTEGER;
                           READONLY info: SynLocation.Info)
                           :SynParse.Tree=
  BEGIN
    RETURN
      MetaParser.TextToTree(NIL, p, NARROW(self,StringAction).text,info);
  END GramActionString;

PROCEDURE GramActionProc(self: SynParse.Action;
                         <*UNUSED*>p: SynParse.T; <*UNUSED*>base: INTEGER;
                         <*UNUSED*>READONLY info: SynLocation.Info)
                         :SynParse.Tree =
  BEGIN
    RETURN
      NARROW(self,ProcAction).proc;
  END GramActionProc;    
  
PROCEDURE GramActionInteger(self: SynParse.Action;
                           p: SynParse.T; <*UNUSED*>base: INTEGER;
                           READONLY info: SynLocation.Info)
                           :SynParse.Tree=
  BEGIN
    RETURN
      MetaParser.IntegerToTree(NIL, p, NARROW(self,IntegerAction).int,info);
  END GramActionInteger;

  
  (* **************************** *)
  
  
  
PROCEDURE GramList(<*UNUSED*>self: SynParse.Action; p: SynParse.T; 
                   base: INTEGER;
                   READONLY info: SynLocation.Info): SynParse.Tree RAISES {SynParse.Fail} =
  BEGIN
    RETURN 
      NEW(SynParse.GrammarList, 
          location:=SynLocation.NewLineLocation(info),
          first:=p.stack[base+1],
          rest:=GGramList(p, base+2));
  END GramList;
  
PROCEDURE Storage(<*UNUSED*>self: SynParse.Action; 
                  p: SynParse.T; base: INTEGER;
                  READONLY info: SynLocation.Info): SynParse.Tree RAISES {SynParse.Fail} =
  BEGIN
    RETURN NEW(SynParse.Storage, 
               location:=SynLocation.NewLineLocation(info),
               position:=MetaParser.GInt(p, base+3),
               item:=GGrammar(p, base+1));
  END Storage;
  
PROCEDURE GramExpSequence(<*UNUSED*>self: SynParse.Action; 
                          p: SynParse.T; base: INTEGER;
                          READONLY info: SynLocation.Info): SynParse.Tree RAISES {SynParse.Fail} =
  BEGIN
    RETURN NEW(SynParse.Sequence, 
               location:=SynLocation.NewLineLocation(info),
               items:=GGramList(p, base+1));
  END GramExpSequence;
  
PROCEDURE GramExpChoice(<*UNUSED*>self: SynParse.Action; 
                        p: SynParse.T; base: INTEGER;
                        READONLY info: SynLocation.Info): SynParse.Tree RAISES {SynParse.Fail} =
  BEGIN
    RETURN NEW(SynParse.Choice, 
               location:=SynLocation.NewLineLocation(info),
               choice:=GGramList(p, base+1));
  END GramExpChoice;
  
PROCEDURE GramExpParens(<*UNUSED*>self: SynParse.Action; 
                        p: SynParse.T; base: INTEGER;
                        <*UNUSED*>READONLY info: SynLocation.Info): 
  SynParse.Tree =
  BEGIN
    RETURN p.stack[base+6];
  END GramExpParens;
  
PROCEDURE GramExpBase(<*UNUSED*>self: SynParse.Action; 
                      p: SynParse.T; base: INTEGER;
                      <*UNUSED*>READONLY info: SynLocation.Info): 
  SynParse.Tree =
  BEGIN
    RETURN p.stack[base+1];
  END GramExpBase;
  
PROCEDURE GramExpIter(<*UNUSED*>self: SynParse.Action; 
                      p: SynParse.T; base: INTEGER;
                      <*UNUSED*>READONLY info: SynLocation.Info): 
  SynParse.Tree =
  BEGIN
    RETURN p.stack[base+5];
  END GramExpIter;
  
PROCEDURE GramExpIterNoPos(<*UNUSED*>self: SynParse.Action; 
                           p: SynParse.T; base: INTEGER;
                           READONLY info: SynLocation.Info): SynParse.Tree RAISES {SynParse.Fail} =
  BEGIN
    RETURN NEW(SynParse.Iter, location:=SynLocation.NewLineLocation(info),
               base:=GGrammar(p, base+1), 
               iter:=GGrammar(p, base+3),
               accum:=FALSE,
               accumPosition:=0);
  END GramExpIterNoPos;
  
PROCEDURE GramExpIterPos(<*UNUSED*>self: SynParse.Action; 
                         p: SynParse.T; base: INTEGER;
                         READONLY info: SynLocation.Info): SynParse.Tree RAISES {SynParse.Fail} =
  BEGIN
    RETURN NEW(SynParse.Iter, location:=SynLocation.NewLineLocation(info),
               base:= GGrammar(p, base+1), 
               iter:= GGrammar(p, base+3),
               accum:=TRUE,
               accumPosition:=MetaParser.GInt(p, base+4));
  END GramExpIterPos;
  

  (* added following procedure for generating constant strings *)
PROCEDURE ActionString(<*UNUSED*>self: SynParse.Action; 
                       p: SynParse.T; base: INTEGER;
                       READONLY info: SynLocation.Info): 
                       SynParse.Tree RAISES {SynParse.Fail} =
  BEGIN
    RETURN
      NEW(StringAction,
          location := SynLocation.NewLineLocation(info),
          grammar := GGrammar(p, base+1),
          text := MetaParser.GText(p, base+3),
          Build := GramActionString);
  END ActionString;


  (* added following procedure for generating constant integers *)
PROCEDURE ActionInteger(<*UNUSED*>self: SynParse.Action; 
                        p: SynParse.T; base: INTEGER;
                       READONLY info: SynLocation.Info): 
                       SynParse.Tree RAISES {SynParse.Fail} =
  BEGIN
    RETURN
      NEW(IntegerAction,
          location := SynLocation.NewLineLocation(info),
          grammar := GGrammar(p, base+1),
          int := MetaParser.GInt(p, base+3),
          Build := GramActionInteger);
  END ActionInteger;


PROCEDURE LookupAction(p: SynParse.T; base: INTEGER;
                       READONLY info: SynLocation.Info): ActionProc
                       RAISES {SynParse.Fail}=
  VAR 
    name : TEXT ;
    ref: REFANY;
  BEGIN                
    (* AK look up action NEW *)
    name := MetaParser.GText(p, base+3);     
    IF actionTable.get(name,ref) THEN                
      TYPECASE ref OF
      | REF ActionProc(node) => RETURN node^;
      ELSE
        WITH out = p.Writer() DO
          SynWr.Text(out, "Not an action: "&name&" ", loud:=TRUE);
          SynLocation.PrintLocation(out, SynLocation.NewLineLocation(info));
          SynWr.Text(out, "\n", loud:=TRUE);
          SynWr.Flush(out, loud:=TRUE);
        END;
        RAISE SynParse.Fail;
      END;
        
    ELSE
      WITH out = p.Writer() DO
        SynWr.Text(out, "Unknown action: "&name&" ", loud:=TRUE);
        SynLocation.PrintLocation(out, SynLocation.NewLineLocation(info));
        SynWr.Text(out, "\n", loud:=TRUE);
        SynWr.Flush(out, loud:=TRUE);
      END;
      RAISE SynParse.Fail;
    END;
  END LookupAction;


PROCEDURE AntiquotedAction(<*UNUSED*>self: SynParse.Action; 
                           p: SynParse.T; base: INTEGER;
                           READONLY info: SynLocation.Info): 
                           SynParse.Tree RAISES {SynParse.Fail} =
  BEGIN
    RETURN
      NEW(ProcAction,
          location := SynLocation.NewLineLocation(info),
          grammar:= GGrammar(p, base+1),
          proc := NEW(SynParse.Action,
                      location := NIL,
                      grammar := NIL,                
                      Build := LookupAction(p, base,info)),
          Build := GramActionProc);
  END AntiquotedAction;

  (* following procedure for generating actions *)
PROCEDURE Action(<*UNUSED*>self: SynParse.Action; p: SynParse.T; base: INTEGER;
                 READONLY info: SynLocation.Info): 
                 SynParse.Tree RAISES {SynParse.Fail} =

  BEGIN
    RETURN       
      NEW(SynParse.Action, 
          location := SynLocation.NewLineLocation(info),
          grammar:= GGrammar(p, base+1),
          Build := LookupAction(p, base, info));
  END Action;

  PROCEDURE Single(<*UNUSED*>self: SynParse.Action; 
                   p: SynParse.T; base: INTEGER;
                   <*UNUSED*>READONLY info: SynLocation.Info): SynParse.Tree =
    BEGIN
      RETURN p.stack[base+1];
    END Single;

  PROCEDURE GramExp(<*UNUSED*>self: SynParse.Action; 
                    p: SynParse.T; base: INTEGER;
                    <*UNUSED*>READONLY info: SynLocation.Info): SynParse.Tree =
    BEGIN
      RETURN p.stack[base+2];
    END GramExp;

PROCEDURE ConsParam(<*UNUSED*>self: SynParse.Action; 
                    p: SynParse.T; base: INTEGER;
               READONLY info: SynLocation.Info): SynParse.Tree=
  BEGIN
    RETURN NEW(Params,  
          location := SynLocation.NewLineLocation(info),
          first:=p.stack[base+1], rest:=p.stack[base+2]);
  END ConsParam;

BEGIN

END Builder.



