(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* Last modified on Fri Nov  5 11:52:44 1993 by luca                       *)
(*      modified on Mon Jun 29 20:25:11 1992 by knaff          *)
MODULE Constructor;

IMPORT  SynWr,SynParse,Builder,MetaParser,SynScan, SynLocation;

TYPE 
  H = MetaParser.ActionProcEntry ; 

VAR
   sourceTable := ARRAY [0..4] OF MetaParser.ActionProcEntry 
                    { H { "select1", Fetch1 }  , 
                      H { "select2", Fetch2 }  , 
                      H { "select3", Fetch3 }  , 
                      H { "select4", Fetch4 }  , 
                      H { "select5", Fetch5 }  } ;

PROCEDURE LinkInSelects(table : MetaParser.ActionTable ) =
BEGIN
  MetaParser.TableFromArray(sourceTable,table);
END LinkInSelects;

PROCEDURE Fetch1(<*UNUSED*>self: SynParse.Action; p: SynParse.T; base: INTEGER;
                 <*UNUSED*>READONLY info: SynLocation.Info): SynParse.Tree =
  BEGIN
    RETURN p.stack[base+1];
  END Fetch1;

PROCEDURE Fetch2(<*UNUSED*>self: SynParse.Action; p: SynParse.T; base: INTEGER;
                 <*UNUSED*>READONLY info: SynLocation.Info): SynParse.Tree =
  BEGIN
    RETURN p.stack[base+2];
  END Fetch2;

PROCEDURE Fetch3(<*UNUSED*>self: SynParse.Action; p: SynParse.T; base: INTEGER;
                 <*UNUSED*>READONLY info: SynLocation.Info): SynParse.Tree =
  BEGIN
    RETURN p.stack[base+3];
  END Fetch3;

PROCEDURE Fetch4(<*UNUSED*>self: SynParse.Action; p: SynParse.T; base: INTEGER;
                 <*UNUSED*>READONLY info: SynLocation.Info): SynParse.Tree =
  BEGIN
    RETURN p.stack[base+4];
  END Fetch4;

PROCEDURE Fetch5(<*UNUSED*>self: SynParse.Action; p: SynParse.T; base: INTEGER;
                 <*UNUSED*>READONLY info: SynLocation.Info): SynParse.Tree =
  BEGIN
    RETURN p.stack[base+5];
  END Fetch5;

PROCEDURE Setup(wr: SynWr.T) RAISES {SynParse.Fail} =
VAR
  keyDefSyn, keyIde, keyName, keyEof, keyInt, keyReal, 
  keyChar, keyString, keyTermSem, keyStar, keyEq,
  keyLess, keyMore, keyHat, keyTermSemHat : SynScan.Keyword;
  clauseRest, clauseSeq, clauseExtends,
  gramExp, gramParamList, gramParamList2, gramExpBase, gramExpIde, 
  gramExpSequence, gramExpChoice, gramExpParens, gramExpIter, gramExpList, 
  action : SynParse.NonTerminal;
  keySet : SynScan.KeywordSet ;
  
BEGIN
  metaParser := SynParse.New(wr, SynParse.NewEnv());
  keySet := SynScan.NewKeywordSet();
  
  keyDefSyn := SynScan.BeKeyword("::=", keySet);    
  keyIde := SynScan.BeKeyword("ide", keySet);
  keyName := SynScan.BeKeyword("name", keySet);
  keyEof := SynScan.BeKeyword("EOF", keySet);
  keyInt := SynScan.BeKeyword("int", keySet);
  keyReal := SynScan.BeKeyword("real", keySet);
  keyChar := SynScan.BeKeyword("char", keySet);
  keyString := SynScan.BeKeyword("string", keySet);
  keyTermSem := SynScan.BeKeyword("::", keySet);
  keyTermSemHat := SynScan.BeKeyword("::^",keySet);  
  keyStar := SynScan.BeKeyword("*", keySet);
  keyEq := SynScan.BeKeyword("=", keySet);
  keyLess := SynScan.BeKeyword("<",keySet);  
  keyMore := SynScan.BeKeyword(">",keySet);  
  keyHat := SynScan.BeKeyword("^",keySet);  

  SynScan.UseKeywordSet(metaParser.Scanner(), keySet);

  grammar := NEW(SynParse.NonTerminal, name:="*grammar", 
    args:=SynParse.noArgs);
  clauseSeq := NEW(SynParse.NonTerminal, name:="*clauseSeq", 
    args:=SynParse.noArgs);
  clauseRest := NEW(SynParse.NonTerminal, name:="*clauseRest", 
    args:=SynParse.noArgs);
  clauseExtends := NEW(SynParse.NonTerminal, name:="*clauseExtends", 
    args:=SynParse.noArgs);
  gramExp := NEW(SynParse.NonTerminal, name:="*gram", 
    args:=SynParse.noArgs);
  gramParamList := NEW(SynParse.NonTerminal, name:= "*paramList", 
    args:=SynParse.noArgs);
  gramParamList2 := NEW(SynParse.NonTerminal, name:= "*paramList2", 
    args:=SynParse.noArgs);
  gramExpBase := NEW(SynParse.NonTerminal, name:="*gramBase", 
    args:=SynParse.noArgs);
  gramExpIde := NEW(SynParse.NonTerminal, name:="*gramIdent", 
    args:=SynParse.noArgs);
  gramExpSequence := NEW(SynParse.NonTerminal, name:="*gramSequence", 
    args:=SynParse.noArgs);
  gramExpChoice := NEW(SynParse.NonTerminal, name:="*gramChoice", 
    args:=SynParse.noArgs);
  gramExpParens := NEW(SynParse.NonTerminal, name:="*gramParens", 
    args:=SynParse.noArgs);
  gramExpIter := NEW(SynParse.NonTerminal, name:="*gramIter", 
    args:=SynParse.noArgs);
  gramExpList := NEW(SynParse.NonTerminal, name:="*gramList", 
    args:=SynParse.noArgs);
  action := NEW(SynParse.NonTerminal, name:="*action", 
    args:=SynParse.noArgs);
  
  (* grammar ::=
     clauseSeq
  *)  
  metaParser.Add(grammar.name,
          NEW(SynParse.Action,
              grammar:=SynParse.Store(1, clauseSeq),
              Build:=Builder.Grammar));
  
  (* clauseSeq ::=
     [ gramExpIde "::=" clauseExtends gramExp clauseRest ]
  *)
  metaParser.Add(clauseSeq.name,
          NEW(SynParse.Action, grammar :=
            NEW(SynParse.Sequence, items:=
              SynParse.List(
                SynParse.Store(1, gramExpIde),
                SynParse.Store(5, gramParamList),
                NEW(SynParse.GivenKeyword, 
                    key:=SynScan.GetKeywordName(keyDefSyn)),
                SynParse.Store(2, clauseExtends),
                SynParse.Store(3, gramExp),
                SynParse.Store(4, clauseRest))),
            Build:=Builder.ClauseList));
  
  (* clauseRest ::=
     { "EOF" clauseSeq  }

     hack to avoid end of file problem 
  *)
  metaParser.Add(clauseRest.name,
          NEW(SynParse.Choice, choice:=
            SynParse.List(
              NEW(SynParse.Sequence, items:=
                SynParse.List(NEW(SynParse.Eof))),
              clauseSeq)));

  (* the given keyword is int a sequence in order to generate NIL *)

  (* clauseExtends ::=
     { [ "." "." "." { [ "*" { [ "_" int ] [] } ] [] } ] [] }
  *)
  metaParser.Add(
    clauseExtends.name,
    NEW(SynParse.Choice, choice:=
      SynParse.List(
        NEW(SynParse.Action, grammar:=
          NEW(SynParse.Sequence, items:=
            SynParse.List(
              NEW(SynParse.GivenDelimiter, delim:='.'),
              NEW(SynParse.GivenDelimiter, delim:='.'),
              NEW(SynParse.GivenDelimiter, delim:='.'),
              SynParse.Store(1,NEW(SynParse.Choice, choice:=
                SynParse.List(
                  NEW(SynParse.Action, grammar:=
                    NEW(SynParse.Sequence, items:=
                      SynParse.List(
                        NEW(SynParse.GivenKeyword, 
                            key:=SynScan.GetKeywordName(keyStar)),
                        SynParse.Store(
                          2,NEW(SynParse.Choice,choice:=
                            SynParse.List(
                              NEW(
                                SynParse.Action, grammar:=
                                  NEW(SynParse.Sequence, items:=
                                    SynParse.List(
                                      NEW(SynParse.GivenDelimiter, delim:='_'),
                                      SynParse.Store(
                                        3, NEW(SynParse.Integer, 
                                               Build:=MetaParser.IntegerToTree)))),
                                  Build:=Builder.ClauseExtendsIterPos),
                              NEW(SynParse.Action, grammar:=
                                NEW(SynParse.Sequence, items:=NIL),
                                Build:=Builder.ClauseExtendsIterNoPos)))))),
                    Build:=Builder.ClauseExtendsIter),
                  NEW(SynParse.Action, grammar:=
                    NEW(SynParse.Sequence, items:=NIL),
                    Build:=Builder.ClauseExtendsChoice)))))),
          Build:=Builder.ClauseExtendsYes),
        NEW(SynParse.Action,
            grammar := NEW(SynParse.Sequence, items:=NIL),
            Build:=Builder.ClauseExtendsNo))));
  
  (* gramExpIde ::=
     ide
  *)
  metaParser.Add(gramExpIde.name,
          NEW(SynParse.Identifier, 
              Build:=Builder.Ide));
      
  (* gramExp ::=
     (gramExpBase  * { [ "::" { actionTermExp 
                                [ "^" ide ] 
                                string 
                                int } 
                       ] 
                       [ "::^" ide ]
                       [ "_" int ] } ) 
  *)
  metaParser.Add(
    gramExp.name,
    NEW(SynParse.Iter,
        accum := TRUE,
        accumPosition := 1,
        base := gramExpBase,
        iter := 
          NEW(SynParse.Choice,choice :=
            SynParse.List(
              NEW(SynParse.Action, grammar:=
                NEW(SynParse.Sequence, items:=
                  SynParse.List(
                    NEW(SynParse.GivenKeyword, 
                        key:=SynScan.GetKeywordName(keyTermSem)),
                    SynParse.Store(
                      3,
                      NEW(SynParse.Choice, choice :=
                        SynParse.List(
                          NEW( SynParse.Action, grammar :=
                            SynParse.Store(3,NEW(SynParse.Identifier,
                                                 Build:=MetaParser.IdentifierToTree)),
                            Build:=Builder.Action),
                          NEW( SynParse.Action, grammar :=
                            NEW( SynParse.Sequence, items :=
                              SynParse.List(
                                NEW(SynParse.GivenKeyword, 
                                    key := SynScan.GetKeywordName(keyHat)),
                                SynParse.Store(3,NEW(SynParse.Identifier,
                                                     Build:=MetaParser.IdentifierToTree))
                                                              )),
                            Build:=Builder.AntiquotedAction),
                          NEW( SynParse.Action, grammar :=
                            SynParse.Store(3,NEW(SynParse.QuotedString,
                                                 Build:=MetaParser.StringToTree)),
                            Build:=Builder.ActionString),
                          NEW( SynParse.Action, grammar :=
                            SynParse.Store(3,NEW(SynParse.Integer,
                                                 Build:=MetaParser.IntegerToTree)),
                            Build:=Builder.ActionInteger)))))),
                Build := Fetch3),
              NEW(SynParse.Action, grammar :=
                NEW(SynParse.Sequence, items :=
                  SynParse.List(
                    NEW(SynParse.GivenKeyword, 
                        key:=SynScan.GetKeywordName(keyTermSemHat)),
                    SynParse.Store(3,NEW(SynParse.Identifier,
                                         Build:=MetaParser.IdentifierToTree)))),
                Build:=Builder.AntiquotedAction),
              NEW(SynParse.Action, grammar:=
                NEW(SynParse.Sequence, items:=
                  SynParse.List(
                    NEW(SynParse.GivenDelimiter, delim:='_'),
                    SynParse.Store(3, NEW(SynParse.Integer, 
                                          Build:=MetaParser.IntegerToTree)))),
                Build:=Builder.Storage)))));

  (*  
     paramList ::= 
     { [ "<" paramList2_1 ">" ] :: select1 [] }
  *)
  metaParser.Add(gramParamList.name,
          NEW(SynParse.Choice, choice :=
            SynParse.List(
              NEW(SynParse.Action, grammar :=                  
                NEW(SynParse.Sequence, items :=
                  SynParse.List( 
                    NEW(SynParse.GivenKeyword,
                        key := SynScan.GetKeywordName(keyLess)),
                    SynParse.Store(1,gramParamList2),
                    NEW(SynParse.GivenKeyword,
                        key := SynScan.GetKeywordName(keyMore)))),
                Build := Fetch1),
              NEW(SynParse.Sequence, items := NIL))));

   (*
     paramList2 ::= 
     { [ "_" int_1 { [ "," paramList2_2 ] :: select2 [] }_2 ] :: cons [] }
   *)
  metaParser.Add(gramParamList2.name,
          NEW(SynParse.Choice, choice :=
            SynParse.List(
              NEW(SynParse.Action, grammar :=
                NEW(SynParse.Sequence, items :=
                  SynParse.List(
                    NEW(SynParse.GivenDelimiter, delim := '_'),
                    SynParse.Store(1,NEW(SynParse.Integer,
                                         Build:= MetaParser.IntegerToTree)),
                    SynParse.Store(2,NEW(SynParse.Choice, choice :=
                      SynParse.List(
                        NEW(SynParse.Action, grammar :=
                          NEW(SynParse.Sequence, items :=
                            SynParse.List(
                              NEW(SynParse.GivenDelimiter, delim := ',' ),
                              SynParse.Store(2, gramParamList2))),
                          Build := Fetch2),
                        NEW(SynParse.Sequence, items := NIL )))))),
                Build := Builder.ConsParam),
              NEW(SynParse.Sequence, items := NIL))));
                                                                              
  (* gramExpBase ::=
     { ide string "ide" "name" "int" "EOF" "real" "char" "string"
     gramExpSequence gramExpChoice gramExpParens }
  *)
  metaParser.Add(gramExpBase.name,
          NEW(SynParse.Choice, choice:=
            SynParse.List(
              NEW(SynParse.Action, grammar :=
                NEW(SynParse.Sequence, items :=
                  SynParse.List(
                    SynParse.Store(1,NEW(SynParse.Identifier, 
                                         Build:= MetaParser.IdentifierToTree)),
                    SynParse.Store(2,gramParamList))),
                Build := Builder.GramIde2 ),
                NEW(SynParse.QuotedString, 
                  Build:=Builder.GramString),
                NEW(SynParse.GivenKeyword, 
                    key:=SynScan.GetKeywordName(keyEof), 
                    Build:=Builder.GramKeyEof),
                NEW(SynParse.GivenKeyword, 
                    key:=SynScan.GetKeywordName(keyIde), 
                    Build:=Builder.GramKeyIde),
                NEW(SynParse.GivenKeyword, 
                    key:=SynScan.GetKeywordName(keyName), 
                    Build:=Builder.GramKeyName),
                NEW(SynParse.GivenKeyword, 
                    key:=SynScan.GetKeywordName(keyInt), 
                    Build:=Builder.GramKeyInt),
                NEW(SynParse.GivenKeyword, 
                    key:=SynScan.GetKeywordName(keyReal), 
                    Build:=Builder.GramKeyReal),
                NEW(SynParse.GivenKeyword, 
                    key:=SynScan.GetKeywordName(keyChar), 
                    Build:=Builder.GramKeyChar),
                NEW(SynParse.GivenKeyword, 
                    key:=SynScan.GetKeywordName(keyString), 
                    Build:=Builder.GramKeyString),
                gramExpSequence,
                gramExpChoice,
                gramExpParens)));
  
  (* gramExpSequence ::=
     [ "[" gramExpList "]" ]
  *)
  metaParser.Add(gramExpSequence.name,
          NEW(SynParse.Action, grammar:=
            NEW(SynParse.Sequence, items:=
              SynParse.List(
                NEW(SynParse.GivenDelimiter, delim:='['),
                SynParse.Store(1, gramExpList),
                NEW(SynParse.GivenDelimiter, delim:=']'))),
            Build:=Builder.GramExpSequence));
  
  (* gramExpChoice ::=
     [ "{" gramExpList "}" ]
  *)
  metaParser.Add(gramExpChoice.name,
          NEW(SynParse.Action, grammar:=
            NEW(SynParse.Sequence, items:=
              SynParse.List(
                NEW(SynParse.GivenDelimiter, delim:='{'),
                SynParse.Store(1, gramExpList),
                NEW(SynParse.GivenDelimiter, delim:='}'))),
            Build:=Builder.GramExpChoice));
  
  (* gramExpParens ::=
     [ "(" gramExp
     { [ "*" { [ "_" int gramExp ] gramExp } ] [] } 
     ")" ]
  *)
  metaParser.Add(gramExpParens.name,
          NEW(SynParse.Action, grammar:=
            NEW(SynParse.Sequence, items:=
              SynParse.List(
                NEW(SynParse.GivenDelimiter, delim:='('),
                SynParse.Store(1, gramExp),
                SynParse.Store(6,NEW(SynParse.Choice, choice :=
                  SynParse.List(
                    NEW(SynParse.Action, grammar:=
                      NEW(SynParse.Sequence, items:=
                        SynParse.List(
                          NEW(SynParse.GivenKeyword, 
                              key:=SynScan.GetKeywordName(keyStar)),
                          SynParse.Store(5,NEW(SynParse.Choice, choice:=
                            SynParse.List(
                              NEW(SynParse.Action, grammar:=
                                NEW(SynParse.Sequence, items:=
                                  SynParse.List(
                                    NEW(SynParse.GivenDelimiter, 
                                        delim:='_'),
                                    SynParse.Store(4, NEW(SynParse.Integer, 
                                                       Build:=MetaParser.IntegerToTree)),
                                    SynParse.Store(3, gramExp))),
                                Build:= Builder.GramExpIterPos),
                              NEW(SynParse.Action, grammar:=
                                SynParse.Store(3, gramExp),
                                Build:= Builder.GramExpIterNoPos)))))),
                      Build:=Builder.GramExpIter),
                    NEW(SynParse.Action, 
                        grammar:=NEW(SynParse.Sequence, items:=NIL),
                        Build:=Builder.GramExpBase)))),
                NEW(SynParse.GivenDelimiter, delim:=')'))),
            Build:=Builder.GramExpParens));
  
  (* gramExpList ::=
     { [ gramExp gramExpList ] [] }
  *)
  metaParser.Add(gramExpList.name,
          NEW(SynParse.Choice, choice :=
            SynParse.List(
              NEW(SynParse.Action, grammar:=
                NEW(SynParse.Sequence, items:=
                  SynParse.List(
                    SynParse.Store(1, gramExp),
                    SynParse.Store(2, gramExpList))),
                Build:=Builder.GramList),
              NEW(SynParse.Sequence, items:=NIL))));
END Setup;

BEGIN
END Constructor.
  

