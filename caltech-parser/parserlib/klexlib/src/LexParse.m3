(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: LexParse.m3,v 1.1.1.1 2001-09-19 11:40:10 wagner Exp $ *)

MODULE LexParse;
IMPORT NFA;
IMPORT Rd;
IMPORT TokSpec;
IMPORT PragmaRead;
IMPORT FileRdErr;
IMPORT Text;
IMPORT TextList;
IMPORT CharRange;
IMPORT Pragma, Thread;
IMPORT RegExpParseNFA;
(* IMPORT Wr, Fmt, Stdio; *)
<* FATAL Thread.Alerted, Rd.Failure, Rd.EndOfFile *>
REVEAL
  T = Public BRANDED OBJECT
    outNum: INTEGER := 0;
    context: RegExpParseNFA.T;
    (* hackOut: Wr.T; *)
  END;
TYPE
  PragType = {Macros, Exprs};
  SelfPragma = Pragma.T OBJECT
    self: T;
  END;

PROCEDURE Stmt(self: T; pt: PragType; name, expr: TEXT) =
  VAR
    nfa: NFA.T;
  BEGIN   
    (* Wr.PutText(Stdio.stdout, name & "=" & expr & "\n"); *)
    nfa := self.context.parseText(expr);
    IF pt = PragType.Exprs THEN
      INC(self.outNum);
      self.names := TextList.Cons(name, self.names);
      self.n := NFA.Or(self.n, NFA.Output(nfa, self.outNum), FALSE);
(*Wr.PutText(Stdio.stdout,name&" with ID = " & Fmt.Int(self.outNum) & "\n");*)
    ELSE
      self.context.putMacro(name, nfa);
    END;
    (* Wr.PutText(self.hackOut, lname & ":=" & expr & "\n"); *)
  END Stmt;

PROCEDURE ParseStmt(self: T; rd: Rd.T; pt: PragType) =
  VAR
    line := Rd.GetLine(rd);
    c: CHAR;
    p3 := Text.Length(line);
    p1 := 0;
    p2 := 0;
  PROCEDURE Error() =
    BEGIN
      FileRdErr.E(rd, "name and expression required");
    END Error;
  BEGIN
    REPEAT
      IF p1 = p3 THEN Error() END;
      c := Text.GetChar(line, p1);
      INC(p1);
    UNTIL c IN CharRange.WhiteSpace;
    DEC(p1);
    p2 := p1;
    REPEAT
      INC(p2);
      IF p2 = p3 THEN Error() END;
      c := Text.GetChar(line, p2);
    UNTIL NOT c IN CharRange.WhiteSpace;
    WHILE Text.GetChar(line, p3-1) IN CharRange.WhiteSpace DO DEC(p3);END;
    Stmt(self, pt,
         Text.Sub(line, 0, p1),
         Text.Sub(line, p2, p3 - p2));
  END ParseStmt;

PROCEDURE ParseMacros(p: SelfPragma; rd: Rd.T) =
  BEGIN ParseStmt(p.self, rd, PragType.Macros); END ParseMacros;
PROCEDURE ParseExprs(p: SelfPragma; rd: Rd.T) =
  BEGIN ParseStmt(p.self, rd, PragType.Exprs); END ParseExprs;
  
PROCEDURE New(rd: Rd.T; tok: TokSpec.T; ): T =
  VAR
    self := NEW(T);
    prag := NEW(PragmaRead.T).init();
    parseMacros := NEW(SelfPragma, self := self, do := ParseMacros);
    parseExprs := NEW(SelfPragma, self := self, do := ParseExprs);
  BEGIN
    (* self.hackOut := FileWr.Open("old_lex_hack"); *)
    self.n := NFA.Empty();
    self.context := NEW(RegExpParseNFA.T).init();
    self.context.putMacro("%char", NFA.FromRange(tok.charTokens));
    self.names := NIL;
    prag.add(parseMacros, "%macro");
    prag.add(parseExprs, "%expr");
    prag.add(parseExprs, "");
    prag.apply(rd);
    (* Wr.Close(self.hackOut); *)
    self.names := TextList.ReverseD(self.names);
    (*    yTab.Init();
          EVAL yTab.exprMacros.put("%char", NFA.FromRange(tok.charTokens));
          yTab.Parse();
          self.n := yTab.result;   *)
    RETURN self;
  END New;
BEGIN
END LexParse.
