(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: Main.m3,v 1.2 2001-09-19 15:05:08 wagner Exp $ *)

MODULE Main;
IMPORT TokParams;
IMPORT FileWr, Wr, Thread, OSError;
IMPORT FileRdErr;
IMPORT Pathname;
IMPORT LexFmt;
IMPORT Env;
<* FATAL Thread.Alerted, Wr.Failure, OSError.E *>
VAR
  tp := TokParams.Get("lex", ".l", "Lex.i3");
  tok := TokParams.ReadTokens(tp);
  rd := FileRdErr.Open(tp.source);
  lexFmt :=  LexFmt.New(rd, tok, tp.outBase, tp.tokOutBase);
  wr: Wr.T;
BEGIN
  wr := FileWr.Open(tp.out);
  lexFmt.writeInterface(wr);
  Wr.Close(wr);
  
  wr := FileWr.Open(Pathname.ReplaceExt(tp.out, "m3"));
  lexFmt.writeModule(wr);
  Wr.Close(wr);
  
  IF Env.Get("lexDEBUG")#NIL THEN lexFmt.test(); END;
END Main.
