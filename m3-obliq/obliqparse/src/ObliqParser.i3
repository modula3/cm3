
(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

INTERFACE ObliqParser;
IMPORT Rd, SynWr, SynLocation, SynParse, ObTree, ObValue, Obliq;
FROM ObValue IMPORT Error, Exception;

(* A simple Obliq read-eval loop.

   VAR swr := SynWr.out; 
     (* Sending output to stdout. *)
   VAR p := ObliqParser.New(swr);
     (* An Obliq parser writing messages to swr. *)
   ObliqParser.ReadFrom(readerName, rd, TRUE);
     (* Set up the parser to read from rd and close the reader when done. *)
   TRY
     VAR env := Obliq.emptyEnv;
     LOOP
       EVAL ObliqParser.EvalPhrase(ObliqParser.ParsePhrase(p), (*in-out*) env);
     END;
   EXCEPT
     SynParse.Eof => (* all done *);
     ObValue.Error(packet) => Obliq.ReportError(swr, packet);
     ObValue.Exception(packet) => Obliq.ReportException(swr, packet);
   END;
*)

EXCEPTION Eof;

TYPE
  T = SynParse.T;
  Phrase = ObTree.Phrase;
  Env = Obliq.Env;
  Location = SynLocation.T;

  PROCEDURE PackageSetup();
  (* To be called at least once before any other use of the obliqparse package.
     It calls all the required PackageSetup routines of other packages,
     including Obliq.PackageSetup for obliqrt-ip. *)

  PROCEDURE New(swr: SynWr.T): T;
  (* Return an obliq parser that writes error messages to swr. The input 
     character stream must be provided by ReadFrom. Separate parsers can
     be used by separate threads, but each parser must be single-threaded. *)

  PROCEDURE ReadFrom(p: T; rdName: TEXT; rd: Rd.T; closeRd: BOOLEAN;
    generateEOF: BOOLEAN := TRUE);
  (* Push a new reader rd as a source of characters for the parser. The new
     reader will be used when all the previous ones are exhausted. The 
     name should be used to identify the reader (e.g. its file name, if any)
     for error reporting purposes. If closeRd is true, Rd.Close(rd) is 
     executed when rd is exhausted. If genereteEOF is true, an Eof lexical 
     token is generated when rd is exhausted (This means that, according to
     the Obliq syntax, the reader cannot end with an incomplete Obliq phrase.
     Moreover, if Eof is generated, unbalanced comments and quotes do not 
     propagate from one reader to the next). *)  

  PROCEDURE ParseTerm(p: T): Obliq.Term RAISES {Error, Eof};
  (* Parse according to the syntax of a single obliq term. *)

  PROCEDURE ParsePhrase(p: T): Obliq.Phrase RAISES {Error, Eof};
  (* Parse according to the syntax of a single obliq phrase. *)

  PROCEDURE EvalPhrase(p: T; phrase: Phrase; VAR (*in-out*) env: Env; 
    loc: Location:=NIL): Obliq.Val(*or NIL*) RAISES {Error, Exception};
  (* Evaluates load and module phrases (whose evaluation requires interaction
     with the parser) and invokes Obliq.EvalPhrase for the other kinds of
     phrases. "env" must be a legal environment. Produces an enriched 
     legal environment and/or a value, or an exception. 
     The result environment is enriched with new bindings when phrase is a
     definition. The result value is non-NIL when the phrase is a term
     or definitions. *)

END ObliqParser.
