(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

INTERFACE ObliqOnline;
IMPORT Rd, SynWr, Obliq;

TYPE 
  T <: TPublic;
  TPublic = 
    OBJECT
      env: Obliq.Env;
      swr: SynWr.T;
    END;
(* An Obliq interpreter. The env field is the current interpreter environment:  
   it can be used to investigate the state of the interpreter. The swr field is 
   the writer the interpreter is using. *)

PROCEDURE Setup();
(* To be called once before any other use of is interface. *)

PROCEDURE New(greetings: TEXT:=""; swr: SynWr.T:=NIL; loadDotObliq: 
    BOOLEAN:=TRUE; env: Obliq.Env := NIL): T; 
(* An interactive interpreter (a read-eval-print loop), producing 
   output to swr (NIL = standard output). If loadDotObliq is true, 
   the file ~/.obliq is loaded. Returns an interpreter that can 
   be given input repeatedly via Interact. Env is the initial environment 
   for the interpreter; if env is NIL, Obliq.emptyEnv is used. Note 
   that the collection of registered built-in libraries, ObLib.libraries, 
   is global and shared by all interpreters. The env field of the 
   result can be used to extract partial or final results, by Obliq.Lookup. 
   *) 

PROCEDURE Interact(interpreter: T; rdName: TEXT:=""; rd: Rd.T:=NIL; 
  closeRd: BOOLEAN:=FALSE; generateEOF: BOOLEAN := TRUE);
(* Push a new reader rd (NIL = standard input) as a source of characters 
   for the interpreter. Returns when rd is exhausted. 
   This procedure can be called repeatedly on the same
   interpreter, each time reusing the previous interpreter state.
   The rdName should be used to identify the reader (e.g. its file name, if any)
   for error reporting purposes. If closeRd is true, Rd.Close(rd) is 
   executed when rd is exhausted. If genereteEOF is true, an Eof lexical 
   token is generated when rd is exhausted (This means that, according to
   the Obliq syntax, the reader cannot end with an incomplete Obliq phrase.
   Moreover, if Eof is generated, unbalanced comments and quotes do not 
   propagate from one reader to the next). *)  

END ObliqOnline.
