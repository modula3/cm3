(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* Last modified on Fri Nov  5 10:20:35 1993 by luca                   *)
(*      modified on Mon Jun 29 19:04:13 1992 by knaff          *)
(*                                                                           *)
(* Parts Copyright (C) 1997, Columbia University                             *)
(* All rights reserved.                                                      *)
(*
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Mon Aug  4 14:54:35 1997
 *)

INTERFACE SynParse;
IMPORT SynWr, SynLocation, SynScan;

(* Part of the synex package: a parser generator for extensible grammars.
   See also the metasyn package *)

(* This interface allow the construction of grammar graphs that can be
   modified even during the parsing process. It provides an LL(1)
   parser for such graphs.
*)

EXCEPTION Fail;

TYPE
  T <: TPublic;
  TPublic = 
    OBJECT
      (* READONLY *) stack: REF ARRAY OF Tree;
      (* The stack of partial parsing results. *)

    METHODS
      Grammar(): GrammarEnv;
      (* The grammar associated with this parser. To parse a specific
         non-terminal of this grammar, give it as a parameter to Read
         or ReadNonTerminal. *)

      Scanner(): SynScan.T;
      (* The scanner associated with this parser. *)

      Writer(): SynWr.T;
      (* The writer associated with this parser. *)

      ReadNonTerminal(named: TEXT): Tree
           RAISES {Fail, SynScan.Fail, SynScan.NoReader};
       (* Parses the non-terminal of given name (having zero arguments). *)

      Read(gram: Grammar; base: INTEGER:=0): Tree 
           RAISES {Fail, SynScan.Fail, SynScan.NoReader};
      (* Parse the grammar "gram", whose non-terminals are defined in "env".
         Gives a relatively meaningful syntax error if parsing fails.
         Raises SynScan.NoReader if no more input is available for parsing.
         Raises Fail only on serious errors, not just if parsing of
         grammar fails. A non-zero "base" for the first Stack frame is 
         for advanced use only. *)

      Lookup(name: TEXT; VAR (*out*) args: Args): Grammar RAISES {Fail};
        (* Find the definition of "name" or give an error. *)

      Add(name: TEXT; grammar: Grammar; 
          args : Args := NIL ) RAISES {Fail};
        (* Add a definition, and give an error if redefining. *)

      Extend(name: TEXT; grammar: Grammar; args: Args := NIL ) 
        RAISES {Fail};
        (* Extend an existing definition name=def changing it to
           name=Choice(def,grammar), or give an error if not found. *)

      ExtendIter(name: TEXT; 
                 iterPosPresent: BOOLEAN;
                 iterPos: INTEGER;
                 grammar: Grammar;
                 args: Args := NIL ) RAISES {Fail};
        (* Extend an existing definition name=Iter(base,iter) changing it to
           name=Iter(base,Choice(iter,grammar)), or give an error if not found.
           If "iterPosPresent" is true then "iterPos" must match the existing 
           "accumPosition" (just some extra consistency checking). *)

    END;
    (* A parser. *)

  Args = REF ARRAY OF INTEGER;    

  Tree = SynLocation.Located;
  (* The result of parsing is a Tree . Clients should define 
     their parse-tree or abstract-syntax-tree data structure to be 
     subtypes of Tree.  *)

  Grammar <: Tree;
  (* A Grammar is a data structure describing an LL(1) grammar for parsing.
     The 1 in LL(1) refers to "1 token" where a token is defined as in
     the interface SynScan. A grammar must be LL(1), but this is not
     checked. In particular, a left-recursive grammar will diverge, and
     a grammar attempting more-than-LL(1) backtracking will cause an
     error during parsing. 
     The various grammar constructions are described below. *)

  (* The main parser routine ("Read") takes a Grammar, and a GrammarEnv in
     which to search for the non-terminal symbols of that Grammar. It returns
     a Tree. Partial parsing result are kept on a stack ("Stack") organized
     in frames; the Stack index which is the "base" of the current frame is 
     passed to client "Action" routines.
     The semantics of Read and the use of Stack are described for each
     subtype of Grammar below. See the interface SynScan for the lexical
     details.
  *)
  
  GrammarEnvRoot <: ROOT;
  GrammarEnv =
    GrammarEnvRoot BRANDED "SynParse.GrammarEnv" OBJECT
    END;
  (* A full grammar definition is given by a pair of a Grammar and a GrammarEnv;
     the latter is an environment associating non-terminals (including the ones 
     used in the Grammar) to Grammars. One can start with an empty GrammarEnv
     (see NewEnv()) and use the Add method to add definitions. *)

  GrammarList =
    Tree BRANDED "SynParse.GrammarList" OBJECT
      first: Grammar;
      rest: GrammarList;
    END;
  (* Just a list of grammars. *)

  NonTerminal =
    Grammar BRANDED "SynParse.NonTerminal" OBJECT
      name: TEXT;
      args: REF ARRAY OF INTEGER;
    END;
  (* A non-terminal grammar symbol, which must be bound to a grammar
     description (via a GrammarEnv). Parsing a non-terminal is the
     same as setting-up a new stack frame and then parsing the associated 
     description; the returned Trees are the same. At the end, the new
     stack frame is cleared and popped. Parsing fails if parsing the 
     associated description fails.  *)

  Storage =
    Grammar BRANDED "SynParse.Storage" OBJECT
      item: Grammar;
      position: INTEGER; (* >= 0 *)
    END;
  (* A storage grammar. Parsing this is the same as parsing "item", but
     the resulting tree is stored at "position" relative to the
     current stack frame, and a NIL tree is returned. Parsing fails if
     parsing "item" fails. Errors are given when storing outside
     of the current frame (with a negative position), when storing twice
     at the same position, and when storing to a read-only position
     (within a Choice or Iter branch).
     The used positions do not have to be contiguous. *)

  Action =
    Grammar BRANDED "SynParse.Action" OBJECT
      grammar: Grammar;
      Build: PROCEDURE(self: Action; g: T; base: INTEGER; 
        READONLY info: SynLocation.Info): Tree RAISES {Fail}
        := BuildNoAction;
    END;
  (* An action grammar. Parsing this is the same as parsing "grammar",
     but the result is discarded (use Storage to preserve it).
     After that, Build is invoked and its result is returned. Build will 
     normally fetch data from the Stack, using the "base" parameter as the base 
     of the current Stack frame, and adding to it the "position" that had been
     given to Storage. The default BuildNoAction is a no-op and returns a NIL 
     tree. Parsing fails if the parsing of "grammar" fails. *)

  GivenKeyword =
    Grammar BRANDED "SynParse.GivenKeyword" OBJECT
      key: TEXT;
      Build: PROCEDURE(self: GivenKeyword; g: T;
	READONLY info: SynLocation.Info): Tree RAISES {Fail}
	:= BuildNoGivenKeyword;
    END;
  (* A grammar to parse a given keyword "key". Parsing this causes the
     Build procedure to be invoked, and its result returned. The default
     is a no-op and returns a NIL tree. Parsing fails if the given keyword
     cannot be scanned. *)

  GivenIdentifier =
    Grammar BRANDED "SynParse.GivenIdentifier" OBJECT
      ide: TEXT;
      Build: PROCEDURE(self: GivenIdentifier; g: T;
	READONLY info: SynLocation.Info): Tree RAISES {Fail}
	:= BuildNoGivenIdentifier;
    END;
  (* A grammar to parse a given non-keyword identifier "ide". Parsing this 
     causes the Build procedure to be invoked, and its result returned. 
     The default is a no-op and returns a NIL tree. Parsing fails if the
     given identifier cannot be scanned. *)

  GivenName =
    Grammar BRANDED "SynParse.GivenName" OBJECT
      text: TEXT;
      Build: PROCEDURE(self: GivenName; g: T;
	READONLY info: SynLocation.Info): Tree RAISES {Fail}
	:= BuildNoGivenName;
    END;
  (* A grammar to parse a given identifier or keyword "text". Parsing this 
     causes the Build procedure to be invoked, and its result returned. 
     The default is a no-op and returns a NIL tree. Parsing fails if
     the given identifier or keyword cannot be scanned. *)

  GivenDelimiter =
    Grammar BRANDED "SynParse.GivenDelimiter" OBJECT
      delim: CHAR;
      Build: PROCEDURE(self: GivenDelimiter; g: T;
	READONLY info: SynLocation.Info): Tree RAISES {Fail}
	:= BuildNoGivenDelimiter;
    END;
  (* A grammar to parse a given delimiter "delim". Parsing this 
     causes the Build procedure to be invoked, and its result returned. 
     The default is a no-op and returns a NIL tree. Parsing fails if
     the given delimiter cannot be scanned. *)

  Eof =
    Grammar BRANDED "SynParse.Eof" OBJECT
      Build: PROCEDURE(self: Eof; g: T;
	READONLY info: SynLocation.Info): Tree RAISES {Fail}
	:= BuildNoEof;
    END; 
  (* A grammar to parse an end-of-file marker. Parsing this 
     causes the Build procedure to be invoked, and its result
     returned. The default is a no-op and returns a NIL tree. 
     Parsing fails if an end-of-file marker cannot be scanned. *)

  Identifier =
    Grammar BRANDED "SynParse.Identifier" OBJECT
      Build: PROCEDURE(self: Identifier; g: T; name: TEXT; 
	READONLY info: SynLocation.Info): Tree RAISES {Fail}
	:= BuildNoIdentifier;
    END;
  (* A grammar to parse any identifier from the input. Parsing this 
     causes the Build procedure to be invoked with the scanned identifier
     as a parameter, and its result returned. The default is a no-op and 
     returns a NIL tree. Parsing fails if no identifier can be scanned. *)

  Name =
    Grammar BRANDED "SynParse.Name" OBJECT
      Build: PROCEDURE(self: Name; g: T; name: TEXT; 
	READONLY info: SynLocation.Info): Tree RAISES {Fail}
	:= BuildNoName;
    END;
  (* A grammar to parse any identifier or keyword from the input. Parsing this 
     causes the Build procedure to be invoked with the scanned entity
     as a parameter, and its result returned. The default is a no-op and 
     returns a NIL tree. Parsing fails if no identifier or keyword can be
     scanned. *)

  QuotedChar =
    Grammar BRANDED "SynParse.QuotedChar" OBJECT
      Build: PROCEDURE(self: QuotedChar; g: T; char: CHAR; 
	READONLY info: SynLocation.Info): Tree RAISES {Fail}
	:= BuildNoQuotedChar;
    END;
  (* A grammar to parse any quoted character from the input. Parsing this 
     causes the Build procedure to be invoked with the scanned entity
     as a parameter, and its result returned. The default is a no-op and 
     returns a NIL tree. Parsing fails if no quoted char can be scanned. *)

  Integer =
    Grammar BRANDED "SynParse.Integer" OBJECT
      Build: PROCEDURE(self: Integer; g: T; int: INTEGER; 
	READONLY info: SynLocation.Info): Tree RAISES {Fail}
	:= BuildNoInteger;
    END;
  (* A grammar to parse any integer from the input. Parsing this 
     causes the Build procedure to be invoked with the scanned entity
     as a parameter, and its result returned. The default is a no-op and 
     returns a NIL tree. Parsing fails if no integer can be scanned. *)

  Real =
    Grammar BRANDED "SynParse.Real" OBJECT
      Build:PROCEDURE(self: Real; g: T; real: LONGREAL; 
	READONLY info: SynLocation.Info): Tree RAISES {Fail}
	:= BuildNoReal;
   END;
  (* A grammar to parse any real number from the input. Parsing this 
     causes the Build procedure to be invoked with the scanned entity
     as a parameter, and its result returned. The default is a no-op and 
     returns a NIL tree. Parsing fails if no real can be scanned. *)

  QuotedString =
    Grammar BRANDED "SynParse.QuotedString" OBJECT
      Build: PROCEDURE(self: QuotedString; g: T; string: TEXT; 
	READONLY info: SynLocation.Info): Tree RAISES {Fail}
	:= BuildNoQuotedString;
    END;
  (* A grammar to parse any quoted string from the input. Parsing this 
     causes the Build procedure to be invoked with the scanned entity
     as a parameter, and its result returned. The default is a no-op and 
     returns a NIL tree. Parsing fails if no quoted string can be scanned. *)

  Sequence =
    Grammar BRANDED "SynParse.Sequence" OBJECT
      items: GrammarList
    END;
  (* A grammar to parse a (possibly empty) sequence of grammars, in the given 
     order.
     The result is always the NIL tree (Storage and Action grammars should
     be used to store and recover partial results). Parsing fails if parsing 
     any of the grammars in the list fails. The parser can backtrack from
     this failure only if no token has been actually scanned, according to the
     definition of LL(1) *)

  Choice =
    Grammar BRANDED "SynParse.Choice" OBJECT
      choice: GrammarList;
	(* returns what the succesful choice returns *)
    END;
  (* A grammar to parse one of a number (possibly zero) of branch grammars, 
     to be tried in the order given. Parsing this is the same as parsing the 
     first successful branch, provided that all the previous ones have 
     failed without actually scanning any tokens (i.e. respecting the 
     definition of LL(1)). The result tree is the result tree of the successful
     branch (if any). Parsing fails if all the grammars in the list fail.
     Within each branch it is legitimate to read from all Storage locations 
     within the current frame, but one can write only to Storage locations 
     larger than the largest one used when that branch was entered.
     Every time a branch fails, the Storage binding established within that 
     branch are undone. *)

  Iter =
    Grammar BRANDED "SynParse.Iter" OBJECT
      accum: BOOLEAN;
      accumPosition: INTEGER;
      base, iter: Grammar;
    END;
  (* An iteration grammar. Parsing it is the same as parsing "base" once, and
     then parsing "iter" as many times as possible until it fails. If "accum"
     is true, the result of every iteration is stored at the relative location 
     "accumPosition" in the current Stack frame. Actions should be attached
     to the "iter" grammar to capture the intermediate results. The result
     is the result of parsing "base", or of parsing the last "iter".
     Parsing fails if parsing "base" fails. 
     Within each "iter" execution it is legitimate to read from all 
     Storage locations within the current frame, but one can write only to 
     Storage locations larger than the largest one used when "iter" was 
     entered. At the end of every "iter" execution, the Storage binding 
     established within that "iter" are undone. *)

     (* Iter(a,b) can be made to produces left-associative parse trees:    
	a, ab, (ab)b, ((ab)b)b, ...
       which are not otherwise constructable by Sequence and Choice alone.
       Examples of use of Iter, using [] for Sequence and ".." for terminals:
	Iter(ide,"'")
		ide, ide', (ide')', ...
	Iter(ide,["("ide")"])
		ide, ide(ide), (ide(ide))(ide), ...
	Iter(ide,["-"ide])
		ide, ide"-"ide, (ide-ide)-ide, ...
	Iter([], "$")
		$, ($$), (($$)$), ...
	Iter(ide, [";" ide])
		ide, ide;ide, (ide;ide);ide, ...
    *)

PROCEDURE Setup();
(* To be called before any other use of this module. *)

PROCEDURE PackageSetup();
(* Call all the Setup functions in this package. *)

VAR (* READONLY *) noArgs : Args;

PROCEDURE New(swr: SynWr.T; env: GrammarEnv; 
              stackSize: CARDINAL := 10240): T;
(* A new parsers, including a new scanner and the specified grammar
   and stack. Error messages are written to swr. 
   Use single-threaded. Separate parsers can be used
   concurrently in separate threads. *)

PROCEDURE NewEnv():  GrammarEnv;
(* Creates a new, empty, grammar env. *)

PROCEDURE List(item1,item2,item3,item4,item5,item6,item7,item8,
    item9, item10, item11, item12, item13, item14, item15, item16,
    item17, item18, item19, item20: Grammar:=NIL; 
  rest: GrammarList:=NIL): GrammarList;
(* A utility procedure to create lists of grammars. *)

PROCEDURE Store(position: INTEGER; grammar: Grammar): Grammar;
(* A utility procedure to create a Storage grammar with item=grammar and
   position=position. *)

PROCEDURE Msg(p: T; msg: TEXT);
  (* Print a message to the parser's writer, with newline and flush. *)

PROCEDURE Fault(p: T; msg: TEXT) RAISES {Fail};
  (* Print a message to the parser's writer and raise Fail. *)

(* Default Build methods all returning NIL *)
PROCEDURE BuildNoAction(self: Action; g: T; base: INTEGER; READONLY info: SynLocation.Info): Tree;
PROCEDURE BuildNoGivenKeyword(self: GivenKeyword; g: T; READONLY info: SynLocation.Info): Tree;
PROCEDURE BuildNoGivenIdentifier(self: GivenIdentifier; g: T; READONLY info: SynLocation.Info): Tree;
PROCEDURE BuildNoGivenName(self: GivenName; g: T; READONLY info: SynLocation.Info): Tree;
PROCEDURE BuildNoGivenDelimiter(self: GivenDelimiter; g: T; READONLY info: SynLocation.Info): Tree;
PROCEDURE BuildNoIdentifier(self: Identifier; g: T; name: TEXT; READONLY info: SynLocation.Info): Tree;
PROCEDURE BuildNoName(self: Name; g: T; name: TEXT; READONLY info: SynLocation.Info): Tree;
PROCEDURE BuildNoQuotedChar(self: QuotedChar; g: T; char: CHAR; READONLY info: SynLocation.Info): Tree;
PROCEDURE BuildNoInteger(self: Integer; g: T; int: INTEGER; READONLY info: SynLocation.Info): Tree;
PROCEDURE BuildNoReal(self: Real; g: T; real: LONGREAL; READONLY info: SynLocation.Info): Tree;
PROCEDURE BuildNoQuotedString(self: QuotedString; g: T; string: TEXT; READONLY info: SynLocation.Info): Tree;
PROCEDURE BuildNoEof(self: Eof; g: T; READONLY info: SynLocation.Info): Tree;

END SynParse.


