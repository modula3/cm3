(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* Last modified on Fri Nov  5 11:13:41 1993 by luca                       *)
(*      modified on Thu Jun 25 01:51:09 1992 by knaff          *)
(*                                                                           *)
(* Parts Copyright (C) 1997, Columbia University                             *)
(* All rights reserved.                                                      *)
(*
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Mon Aug  4 15:05:14 1997
 *)

INTERFACE MetaParser ;
IMPORT SynWr, SynLocation, SynParse, Rd, SynScan, TextRefTbl;

(* =============================================================================
   | This interface is the main interface for the MetaParser package
   | See also the metaParser manpage.
   |
   | This metaparser can be used to produce a parser for
   | an extensible grammar, from a BNF-like description of the grammar.
   | It is based on the synex package.
   *)
(*
   | Interpretation of rd and fileName
   | ---------------------------------
   |
   | Several procedures and methods in this interface have arguments to specify 
   where their input should come from. These arguments come in couples:
   A reader and a fileName. 
   |
   | If both fileName and rd are specified (i.e. non-NIL), the actual input will 
   come from rd, and the fileName is used when displaying error-messages. 
   (When there is a Syntax error, the scanner displays an error message, containing
   the line-number and the FILENAME where the error occured. See SynScan.i3 for 
   details) If FileName is equal to "", this means that the input is toplevel, 
   and this prevents the reader from being popped when there is an error. (To 
   avoid losing stdin)
   |
   | If only a fileName is specified, then it is opened, and the input will come
   from this file. It will also be used for the error messages.
   |
   | If only rd is specified, the input will come from rd, and the fileName used
   to display errror-messages will be "(unknown file)" if the rd is different from
   stdin, and "" if rd is equal to stdin.
   |
   | If neither a reader, nor a fileName will be specified, input will come from
   Stdio.stdin, and "" will be used while displaying error messages.  
   |
   | End of file handling
   | --------------------
   | When the scanner reaches the end of a file, it generates and EOF token if the
   | genereateEOF flag has been set for this reader, and then it pops the reader 
   | from the reader stack, and continues with the previous reader.
   | Continuing parsing after popping the last reader causes a run-time error.
   | See SynScan.i3 for details.
   |
   |===============================================================================
*)


TYPE
  ActionProc = PROCEDURE(self: SynParse.Action; p: SynParse.T; base: INTEGER; 
                         READONLY info: SynLocation.Info)
                         : SynParse.Tree RAISES {SynParse.Fail} ;
  (* 
     | type of an action procedure
     |
     | base is the frame-pointer for the SynParse.Stack (see SynParse.i3)
     | SynParse.Stack[base+i] contains the argument stored at the position i
     |
     | info is a position descriptor to be used to display errors.
     | Transform it into an SynLocation.T using SynLocation.NewLineLocation(info), and
     stick it into the location field of the parse-tree you return. When some other
     Action won't be "happy" with this parse-tree, it can find out form which 
     place in the text this parse-tree came from, in order to display the error.
     |
     | In order to display the location, use:
     |      SynLocation.PrintLocation(SynOut.out,tree.location,0);      
     |
  *)




  ActionProcEntry = RECORD
    name : TEXT ;                 (* name of the action *)
    proc : ActionProc ;           (* procedure which implements the action *) 
  END;
  (*
    | entry in the "source" action table
    | the "source" action table is juste an array of these.
    | To use it, it must first be transformed into a hash-table, using the 
    | procedure TableFromArray
  *)

  ActionTable = TextRefTbl.T ;
  (* Hashed action-table to be passed to ReadGFile. It is genereated from
     | an array of ActionProcEntry's using ReadGFile
  *)

  ActionProcTable = ARRAY OF ActionProcEntry ;

  Grammar <: Grammar_public;
  Grammar_public = BRANDED "MetaParser.Grammar_public" OBJECT
    env: SynParse.GrammarEnv := NIL;
    (* environment of non-terminals *)
    keySet: SynScan.KeywordSet := NIL;
    (* set of keywords of this grammar *)
  END; (* object *)
  (* grammar object *)

  ClauseList = 
    SynParse.Tree BRANDED "MetaParser.ClauseList" OBJECT
    ide: TextNode;
    args: SynParse.Args;
    extend, extendIter, iterPosPresent: BOOLEAN; iterPos: INTEGER;
    gram: SynParse.Grammar;
    rest:ClauseList;
  END;

  TextNode =
    SynParse.Tree BRANDED "MetaParser.TextNode" OBJECT
      text: TEXT;
    END;

(* CAUTION: If you use multi-threading, put a lock around any call to Parse. *)

PROCEDURE NewParser(swr: SynWr.T; 
                    actionTable : ActionTable; fileName: TEXT; rd: Rd.T)
    : SynParse.T 
    RAISES {SynParse.Fail, SynScan.Fail, SynScan.NoReader};
(* This procedure returns a new parser for the grammar read from rd;
    actionTable is a hashed actionTable used to translate the action strings 
   of the grammar file into procedures.  The parser writes messages to
   swr.
*)

PROCEDURE NewClauseList(actionTable : ActionTable; fileName: TEXT; rd: Rd.T)
    : ClauseList RAISES {SynParse.Fail, SynScan.Fail, SynScan.NoReader};
PROCEDURE AddClauseList(tree: SynParse.Tree; p: SynParse.T) 
    RAISES {SynParse.Fail};
(* Main subroutines of NewParser. The clauselist returned by NewClauseList
   may be attached to multiple parsers via AddClauseList.  *)

PROCEDURE Setup() RAISES {SynParse.Fail};
(* To be called before any other use of this module. *)

PROCEDURE PackageSetup(wr: SynWr.T) RAISES {SynParse.Fail};
(* Call all the Setup functions in this package. *)

PROCEDURE NewActionTable(): ActionTable;
(* returns a new, empty action table *)

PROCEDURE Register(name: TEXT; proc: ActionProc;
                          table: ActionTable );

PROCEDURE TableFromArray( sourceTable : ActionProcTable;
                          table: ActionTable ) ;
(* merges an array of ActionProcEntry's (sourceTable) into a ActionTable,
  which can be passed to ReadGFile *)

PROCEDURE PrintClauseList(wr: SynWr.T; list: ClauseList);
(* Basic routines to deal with the metaParser
   datatypes IntegerTemp, RealTemp, and TextTemp. *)

CONST defaultinfo = SynLocation.Info  { fileName :="dummy" ,
                                           char := 0,
                                           line := 0,
                                           lineChar := 0 } ;
      (* too avoid passing info when we don't really need it *)

TYPE
  (* types to represent integers, reals, and strings as parse-trees  *)
  
  IntegerTemp =
    SynParse.Tree BRANDED "MetaParser.IntegerTemp" OBJECT
    int: INTEGER;
  END;
  RealTemp =
    SynParse.Tree BRANDED "MetaParser.RealTemp" OBJECT
    real: LONGREAL;
  END;
  TextTemp =
    SynParse.Tree BRANDED "MetaParser.TextTemp" OBJECT
    text: TEXT ;
  END;
  (* TextTemp, represents Strings, Texts and Chars *)
  (* characters are just represented as one-length TextTemp's *)


(* ==================================================================== *)
(* the following routines transform the given type into a SynParse.Tree *)
(* ==================================================================== *)

PROCEDURE IdentifierToTree(self: SynParse.Identifier; 
                     p: SynParse.T; name: TEXT;
                     READONLY info: SynLocation.Info := defaultinfo): 
                                                             SynParse.Tree;

PROCEDURE NameToTree(self: SynParse.Name; 
                     p: SynParse.T; name: TEXT;
                     READONLY info: SynLocation.Info := defaultinfo): 
                                                             SynParse.Tree;

PROCEDURE IntegerToTree(self: SynParse.Integer; 
                  p: SynParse.T; int: INTEGER;
                  READONLY info: SynLocation.Info := defaultinfo): 
                                                          SynParse.Tree;
                                                          
PROCEDURE RealToTree(self: SynParse.Real; 
               p: SynParse.T; real: LONGREAL;
               READONLY info: SynLocation.Info := defaultinfo ): 
                                                           SynParse.Tree;

PROCEDURE CharToTree(self: SynParse.QuotedChar; 
               p: SynParse.T; char: CHAR;
               READONLY info: SynLocation.Info := defaultinfo): SynParse.Tree;

PROCEDURE StringToTree(self: SynParse.QuotedString; 
                 p: SynParse.T; string: TEXT;
                 READONLY info: SynLocation.Info := defaultinfo): SynParse.Tree;

PROCEDURE TextToTree(self: SynParse.QuotedString; 
                 p: SynParse.T; text: TEXT ;
                 READONLY info: SynLocation.Info := defaultinfo ): SynParse.Tree;


(* ================================================================== *)
(* The G-routines "get" the requested type from location "loc" of the *)
(* SynParse.Stack . loc = base + position                             *)
(* ================================================================== *)

PROCEDURE GInt(p: SynParse.T; loc : INTEGER): INTEGER RAISES {SynParse.Fail};
PROCEDURE GReal(p: SynParse.T; loc :INTEGER): LONGREAL RAISES {SynParse.Fail};
PROCEDURE GText(p: SynParse.T; loc :INTEGER): TEXT RAISES {SynParse.Fail};
PROCEDURE GBool(p: SynParse.T; loc :INTEGER): BOOLEAN RAISES {SynParse.Fail};


(* ============================================================== *)
(* the X-routines transform a SynParse.Tree to the requested type *) 
(* ============================================================== *)

PROCEDURE XInt(wr: SynWr.T; tree: SynParse.Tree): INTEGER RAISES {SynParse.Fail};
PROCEDURE XReal(wr: SynWr.T; tree: SynParse.Tree): LONGREAL RAISES {SynParse.Fail};
PROCEDURE XText(wr: SynWr.T; tree: SynParse.Tree): TEXT RAISES {SynParse.Fail};
PROCEDURE XBool(wr: SynWr.T; tree: SynParse.Tree): BOOLEAN RAISES {SynParse.Fail};


(* ================================================================ *)
(* TypeError prints a type-error, using the location-info contained *)
(* in tree to tell the line and the file-name                       *)
(* Text should contain the expected type                            *)
(* ================================================================ *)

PROCEDURE TypeError(wr: SynWr.T; type: TEXT ; tree : SynParse.Tree) RAISES {SynParse.Fail};

END MetaParser.







