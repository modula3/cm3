(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Thu Oct 20 09:45:27 PDT 1994 by kalsow     *)
(*      modified on Sun Sep  4 22:02:44 PDT 1994 by stolfi     *)
(*      modified on Sat Sep 15 02:51:50 1990 by muller         *)
(*      modified on Fri Jun 10 14:20:12 1988 by glassman       *)

(* This interface provides simple and robust tools for parsing
   the command line arguments given to a process when it is 
   started (see "Params" and "Process.Create"). 
   \index{parameters of a process} \index{process!parameters}
*)

INTERFACE ParseParams;

(* NOTE: Before reading the details, check the usage example at the
   end of this interface. *)

IMPORT Wr;

EXCEPTION Error;

TYPE
  T <: Public; 
    (* A parser for UNIX-style command line arguments. *)

  Public = OBJECT 
      
      arg: REF ARRAY OF TEXT;
        (* Arguments given, including the command name "arg[0]",
	   but excluding any "@M3" directives. *)
      
      parsed: REF ARRAY OF BOOLEAN;
        (* Flag "parsed[i]" is "TRUE" if "arg[i] 
	   has been parsed. *)
	
      next: CARDINAL;
        (* The next argument to parse is "arg[next]" *)
      
    METHODS

      init (wr: Wr.T): T;
        (* Allocates the arrays "arg" and "parsed" and
	   initializes them with the parameters of the current
	   process.  Marks "arg[0]" as parsed, all others as unparsed, 
	   and sets "next" to 1.  Any subsequent parsing errors
	   will be printed out to "wr". *)
      
      keywordPresent (key: TEXT): BOOLEAN;
        (* Looks for the first unparsed argument "arg[i]"
	   that is equal to "key".  If found, marks it as parsed, 
	   sets "next" to "i+1", and returns "TRUE".
           Otherwise returns "FALSE" and leaves "next" unchanged. *)
	
      getKeyword (key: TEXT) RAISES {Error};
        (* Same as "keywordPresent", but raises "Error" if the 
	   keyword is not found. *)

      getNext (): TEXT RAISES {Error};
        (* Returns "arg[next]", marks it as parsed and increments "next".  
	   Raises "Error" if "arg[next]" does not exist or has already 
	   been parsed. *)

      testNext (key: TEXT): BOOLEAN RAISES {};
        (* If "arg[next]" exists, is unparsed, and is equal to "key", 
	   marks it as parsed, increments "next" and returns TRUE.  
	   Otherwise does none of these things and returns "FALSE". *)

      getNextInt (
          min:=FIRST(INTEGER); max:=LAST(INTEGER)
	): INTEGER RAISES {Error};
      getNextReal (
          min:=FIRST(REAL); max:=LAST(REAL)
	): REAL RAISES {Error};
      getNextLongReal (
          min:=FIRST(LONGREAL); max:=LAST(LONGREAL)
	): LONGREAL RAISES {Error};  
        (* Same as "getNext", but converts the result to the approriate
	   type (using "Scan.Int", "Scan.Real", "Scan.LongReal").  
	   Raises "Error" if the parameter is not a valid literal, or
	   lies outside of the range "[min..max]".  *)

      error (msg: TEXT) RAISES {Error};
        (* Prints the given message, and raises "Error". *)

      skipParsed () RAISES {Error};
        (* Points "next" at the first unparsed argument.
	   If there are parsed arguments beyond that one,
	   prints a message and raises "Error". *)

      finish () RAISES {Error};
        (* Checks if all parameters have been parsed; if not,
	   prints a message and raises "Error". *)
   END;

END ParseParams.

(*
   In some popular operating systems, most programs expect their
   command-line arguments to consist of a string of keywords and
   keyword-labeled arguments (`options', `switches', etc.), followed
   by a list of positional arguments.

   To help the user, programs generally allow the switches and
   keyword-labeled arguments to be given in any order.  Some of those
   parameters may be optional and/or repeatable, some may be
   mandatory; some may be required or forbidden depending on the
   values of the other parameters.  Furthermore, the value of an
   argument may be just a number or a text string, or may be a cluster
   of two or more values with their own little syntax.

   This module simplifies the parsing of such command-line parameters,
   by allowing the program to scan the arguments in their "logical
   order".  It also detects automatically many kinds of common
   mistakes---arguments that are missing, repeated, extraneous,
   malformed, or out of range---and prints the appropriate error
   messages.

   For example, here is how this module could be used by an
   hypothetical program "prt" that concatenates a bunch of files and
   prints selected pages ranges, possibly in reverse order, with
   several formatting options.

   | CONST
   |   MaxPages = 10000;
   |
   | VAR (* Arguments from command line: *)
   |   fontSize: CARDINAL;
   |   landscape: BOOLEAN;
   |   nRanges: CARDINAL := 0;
   |   ini, fin: ARRAY [0..99] OF [1..MaxPages];
   |   rev: ARRAY [0..99] OF BOOLEAN;
   |   files: REF ARRAY OF TEXT;
   |
   | PROCEDURE ParseCommandLine () =
   |   CONST
   |     Usage =
   |       "prt \\\n" &
   |       "  -fontSize <n> \\\n" &
   |       "  [ -landscape | -portrait ] \\\n" &
   |       "  [ -pages <n> <n> [ -reverse ] ]... \\\n" &
   |       "  file...\n";
   |   BEGIN
   |     WITH
   |       pp = NEW(ParseParams.T).init(Stdio.stderr)
   |     DO
   |       TRY
   |
   |         (* The "-fontSize " parameter is mandatory: *)
   |         pp.getKeyword("-fontSize");
   |         fontSize := pp.getNextInt(1,100);
   |
   |         (* Either "-landscape" or "-portrait", but not both: *)
   |         IF pp.keywordPresent("-landscape") THEN
   |           landscape := TRUE
   |         ELSIF pp.keywordPresent("-portrait")  THEN
   |           landscape := FALSE
   |         ELSE
   |           (* Default is "-portrait" unless font is too big: *)
   |           landscape := (fontSize > 8)
   |         END;
   |
   |         (* Parse the page ranges: *)
   |         nRanges := 0;
   |         WHILE pp.keywordPresent("-pages") DO
   |           IF nRanges > LAST(ini) THEN pp.error("Too many page ranges") END;
   |           ini[nRanges] := pp.getNextInt(1,MaxPages);
   |           fin[nRanges] := pp.getNextInt(ini[nRanges],MaxPages);
   |           rev[nRanges] := pp.testNext("-reverse");
   |           nRanges := nRanges+1;
   |         END;
   |         IF nRanges = 0 THEN
   |           ini[0] := 1; fin[0] := MaxPages; rev[0] := FALSE;
   |           nRanges := 1
   |         END;
   |
   |         (* Parse the file list: *)
   |         pp.skipParsed();
   |         WITH nFiles = NUMBER(pp.arg^) - pp.next DO
   |           IF nFiles = 0 THEN pp.error("no files specified") END;
   |           files := NEW(REF ARRAY OF TEXT, );
   |           FOR i := 0 TO nFiles-1 DO
   |             files[i] := pp.getNext()
   |           END
   |         END;
   |
   |         (* Check for any unparsed parameters: *)
   |         pp.finish();
   |
   |       EXCEPT
   |         ParseParams.Error =>
   |           Wr.PutText(Stdio.stderr, Usage);
   |           Process.Exit(1);
   |       END
   |     END
   |   END ParseCommandLine;

   Note that even though this code parses the parameters in a fixed
   order, the user may give them in any order.
  *)
