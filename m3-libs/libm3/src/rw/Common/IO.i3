(* Copyright 1993 by Digital Equipment Corp.                   *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Last modified on Thu Sep 23 12:17:09 PDT 1993 by mcjones    *)
(*      modified on Fri Jun 18 13:27:27 PDT 1993 by wobber     *)
(*      modified on Sat Jan 26 14:38:00 PST 1993 by gnelson    *)

(* The "IO" interface provides textual input and output for simple
   programs.  For more detailed control, use the interfaces "Rd",
   "Wr", "Stdio", "FileRd", "FileWr", "Fmt", and "Lex".

   The input procedures take arguments of type "Rd.T" that specify
   which input stream to use.  If this argument is defaulted, standard
   input ("Stdio.stdin") is used.  Similarly, if an argument of type
   "Wr.T" to an output procedure is defaulted, "Stdio.stdout" is used.
   *)

INTERFACE IO;

IMPORT Rd, Wr;

PROCEDURE Put(txt: TEXT; wr: Wr.T := NIL);
(* Output "txt" to "wr" and flush "wr". *)

PROCEDURE PutInt(n: INTEGER; wr: Wr.T := NIL);
(* Output "Fmt.Int(n)" to "wr" and flush "wr". *)
   
PROCEDURE PutReal(r: REAL; wr: Wr.T := NIL);
(* Output "Fmt.Real(r)" to "wr" and flush "wr". *)

PROCEDURE EOF(rd: Rd.T := NIL): BOOLEAN;
(* Return "TRUE" iff "rd" is at end-of-file. *)

EXCEPTION Error;

(* The exception "Error" is raised whenever a "Get" procedure
   encounters syntactically invalid input, including unexpected
   end-of-file. *)

PROCEDURE GetLine(rd: Rd.T := NIL): TEXT RAISES {Error};
(* Read a line of text from "rd" and return it. *)

(* A line of text is either zero or more characters terminated by a
   line break, or one or more characters terminated by an end-of-file.
   In the former case, "GetLine" consumes the line break but does not
   include it in the returned value.  A line break is either {\tt
   \char'42\char'134n\char'42} or {\tt
   \char'42\char'134r\char'134n\char'42}. *)

PROCEDURE GetChar(rd: Rd.T := NIL): CHAR RAISES {Error};
(* Read the next character from "rd" and return it. *)

PROCEDURE GetInt(rd: Rd.T := NIL): INTEGER RAISES {Error};
(* Read a decimal numeral from "rd" using "Lex.Int" and return its
   value. *)

PROCEDURE GetReal(rd: Rd.T := NIL): REAL RAISES {Error};
(* Read a real number from "rd" using "Lex.Real" and return its value.
   *)
  
PROCEDURE OpenRead(f: TEXT): Rd.T;
(* Open the file name "f" for reading and return a reader on its
   contents. If the file doesn't exist or is not readable, return
   "NIL". *)

PROCEDURE OpenWrite(f: TEXT): Wr.T;
(* Open the file named "f" for writing and return a writer on its
   contents.  If the file does not exist it will be created.  If the
   process does not have the authority to modify or create the file,
   return "NIL". *)

END IO.
