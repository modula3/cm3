(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Jul  8 12:23:41 PDT 1993 by heydon                   *)

INTERFACE JunoLiteral;

IMPORT JunoValue, Rd;

EXCEPTION ParseError;

PROCEDURE Parse(rd: Rd.T): JunoValue.T RAISES {ParseError, Rd.Failure};
(* First, skip blanks in "rd". Then parse the next Juno literal from "rd" and
   return the corresponding Juno value. A literal is either: a real number,
   the value "NIL", a text enclosed in double-quotes, a pair of values, or a
   list of values. Character escapes within text strings are converted to the
   appropriate ASCII characters.

   Raises "ParseError" if any sort of parse error is detected. In this case,
   the state of the reader "rd" is undefined. *)

END JunoLiteral.
