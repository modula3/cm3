INTERFACE SIO;

(* This interface provides functions and procedures for simple input and output.
   Author          : Moritz Schnizler, RWTH Aachen
   Created         : 27.08.98
   Changed         : 03.11.98
*)

IMPORT Fmt, Rd, Wr, Word;

EXCEPTION Error;

TYPE
  Base   = Fmt.Base; (* Base is of type Fmt.Base *)
  Reader = Rd.T;     (* Inputstreams have type Rd.T *)
  Writer = Wr.T;     (* Outputstreams have type Wr.T *)
  Unsigned = Word.T; (* Unsigned is of type Word.T *)

PROCEDURE GetChar(rd: Reader := NIL): CHAR RAISES {Error};
(* Read next character from stream rd and return it. *)
 
PROCEDURE PutChar(ch: CHAR; wr: Writer := NIL);
(* Write ch to outputstream wr. *)

PROCEDURE GetText(rd: Reader := NIL; len: CARDINAL): TEXT;
(* Read a sequence of len characters from rd and return them. If there are not
   enougth characters return what is there. *)

PROCEDURE PutText(t: TEXT; wr: Writer := NIL);
(* Write character sequence t to outputstream wr. *)

PROCEDURE GetWord(rd: Reader := NIL): TEXT RAISES {Error};
(* Read a sequence of characters terminated by space, tab or
   end of line character from rd and return them as text. The
   terminating space etc. is consumed. Maximum of 1024 
   characters allowed. *)

PROCEDURE PutWord(t: TEXT; wr: Writer := NIL);
(* Write text to outputstream wr and terminate it with a space. *)

PROCEDURE GetLine(rd: Reader := NIL): TEXT RAISES {Error};
(* Read a full line of text terminated by the next RETURN from 
   inputstream rd and return it (without RETURN!). *)
  
PROCEDURE PutLine(t: TEXT; wr: Writer := NIL);
(* Write full line of text to the outputstream wr and terminate it
   with RETURN. *)

PROCEDURE GetInt(rd: Reader := NIL): INTEGER RAISES {Error};
(* Read all consecutive numbers from inputstream rd and return
   the result as an integer value. *)

PROCEDURE PutInt(i: INTEGER; base: Base := 10; wr: Writer := NIL);
(* Write the integer i as a sequence of numbers to the outputstream wr 
   according to base. *)

PROCEDURE GetUnsigned(rd: Reader := NIL; base: Base := 16): Unsigned 
  RAISES {Error};
(* Read consecutive numbers from inputstream rd and return the result 
   as an unsigned value. Default base is hexadecimal. *)

PROCEDURE PutUnsigned(w: Unsigned; base: Base := 16; wr: Writer := NIL);
(* Write unsigned number according to base to outputstream wr. Default base
   is hexadecimal. *)

PROCEDURE GetReal(rd: Reader := NIL): REAL RAISES {Error};
(* Read all consecutive characters from inputstream rd which can
   be interpreted as a real number and return them as a real. *)

PROCEDURE PutReal(r: REAL; wr: Writer := NIL);
(* Write text representation of r to outputstream wr. *)

PROCEDURE GetLongReal(rd: Reader := NIL): LONGREAL RAISES {Error};
(* Read all consecutive characters from inputstream rd which can
   be interpreted as a long real number and return them as a long 
   real. *)

PROCEDURE PutLongReal(lr: LONGREAL; wr: Writer := NIL);
(* Write text representation of lr to outputstream wr. *)

PROCEDURE GetExtended(rd: Reader := NIL): EXTENDED RAISES {Error};
(* Read all consecutive characters from inputstream rd which can
   be interpreted as an extended number and return them as an extended. *)

PROCEDURE PutExtended(e: EXTENDED; wr: Writer := NIL);
(* Write text representation of e to outputstream wr. *)

PROCEDURE GetBool(rd: Reader := NIL): BOOLEAN RAISES {Error};
(* Depending on input, it returns false for "FALSE" or true for "TRUE". 
   Case does not matter. *)

PROCEDURE PutBool(b: BOOLEAN; wr: Writer := NIL);
(* Writes, depending on the value of b, the text sequences "TRUE"
   or "FALSE" to the outputstream wr. *)

PROCEDURE LookAhead(rd: Reader := NIL): CHAR RAISES {Error};
(* Returns next character on inputstream rd without removing it from
   the inputstream rd. *)

PROCEDURE Nl(wr: Writer := NIL);
(* Write a RETURN (or new line) to outputstream wr. *)

PROCEDURE EOF(rd: Reader := NIL): BOOLEAN;
(* Returns true, if end of inputstream rd has been reached. *)
  
PROCEDURE Flush(wr: Writer := NIL);
(* Empties the output buffer for output stream wr. Not necessary for
   default outputstream. *)

PROCEDURE Available(rd: Reader := NIL): BOOLEAN;
(* Returns true, if inputstream rd can return a character without blocking. *)

END SIO.