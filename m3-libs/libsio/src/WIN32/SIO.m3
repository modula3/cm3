MODULE SIO;

(* Platform NT 386 with the two characters CR/LF as end of line. *)

(* This module implements functions and procedures for simple input and output.
   Author          : Moritz Schnizler, RWTH Aachen
   Environment     : SRC-Modula-3 rel. 3.6, Windows NT 4.0
   Created         : 27.08.98
   Changed         : 03.11.98
*)

IMPORT FloatMode, Fmt, Lex, Rd, Stdio, Text, Thread, Wr;

<* FATAL Rd.Failure, Wr.Failure, Thread.Alerted *>

PROCEDURE CheckRd(rd: Reader): Reader =
(* Check, if input stream is given or default stream required. *)
  BEGIN 
    IF rd = NIL THEN rd := Stdio.stdin END;
    RETURN rd;
  END CheckRd;

PROCEDURE CheckWr(wr: Writer): Writer =
(* Check, if output stream is given or default stream required. *)
  BEGIN 
    IF wr = NIL THEN wr := Stdio.stdout END;
    RETURN wr;
  END CheckWr;

PROCEDURE GetChar(rd: Reader := NIL): CHAR RAISES {Error}=
(* Read next character from stream rd and return it. *)
  BEGIN
    rd := CheckRd(rd);
    TRY
      RETURN Rd.GetChar(rd);
    EXCEPT
    |  Rd.EndOfFile => RAISE Error;
    END;
  END GetChar;

PROCEDURE PutChar(ch: CHAR; wr: Writer := NIL)=
(* Write ch to outputstream wr. *)
  BEGIN
    wr := CheckWr(wr);
    Wr.PutChar(wr, ch); Wr.Flush(wr);
  END PutChar;

PROCEDURE GetText(rd: Reader := NIL; len: CARDINAL): TEXT=
(* Read a sequence of len characters from rd and return them. If there are not
   enougth characters return what is there. *)
  BEGIN
    rd := CheckRd(rd);
    RETURN Rd.GetText(rd, len);
  END GetText;

PROCEDURE PutText(t: TEXT; wr: Writer := NIL)=
(* Write character sequence t to outputstream wr. *)
  BEGIN
    wr := CheckWr(wr);
    Wr.PutText(wr, t); Wr.Flush(wr);
  END PutText;

PROCEDURE GetWord(rd: Reader := NIL): TEXT RAISES {Error}=
(* Read a sequence of characters terminated by space, tab or
   end of line character from rd and return them as text. The
   terminating space etc. is consumed. Maximum of 1024 
   characters allowed. *)

  CONST max = 1026; (* 1024 characters + CR/LF *)
                    
  VAR len   : [0..max];
      rtext : ARRAY [1..max] OF CHAR;
      buffer: TEXT;
  BEGIN
    rd := CheckRd(rd);

    (* Read word from input. *)
    len := 0;
    REPEAT
      INC(len);
      TRY
        rtext[len] := Rd.GetChar(rd);
      EXCEPT
      | Rd.EndOfFile => RAISE Error;
      END;
    UNTIL (len = max) OR 
          (rtext[len] = ' ') OR (rtext[len] = '\t') OR 
          (rtext[len] = '\n'); 

    (* Raise error if more than 1024 chars. *)
    IF (len >= max-1) AND 
       NOT ((rtext[len] = ' ') OR 
            (rtext[len] = '\t') OR 
            (rtext[len] = '\n')) THEN RAISE Error END;

    (* If new line read, also destroy CR under NT 386. *)
    IF (rtext[len] = '\n') THEN DEC(len); END;

    buffer := Text.FromChars(rtext);
    RETURN Text.Sub(buffer, 0, len-1);  
  END GetWord;

PROCEDURE PutWord(t: TEXT; wr: Writer := NIL)=
(* Write text to outputstream wr and terminate it with a space. *)
  BEGIN
    wr := CheckWr(wr);
    Wr.PutText(wr, t); Wr.PutChar(wr, ' '); Wr.Flush(wr);
  END PutWord;

PROCEDURE GetLine(rd: Reader := NIL): TEXT RAISES {Error}=
(* Read a full line of text terminated by the next RETURN from 
   inputstream rd and return it (without RETURN!). *)
  BEGIN
    rd := CheckRd(rd);
    TRY
      RETURN Rd.GetLine(rd);
     EXCEPT
     | Rd.EndOfFile => RAISE Error;
     END;
  END GetLine;

PROCEDURE PutLine(t: TEXT; wr: Writer := NIL)=
(* Write full line of text to the outputstream wr and terminate it
   with RETURN. *)
  BEGIN
    wr := CheckWr(wr);
    Wr.PutText(wr, t); Nl(wr); Wr.Flush(wr);
  END PutLine;

PROCEDURE GetInt(rd: Reader := NIL): INTEGER RAISES {Error}=
(* Read all consecutive numbers from inputstream rd and return
   the result as an integer value. *)
  BEGIN
    rd := CheckRd(rd);

    TRY 
      RETURN Lex.Int(rd);
    EXCEPT
    | Lex.Error, FloatMode.Trap => RAISE Error;
    END;   
  END GetInt;

PROCEDURE PutInt(i: INTEGER; base: Base := 10; wr: Writer := NIL)=
(* Write the integer i as a sequence of numbers to the outputstream wr 
   according to base. *)
  BEGIN
    wr := CheckWr(wr);
    Wr.PutText(wr, Fmt.Int(i, base)); Wr.Flush(wr);
  END PutInt;

PROCEDURE GetUnsigned(rd: Reader := NIL; base: Base := 16): Unsigned 
  RAISES {Error}=
(* Read consecutive numbers from inputstream rd and return the result 
   as an unsigned value. Default base is hexadecimal. *)
  BEGIN
    rd := CheckRd(rd);

    TRY 
      RETURN Lex.Unsigned(rd, base);
    EXCEPT
    | Lex.Error, FloatMode.Trap => RAISE Error;
    END;   
  END GetUnsigned;

PROCEDURE PutUnsigned(w: Unsigned; base: Base := 16; wr: Writer := NIL)=
(* Write unsigned number according to base to outputstream wr. Default base
   is hexadecimal. *)
  BEGIN
    wr := CheckWr(wr);
    Wr.PutText(wr, Fmt.Unsigned(w, base)); Wr.Flush(wr);
  END PutUnsigned;


PROCEDURE GetReal(rd: Reader := NIL): REAL RAISES {Error}=
(* Read all consecutive characters from inputstream rd which can
   be interpreted as a real number and return them as a real. *)
  BEGIN
    rd := CheckRd(rd);
    
    TRY
      RETURN Lex.Real(rd);
    EXCEPT
    | Lex.Error, FloatMode.Trap => RAISE Error;
    END;
  END GetReal;

PROCEDURE PutReal(r: REAL; wr: Writer := NIL)=
(* Write text representation of r to outputstream wr. *)
  BEGIN
    wr := CheckWr(wr);
    Wr.PutText(wr, Fmt.Real(r)); Wr.Flush(wr);
  END PutReal;

PROCEDURE GetLongReal(rd: Reader := NIL): LONGREAL RAISES {Error}=
(* Read all consecutive characters from inputstream rd which can
   be interpreted as a long real number and return them as a long 
   real. *)
  BEGIN
    rd := CheckRd(rd);
    
    TRY
      RETURN Lex.LongReal(rd);
    EXCEPT
    | Lex.Error, FloatMode.Trap => RAISE Error;
    END;
  END GetLongReal;

PROCEDURE PutLongReal(lr: LONGREAL; wr: Writer := NIL)=
(* Write text representation of lr to outputstream wr. *)
  BEGIN
    wr := CheckWr(wr);
    Wr.PutText(wr, Fmt.LongReal(lr)); Wr.Flush(wr);
  END PutLongReal;

PROCEDURE GetExtended(rd: Reader := NIL): EXTENDED RAISES {Error}=
(* Read all consecutive characters from inputstream rd which can
   be interpreted as an extended number and return them as an extended. *)
  BEGIN
    rd := CheckRd(rd);
    
    TRY
      RETURN Lex.Extended(rd);
    EXCEPT
    | Lex.Error, FloatMode.Trap => RAISE Error;
    END;
  END GetExtended;

PROCEDURE PutExtended(e: EXTENDED; wr: Writer := NIL)=
(* Write text representation of e to outputstream wr. *)
  BEGIN
    wr := CheckWr(wr);
    Wr.PutText(wr, Fmt.Extended(e)); Wr.Flush(wr);
  END PutExtended;

PROCEDURE GetBool(rd: Reader := NIL): BOOLEAN RAISES {Error}=
(* Depending on input, it returns false for "FALSE" or true for "TRUE". 
   Case does not matter. *)
  BEGIN
    rd := CheckRd(rd);
    
    TRY
      RETURN Lex.Bool(rd);
    EXCEPT
    | Lex.Error => RAISE Error;
    END;
  END GetBool;

PROCEDURE PutBool(b: BOOLEAN; wr: Writer := NIL)=
(* Writes, depending on the value of b, the text sequences "TRUE"
   or "FALSE" to the outputstream wr. *)
  BEGIN
    wr := CheckWr(wr);
    Wr.PutText(wr, Fmt.Bool(b)); Wr.Flush(wr);
  END PutBool;

PROCEDURE LookAhead(rd: Reader := NIL): CHAR RAISES {Error}=
(* Returns next character on inputstream rd without removing it from
   the inputstream rd. *)
  VAR c: CHAR;
  BEGIN
    rd := CheckRd(rd);
    TRY
      c := Rd.GetChar(rd);
    EXCEPT
    | Rd.EndOfFile => RAISE Error;
    END;
    Rd.UnGetChar(rd);
    RETURN c;    
  END LookAhead;

PROCEDURE Nl(wr: Writer := NIL)=
(* Write a RETURN (or new line) to outputstream wr. *)
  BEGIN
    wr := CheckWr(wr);
    Wr.PutChar(wr, '\r'); (* CR for NT 386 *)
    Wr.PutChar(wr, '\n'); 
    Wr.Flush(wr);
  END Nl; 

PROCEDURE EOF(rd: Reader := NIL): BOOLEAN=
(* Returns true, if end of inputstream rd has been reached. *)
  BEGIN
    rd := CheckRd(rd);
    RETURN Rd.EOF(rd);
  END EOF;    
  
PROCEDURE Flush(wr: Writer := NIL)=
(* Empties the output buffer for output stream wr. Not necessary for
   default outputstream. *)
  BEGIN
    wr := CheckWr(wr);
    Wr.Flush(wr);
  END Flush;

PROCEDURE Available(rd: Reader := NIL): BOOLEAN=
(* Returns true, if inputstream rd can return a character without blocking. *)
  BEGIN
    rd := CheckRd(rd);
    IF (Rd.CharsReady(rd) > 0) THEN 
      RETURN TRUE 
    ELSE 
      RETURN FALSE 
    END;
  END Available;

BEGIN
END SIO.