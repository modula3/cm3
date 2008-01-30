(* Copyright 1999-2002 elego Software Solutions GmbH, Berlin, Germany.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * $Id$ *)

(*---------------------------------------------------------------------------*)
MODULE EnvUtils;

IMPORT Lex, TextTextTbl, Text, TextConv, TextRd, TextWr, Rd, Wr, Thread;

(*---------------------------------------------------------------------------*)
PROCEDURE ToText(env : T; sepChar := '\t') : TEXT RAISES {Error} =
  VAR
    iter := env.iterate();
    name ,
    val  :  TEXT;
    twr  := TextWr.New();
    sep  := Text.FromChar(sepChar);
  BEGIN
    TRY
      Wr.PutText(twr, "(@env-start" & sep);
      WHILE iter.next(name, val) DO
        Wr.PutText(twr, name & sep & TextConv.Encode(val, TRUE) & sep);
      END;
      Wr.PutText(twr, "env-end@)" & sep);
    EXCEPT
      Thread.Alerted => RAISE Error("interrupted converting environment");
    | Wr.Failure => RAISE Error("writer failure converting environment"); 
    END;
    RETURN TextWr.ToText(twr);
  END ToText;

(*---------------------------------------------------------------------------*)
PROCEDURE FromRd(rd : Rd.T; sepChars := Separators; skipStart := FALSE) : T 
  RAISES {Error} =
  VAR
    sep  := sepChars;
    nsep := SET OF CHAR{'\000' .. '\377'} - sep;
    res  := NEW(TextTextTbl.Default).init();
    name :  TEXT;
    val  :  TEXT;
    done := FALSE;
  BEGIN
    TRY
      IF NOT skipStart THEN
        Lex.Skip(rd, sep);
        name := Lex.Scan(rd, nsep);
        IF NOT Text.Equal(name, "(@env-start") THEN
          RAISE Error("expected " & "(@env-start" & ", found " & name);
        END;
      END;
      WHILE NOT done AND NOT Rd.EOF(rd) DO
        Lex.Skip(rd, sep);
        name := Lex.Scan(rd, nsep);
        IF Text.Equal(name, "env-end@)") THEN
          done := TRUE;
        ELSE
          Lex.Skip(rd, sep);
          val := Lex.Scan(rd, nsep);
          val := TextConv.Decode(val, TRUE);
          EVAL res.put(name, val);
        END;
      END;
    EXCEPT
      Rd.Failure => RAISE Error("read error on text resource");
    | Thread.Alerted => RAISE Error("interrupted reading text resource");
    | TextConv.Fail => RAISE Error("conversion error in text resource");
    END;
    IF NOT done THEN
      RAISE Error("premature end of environment denotation");
    END;
    RETURN res;
  END FromRd;

(*---------------------------------------------------------------------------*)
PROCEDURE FromText(t : TEXT; sepChars := Separators) : T RAISES {Error} =
  VAR
    trd  := TextRd.New(t);
  BEGIN
    RETURN FromRd(trd, sepChars);
  END FromText;

BEGIN
END EnvUtils.
