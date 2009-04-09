(* Copyright 1996-2003 John D. Polstra.
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
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgment:
 *      This product includes software developed by John D. Polstra.
 * 4. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission.
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
 * $Id: MD5.m3,v 1.1.1.1 2009-04-09 17:01:55 jkrell Exp $ *)

UNSAFE MODULE MD5;

IMPORT
  CText, Ctypes, M3toC, MD5Digest, OSError, OSErrorPosix, Pathname,
  Text, Umd5;

(*****************************************************************************)
(* Code common to all subtypes. *)
(*****************************************************************************)

TYPE
  CUCS = Ctypes.const_unsigned_char_star;

  Common = T OBJECT
    ctx: Umd5.MD5_CTX;
    initted := FALSE;
  METHODS
    init(): Common := Init;
  OVERRIDES
    finish := Finish;
    finishRaw := FinishRaw;
    update := Update;
    updateText := UpdateText;
  END;

PROCEDURE Finish(self: Common): TEXT =
  VAR
    str: Ctypes.char_star;
    t: TEXT;
  BEGIN
    <* ASSERT self.initted *>
    str := Umd5.MD5End(ADR(self.ctx), NIL);
    t := M3toC.CopyStoT(str);
    DISPOSE(str);
    self.initted := FALSE;
    RETURN t;
  END Finish;

PROCEDURE FinishRaw(self: Common; VAR rslt: MD5Digest.T) =
  BEGIN
    <* ASSERT self.initted *>
    Umd5.MD5Final(ADR(rslt[0]), ADR(self.ctx));
    self.initted := FALSE;
  END FinishRaw;

PROCEDURE Init(self: Common): Common =
  BEGIN
    Umd5.MD5Init(ADR(self.ctx));
    self.initted := TRUE;
    RETURN self;
  END Init;

PROCEDURE Update(self: Common; READONLY data: ARRAY OF CHAR) =
  BEGIN
    <* ASSERT self.initted *>
    IF NUMBER(data) # 0 THEN
      self.updateRaw(ADR(data[0]), NUMBER(data));
    END;
  END Update;

PROCEDURE UpdateText(self: Common; text: TEXT) =
  VAR
    textStr := CText.SharedTtoS(text);
  BEGIN
    <* ASSERT self.initted *>
    self.updateRaw(LOOPHOLE(textStr, UNTRACED REF CHAR), Text.Length(text));
    CText.FreeSharedS(text, textStr);
  END UpdateText;

(*****************************************************************************)
(* A subtype for computing MD5 checksums over uninterpreted characters. *)
(*****************************************************************************)

TYPE
  Raw = Common OBJECT OVERRIDES
    clone := Clone;
    updateRaw := UpdateRaw;
  END;

PROCEDURE New(): T =
  VAR
    self := NEW(Raw);
  BEGIN
    EVAL Common.init(self);
    RETURN self;
  END New;

PROCEDURE Clone(self: Raw): T =
  BEGIN
    RETURN NEW(Raw, ctx := self.ctx, initted := self.initted);
  END Clone;

PROCEDURE UpdateRaw(self: Raw; data: UNTRACED REF CHAR; count: CARDINAL) =
  BEGIN
    <* ASSERT self.initted *>
    Umd5.MD5Update(ADR(self.ctx), LOOPHOLE(data, CUCS), count);
  END UpdateRaw;

(*****************************************************************************)
(* A subtype for computing MD5 checksums over RCS files, carefully ignoring
   unimportant details of white space. *)
(*****************************************************************************)

TYPE
  RCS = Common OBJECT
    lastCh := ';';
    state := State.InPrint;
  OVERRIDES
    clone := RCSClone;
    updateRaw := RCSUpdateRaw;
  END;

  State = { InPrint, InWS, InString, AtSeen };

PROCEDURE NewRCS(): T =
  VAR
    self := NEW(RCS);
  BEGIN
    EVAL Common.init(self);
    RETURN self;
  END NewRCS;

PROCEDURE RCSClone(self: RCS): T =
  BEGIN
    RETURN NEW(RCS,
      ctx := self.ctx,
      initted := self.initted,
      lastCh := self.lastCh,
      state := self.state);
  END RCSClone;

PROCEDURE RCSUpdateRaw(self: RCS; data: UNTRACED REF CHAR; count: CARDINAL) =
  CONST
    WS = SET OF CHAR{' ', '\010', '\t', '\n', '\013', '\f', '\r'};
    Print = SET OF CHAR{FIRST(CHAR)..LAST(CHAR)} - WS - SET OF CHAR{'@'};
    Special = SET OF CHAR{'$', ',', ':', ';', '@'};	(* Self-delimiting. *)
    (* '@' isn't strictly self-delimiting, because two in a row is
       special.  But that is handled by special-case code below. *)
  VAR
    Sp := ' ';  (* CONST *)
    ptr := data;
    lim: UNTRACED REF CHAR := ptr + count;
    start: UNTRACED REF CHAR;
  BEGIN
    <* ASSERT self.initted *>
    WHILE ptr < lim DO
      CASE self.state OF
      | State.InPrint =>
	  start := ptr;
	  WHILE ptr < lim AND ptr^ IN Print DO
	    self.lastCh := ptr^;
	    INC(ptr);
	  END;
	  Umd5.MD5Update(ADR(self.ctx), LOOPHOLE(start, CUCS), ptr - start);
	  IF ptr < lim THEN
	    IF ptr^ = '@' THEN
	      Umd5.MD5Update(ADR(self.ctx), LOOPHOLE(ptr, CUCS), 1);
	      INC(ptr);
	      self.state := State.InString;
	    ELSE
	      self.state := State.InWS;
	    END;
	  END;
      | State.InWS =>
	  WHILE ptr < lim AND ptr^ IN WS DO
	    INC(ptr);
	  END;
	  IF ptr < lim THEN
	    IF ptr^ = '@' THEN
	      IF self.lastCh = '@' THEN
		Umd5.MD5Update(ADR(self.ctx), ADR(Sp), 1);
	      END;
	      Umd5.MD5Update(ADR(self.ctx), LOOPHOLE(ptr, CUCS), 1);
	      INC(ptr);
	      self.state := State.InString;
	    ELSE
	      IF NOT self.lastCh IN Special AND NOT ptr^ IN Special THEN
		Umd5.MD5Update(ADR(self.ctx), ADR(Sp), 1);
	      END;
	      self.state := State.InPrint;
	    END;
	  END;
      | State.InString =>
	  start := ptr;
	  WHILE ptr < lim AND ptr^ # '@' DO
	    INC(ptr);
	  END;
	  Umd5.MD5Update(ADR(self.ctx), LOOPHOLE(start, CUCS), ptr - start);
	  IF ptr < lim THEN
	    Umd5.MD5Update(ADR(self.ctx), LOOPHOLE(ptr, CUCS), 1);
	    INC(ptr);
	    self.state := State.AtSeen;
	  END;
      | State.AtSeen =>
	  IF ptr^ = '@' THEN
	    Umd5.MD5Update(ADR(self.ctx), LOOPHOLE(ptr, CUCS), 1);
	    INC(ptr);
	    self.state := State.InString;
	  ELSIF ptr^ IN WS THEN
	    self.lastCh := '@';
	    self.state := State.InWS;
	  ELSE
	    self.state := State.InPrint;
	  END;
      END;
    END;
  END RCSUpdateRaw;

(*****************************************************************************)
(* Convenience procedures. *)
(*****************************************************************************)

PROCEDURE FileSignature(name: Pathname.T): TEXT
  RAISES {OSError.E} =
  VAR
    nameStr := CText.SharedTtoS(name);
    str: Ctypes.char_star;
    t: TEXT;
  BEGIN
    str := Umd5.MD5File(nameStr, NIL);
    CText.FreeSharedS(name, nameStr);
    IF str = NIL THEN
      OSErrorPosix.Raise();
    END;
    t := M3toC.CopyStoT(str);
    DISPOSE(str);
    RETURN t;
  END FileSignature;

BEGIN
END MD5.
