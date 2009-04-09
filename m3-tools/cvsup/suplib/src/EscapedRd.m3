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
 * $Id: EscapedRd.m3,v 1.1.1.1 2009-04-09 17:01:53 jkrell Exp $ *)

MODULE EscapedRd;

IMPORT Atom, AtomList, Rd, RdClass, Thread;

REVEAL
  T = Public BRANDED OBJECT
    rd: Rd.T;
    maxChildRead: CARDINAL;
    closeChild: BOOLEAN;
    eof := FALSE;
    atBOL := TRUE;
    newlinePending := FALSE;
  OVERRIDES
    init := Init;
    seek := Seek;
    length := Length;
    close := Close;
  END;

PROCEDURE Close(self: T)
  RAISES {Rd.Failure, Thread.Alerted} =
  BEGIN
    IF self.closeChild THEN
      Rd.Close(self.rd);
    END;
  END Close;

PROCEDURE Init(self: T;
               rd: Rd.T;
	       maxChildRead: CARDINAL := LAST(CARDINAL);
	       closeChild: BOOLEAN := TRUE): T =
  BEGIN
    RdClass.Lock(self);
    TRY
      self.rd := rd;
      self.maxChildRead := maxChildRead;
      self.closeChild := closeChild;

      self.buff := NEW(REF ARRAY OF CHAR, MIN(maxChildRead, 8192));
      self.st := 0;  (* Always. *)
      self.seekable := FALSE;
      self.intermittent := TRUE;  (* Because we do not know the length. *)
      self.closed := FALSE;
    FINALLY
      RdClass.Unlock(self);
    END;
    RETURN self;
  END Init;

PROCEDURE Length(<*UNUSED*> self: T): INTEGER =
  BEGIN
    RETURN -1;
  END Length;

PROCEDURE Raise(a: Atom.T)
  RAISES {Rd.Failure} =
  BEGIN
    RAISE Rd.Failure(AtomList.List1(a));
  END Raise;

PROCEDURE Seek(self: T; n: CARDINAL; dontBlock: BOOLEAN): RdClass.SeekResult
  RAISES {Rd.Failure, Thread.Alerted} =
  VAR
    pos, lim, len: CARDINAL;
  BEGIN
    <* ASSERT n = self.cur *>
    <* ASSERT self.cur = self.hi *>

    self.lo := self.cur;

    IF self.eof THEN  (* End of stream already reached on previous call. *)
      RETURN RdClass.SeekResult.Eof;
    END;
    IF dontBlock AND Rd.CharsReady(self.rd) = 0 THEN
      RETURN RdClass.SeekResult.WouldBlock;
    END;

    pos := 0;
    lim := NUMBER(self.buff^);
    WHILE pos <= lim - 4 DO  (* Note we leave room for at least "\n.+\n" *)
      IF self.newlinePending THEN  (* Insert pending newline from last time. *)
	self.buff[pos] := '\n';
	INC(pos);
      END;

      len := Rd.GetSubLine(self.rd, SUBARRAY(self.buff^, pos, lim-pos));
      IF len = 0 THEN
	Raise(PrematureEOF);
      END;

      IF self.atBOL AND self.buff[pos] = '.' THEN  (* Escape sequence. *)
	IF len < 2 THEN Raise(PrematureEOF) END;
	CASE self.buff[pos+1] OF
	| '.'  =>
	    SUBARRAY(self.buff^, pos, len-1) :=
	      SUBARRAY(self.buff^, pos+1, len-1);
	    DEC(len);
	| '\n' =>
	    self.eof := TRUE;
	    EXIT;
	| '+'  =>
	    IF len < 3 THEN Raise(PrematureEOF) END;
	    IF self.buff[pos+2] # '\n' THEN Raise(InvalidEscape) END;
	    IF pos > 0 THEN  (* Erase the previous newline *)
	      <* ASSERT self.buff[pos-1] = '\n' *>
	      DEC(pos);
	    END;
	    self.eof := TRUE;
	    EXIT;
	ELSE
	  Raise(InvalidEscape);
	END;
      END;

      INC(pos, len);
      IF self.buff[pos-1] = '\n' THEN
	DEC(pos);
	self.newlinePending := TRUE;
      ELSE
	self.newlinePending := FALSE;
      END;
      self.atBOL := self.newlinePending;
    END;
    self.hi := self.lo + pos;

    IF pos = 0 THEN
      <* ASSERT self.eof *>
      RETURN RdClass.SeekResult.Eof;
    END;
    RETURN RdClass.SeekResult.Ready;
  END Seek;

BEGIN
  PrematureEOF := Atom.FromText("EscapedRd.PrematureEOF");
  InvalidEscape := Atom.FromText("EscapedRd.InvalidEscape");
END EscapedRd.
