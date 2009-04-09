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
 * $Id: EscapedWr.m3,v 1.1.1.1 2009-04-09 17:01:53 jkrell Exp $ *)

MODULE EscapedWr;

IMPORT Thread, Wr, WrClass;

REVEAL
  T = Public BRANDED OBJECT
    wr: Wr.T;
    closeChild: BOOLEAN;
    atBOL := TRUE;	(* At beginning of a line. *)
  OVERRIDES
    init := Init;
    seek := Seek;
    flush := Flush;
    close := Close;
  END;

PROCEDURE Close(self: T)
  RAISES {Thread.Alerted, Wr.Failure} =
  BEGIN
    TRY
      IF self.atBOL AND self.cur > 0 THEN
	Wr.PutText(self.wr, ".\n");
      ELSE
	Wr.PutText(self.wr, "\n.+\n");
      END;
      Wr.Flush(self.wr);
    FINALLY
      IF self.closeChild THEN
	Wr.Close(self.wr);
      END;
    END;
  END Close;

PROCEDURE Flush(self: T)
  RAISES {Thread.Alerted, Wr.Failure} =
  BEGIN
    Seek(self, self.cur);
    Wr.Flush(self.wr);
  END Flush;

PROCEDURE Init(self: T;
               wr: Wr.T;
	       closeChild: BOOLEAN := TRUE): T =
  BEGIN
    WrClass.Lock(self);
    TRY
      self.wr := wr;
      self.closeChild := closeChild;

      self.buff := NEW(REF ARRAY OF CHAR, 8192);
      self.st := 0;  (* Always *)
      self.hi := NUMBER(self.buff^);
      self.seekable := FALSE;
      self.buffered := wr.buffered;
      self.closed := FALSE;
    FINALLY
      WrClass.Unlock(self);
    END;
    RETURN self;
  END Init;

PROCEDURE Seek(self: T; n: CARDINAL)
  RAISES {Thread.Alerted, Wr.Failure} =
  VAR
    ch: CHAR;
    pos, lim, start: CARDINAL;
  BEGIN
    <* ASSERT n = self.cur *>

    pos := 0;
    lim := self.cur - self.lo;
    start := pos;
    WHILE pos < lim DO
      (* Scan forward to the point at which we have to escape something,
	 or to the end of the buffered characters. *)

      ch := self.buff[pos];
      INC(pos);

      IF self.atBOL AND ch = '.' THEN
	(* Emit the accumulated characters, and double the '.'. *)
	Wr.PutString(self.wr, SUBARRAY(self.buff^, start, pos-start));
	Wr.PutChar(self.wr, '.');
	start := pos;
      END;

      self.atBOL := ch = '\n';
    END;
    IF start < pos THEN  (* Emit escape-free tail portion. *)
      Wr.PutString(self.wr, SUBARRAY(self.buff^, start, pos-start));
      start := pos;
    END;

    self.lo := self.cur;
    self.hi := self.lo + NUMBER(self.buff^);
  END Seek;

BEGIN
END EscapedWr.
