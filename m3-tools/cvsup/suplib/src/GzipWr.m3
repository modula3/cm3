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
 *)

UNSAFE MODULE GzipWr;

IMPORT
  GzipError, OSError, StreamWrClass, Thread, Ugzip, Wr, WrClass;
FROM Ctypes IMPORT unsigned_char_star;

REVEAL
  T = Public BRANDED OBJECT
    wr: Wr.T;
    closeChild: BOOLEAN;
    strmp: Ugzip.z_stream_star := NIL;
  OVERRIDES
    init := Init;
    seek := Seek;
    flush := Flush;
    close := Close;
  END;

PROCEDURE Cleanup(self: T) =
  BEGIN
    IF self.strmp # NIL THEN
      IF self.strmp.state # NIL THEN
	EVAL Ugzip.deflateEnd(self.strmp);
      END;
      DISPOSE(self.strmp);
    END;
  END Cleanup;

PROCEDURE Close(self: T)
  RAISES {Thread.Alerted, Wr.Failure} =
  VAR
    status: INTEGER;
  BEGIN
    TRY
      TRY
	REPEAT
	  IF self.wr.cur = self.wr.hi THEN  (* Must flush the output buffer. *)
	    self.wr.seek(self.wr.cur);
	  END;
	  status := Deflate(self.strmp,
	    ADR(self.buff[self.st]),
	    0,
	    ADR(self.wr.buff[self.wr.st]) + self.wr.cur - self.wr.lo,
	    self.wr.hi - self.wr.cur,
	    Ugzip.Z_FINISH);
	  IF status # Ugzip.Z_OK AND status # Ugzip.Z_STREAM_END THEN
	    RAISE Wr.Failure(GzipError.FromStatus(status));
	  END;
	  self.wr.cur := self.wr.hi - self.strmp.avail_out;
	UNTIL status = Ugzip.Z_STREAM_END;
	self.wr.flush();  (* FIXME - Is this necessary? *)
      FINALLY
	status := Ugzip.deflateEnd(self.strmp);
	DISPOSE(self.strmp);
	IF status # Ugzip.Z_OK THEN
	  RAISE Wr.Failure(GzipError.FromStatus(status));
	END;
      END;
    FINALLY
      IF self.closeChild THEN
	Wr.Close(self.wr);
      END;
    END;
  END Close;

PROCEDURE Deflate(strmp: Ugzip.z_stream_star;
                  next_in: unsigned_char_star;
		  avail_in: CARDINAL;
		  next_out: unsigned_char_star;
		  avail_out: CARDINAL;
		  flush: INTEGER): INTEGER =
(* Call "Ugzip.deflate", making sure that pointers into the (traced)
   input and output buffers are on the stack or in registers.  This
   ensures that the collector will not move the buffers. *)
  BEGIN
    strmp.next_in := next_in;
    strmp.avail_in := avail_in;
    strmp.next_out := next_out;
    strmp.avail_out := avail_out;
    RETURN Ugzip.deflate(strmp, flush);
  END Deflate;

PROCEDURE Flush(self: T)
  RAISES {Thread.Alerted, Wr.Failure} =
  VAR
    status: INTEGER;
  BEGIN
    Seek(self, self.cur);  (* Compress everything in the input buffer. *)
    REPEAT
      IF self.wr.cur = self.wr.hi THEN  (* Must flush the output buffer. *)
	self.wr.seek(self.wr.cur);
      END;
      status := Deflate(self.strmp,
	ADR(self.buff[self.st]),
	0,
	ADR(self.wr.buff[self.wr.st]) + self.wr.cur - self.wr.lo,
	self.wr.hi - self.wr.cur,
	Ugzip.Z_PARTIAL_FLUSH);
      IF status # Ugzip.Z_OK AND status # Ugzip.Z_BUF_ERROR THEN
	RAISE Wr.Failure(GzipError.FromStatus(status));
      END;
      self.wr.cur := self.wr.hi - self.strmp.avail_out;
    UNTIL self.strmp.avail_out > 0;
    self.wr.flush();
  END Flush;

PROCEDURE Init(self: T;
               wr: Wr.T; level: CompLevel := -1;
	       closeChild: BOOLEAN := TRUE): T
  RAISES {OSError.E} =
  VAR
    status: INTEGER;
  BEGIN
    WrClass.Lock(self);
    TRY
      self.wr := wr;
      self.closeChild := closeChild;

      self.buff := NEW(REF ARRAY OF CHAR, 8192);
      self.st := 0;
      self.hi := NUMBER(self.buff^);
      self.seekable := FALSE;
      self.buffered := wr.buffered;
      self.closed := FALSE;

      self.strmp := NEW(Ugzip.z_stream_star,
	zalloc := Ugzip.malloc,
	zfree := Ugzip.free,
	opaque := NIL,
	state := NIL);
      status := Ugzip.deflateInit(self.strmp, level);
      IF status # Ugzip.Z_OK THEN
	RAISE OSError.E(GzipError.FromStatus(status));
      END;
    FINALLY
      WrClass.Unlock(self);
    END;
    RETURN self;
  END Init;

PROCEDURE Seek(self: T; n: CARDINAL)
  RAISES {Thread.Alerted, Wr.Failure} =
  VAR
    oldLo: CARDINAL;
    status: INTEGER;
  BEGIN
    <* ASSERT n = self.cur *>

    oldLo := self.lo;
    WHILE self.lo < self.cur DO  (* There is more in the input buffer. *)
      IF self.wr.cur = self.wr.hi THEN  (* Must flush the output buffer. *)
	self.wr.seek(self.wr.cur);
      END;
      status := Deflate(self.strmp,
	ADR(self.buff[self.st]) + self.lo - oldLo,
	self.cur - self.lo,
	ADR(self.wr.buff[self.wr.st]) + self.wr.cur - self.wr.lo,
	self.wr.hi - self.wr.cur,
	Ugzip.Z_NO_FLUSH);
      IF status # Ugzip.Z_OK THEN
	RAISE Wr.Failure(GzipError.FromStatus(status));
      END;
      self.lo := self.cur - self.strmp.avail_in;
      self.wr.cur := self.wr.hi - self.strmp.avail_out;
    END;
    self.hi := self.lo + NUMBER(self.buff^) - self.st;
    StreamWrClass.DontOverflow(self);
  END Seek;

BEGIN
END GzipWr.
