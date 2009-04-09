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
 * $Id$ *)

MODULE TimeStampLogger;

IMPORT Date, Fmt, Logger, LoggerClass, Process, Text, Time;

REVEAL
  T = Public BRANDED OBJECT
    child: Logger.T;
    ident: TEXT;
  OVERRIDES
    init := Init;
    put := Put;
    close := Close;
  END;

PROCEDURE Init(self: T;
	       child: Logger.T;
	       ident: TEXT := NIL): T =
  BEGIN
    EVAL Logger.T.init(self);
    self.child := child;
    IF ident = NIL THEN ident := "" END;
    self.ident := ident;
    RETURN self;
  END Init;

PROCEDURE Put(self: T;
              priority: Logger.Priority;
	      msg: TEXT) =
  VAR
    Zone := Date.Local;  (* CONST *)
    when: Date.T;
    timeStamp: TEXT;
  BEGIN
    when := Date.FromTime(Time.Now(), Zone);
    IF Text.Empty(when.zone) AND Zone = Date.UTC THEN
      when.zone := "GMT";
    END;
    timeStamp := Fmt.Int(when.year)
      & "." & Fmt.Pad(Fmt.Int(ORD(when.month)+1), 2, '0')
      & "." & Fmt.Pad(Fmt.Int(when.day), 2, '0')
      & " " & Fmt.Pad(Fmt.Int(when.hour), 2, '0')
      & ":" & Fmt.Pad(Fmt.Int(when.minute), 2, '0')
      & ":" & Fmt.Pad(Fmt.Int(when.second), 2, '0')
      & " " & when.zone
      & " " & self.ident & "[" & Fmt.Int(Process.GetMyID()) & "]: ";
    Logger.Put(self.child, priority, timeStamp & msg);
  END Put;

PROCEDURE Close(self: T) =
  BEGIN
    Logger.Close(self.child);
  END Close;

BEGIN
END TimeStampLogger.
