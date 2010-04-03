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
 *)

(*--------------------------------------------------------------------------*)
MODULE SMsg;

IMPORT Wr, Stdio, Process, Text;

(*--------------------------------------------------------------------------*)
PROCEDURE Error(msg : TEXT) =
  BEGIN
    TRY
      IF beepFlag THEN
        Wr.PutChar(errorWr, '\007');
      END;
      Wr.PutText(errorWr, " *** error: " & msg & "\n");
      Wr.Flush(errorWr);
    EXCEPT ELSE END;
  END Error;

(*--------------------------------------------------------------------------*)
PROCEDURE Fatal(msg : TEXT; exitCode : INTEGER := 1) =
  BEGIN
    TRY
      IF beepFlag THEN
        Wr.PutChar(fatalWr, '\007');
      END;
      Wr.PutText(fatalWr, " ### fatal: " & msg & "\n");
      Wr.Flush(fatalWr);
    EXCEPT ELSE END;
    Process.Exit(exitCode);
  END Fatal;

(*--------------------------------------------------------------------------*)
PROCEDURE Warning(msg : TEXT) =
  BEGIN
    TRY
      IF beepFlag THEN
        Wr.PutChar(warningWr, '\007');
      END;
      Wr.PutText(warningWr, " !!! warning: " & msg & "\n");
      Wr.Flush(warningWr);
    EXCEPT ELSE END;
  END Warning;

(*--------------------------------------------------------------------------*)
PROCEDURE Debug(msg : TEXT) =
  BEGIN
    TRY
      Wr.PutText(debugWr, " === debug: " & msg & "\n");
      Wr.Flush(debugWr);
    EXCEPT ELSE END;
  END Debug;

(*--------------------------------------------------------------------------*)
PROCEDURE Error2(proc, msg : TEXT) =
  BEGIN
    TRY
      IF beepFlag THEN
        Wr.PutChar(errorWr, '\007');
      END;
      Wr.PutText(errorWr, " *** error in " & proc & ": " & msg & "\n");
      Wr.Flush(errorWr);
    EXCEPT ELSE END;
  END Error2;

(*--------------------------------------------------------------------------*)
PROCEDURE Fatal2(proc, msg : TEXT; exitCode : INTEGER := 1) =
  BEGIN
    TRY
      IF beepFlag THEN
        Wr.PutChar(fatalWr, '\007');
      END;
      Wr.PutText(fatalWr, " ### fatal in " & proc & ": " & msg & "\n");
      Wr.Flush(fatalWr);
    EXCEPT ELSE END;
    Process.Exit(exitCode);
  END Fatal2;

(*--------------------------------------------------------------------------*)
PROCEDURE Warning2(proc, msg : TEXT) =
  BEGIN
    TRY
      IF beepFlag THEN
        Wr.PutChar(warningWr, '\007');
      END;
      Wr.PutText(warningWr, " !!! warning in " & proc & ": " & msg & "\n");
      Wr.Flush(warningWr);
    EXCEPT ELSE END;
  END Warning2;

(*--------------------------------------------------------------------------*)
PROCEDURE Debug2(proc, msg : TEXT) =
  BEGIN
    TRY
      Wr.PutText(debugWr, " === debug in " & proc &
        ": " & msg & "\n");
      Wr.Flush(debugWr);
    EXCEPT ELSE END;
  END Debug2;

(*--------------------------------------------------------------------------*)
PROCEDURE V(msg : TEXT; unconditionalNewLine := TRUE) =
  BEGIN
    IF vFlag THEN
      TRY
        IF unconditionalNewLine OR NOT Text.Empty(msg) THEN
          Wr.PutText(vWr, msg & "\n");
          Wr.Flush(vWr);
        END;
      EXCEPT ELSE END;
    END;
  END V;

(*--------------------------------------------------------------------------*)
PROCEDURE V2(proc, msg : TEXT; unconditionalNewLine := TRUE) =
  BEGIN
    IF vFlag THEN
      TRY
        IF unconditionalNewLine OR NOT Text.Empty(msg) THEN
          Wr.PutText(vWr, proc & ": " & msg & "\n");
          Wr.Flush(vWr);
        END;
      EXCEPT ELSE END;
    END;
  END V2;

(*--------------------------------------------------------------------------*)
PROCEDURE T(msg : TEXT; unconditionalNewLine := TRUE) =
  BEGIN
    IF tFlag THEN
      TRY
        IF unconditionalNewLine OR NOT Text.Empty(msg) THEN
          Wr.PutText(tWr, msg & "\n");
          Wr.Flush(tWr);
        END;
      EXCEPT ELSE END;
    END;
  END T;

(*--------------------------------------------------------------------------*)
PROCEDURE T2(proc, msg : TEXT; unconditionalNewLine := TRUE) =
  BEGIN
    IF tFlag THEN
      TRY
        IF unconditionalNewLine OR NOT Text.Empty(msg) THEN
          Wr.PutText(tWr, proc & ": " & msg & "\n");
          Wr.Flush(tWr);
        END;
      EXCEPT ELSE END;
    END;
  END T2;

(*--------------------------------------------------------------------------*)
PROCEDURE D(msg : TEXT; unconditionalNewLine := TRUE) =
  BEGIN
    IF dFlag THEN
      TRY
        IF unconditionalNewLine OR NOT Text.Empty(msg) THEN
          Wr.PutText(dWr, msg & "\n");
          Wr.Flush(dWr);
        END;
      EXCEPT ELSE END;
    END;
  END D;

(*--------------------------------------------------------------------------*)
PROCEDURE D2(proc, msg : TEXT; unconditionalNewLine := TRUE) =
  BEGIN
    IF dFlag THEN
      TRY
        IF unconditionalNewLine OR NOT Text.Empty(msg) THEN
          Wr.PutText(dWr, proc & ": " & msg & "\n");
          Wr.Flush(dWr);
        END;
      EXCEPT ELSE END;
    END;
  END D2;

(*--------------------------------------------------------------------------*)
BEGIN
  errorWr   := Stdio.stderr;
  fatalWr   := Stdio.stderr;
  debugWr   := Stdio.stderr;
  warningWr := Stdio.stderr;
  vWr := Stdio.stdout;
  dWr := Stdio.stdout;
  tWr := Stdio.stdout;
  vFlag := FALSE;
  dFlag := FALSE;
  tFlag := FALSE;
  beepFlag := FALSE;
END SMsg.
