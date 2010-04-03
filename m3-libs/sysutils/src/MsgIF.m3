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
MODULE MsgIF;

IMPORT FileWr, Wr, Stdio, OSError, Pathname, Text, TextWr;

REVEAL
  T = Public BRANDED "MsgIF v. 0.0" OBJECT
  METHODS
  OVERRIDES
    init := Init;
    detailedInit := DetailedInit;
    close := Close;
    error := Error;
    fatal := Fatal;
    warning := Warning;
    debug := Debug;
    error2 := Error2;
    fatal2 := Fatal2;
    warning2 := Warning2;
    debug2 := Debug2;
    d := D;
    t := T1;
    v := V;
    d2 := D2;
    t2 := T2;
    v2 := V2;
  END;

(*--------------------------------------------------------------------------*)
PROCEDURE DetailedInit(self : T; wr : Wr.T;
                       dLevel := 0; tLevel := 0; vLevel := 0;
                       beep := FALSE;
                       errorWr   : Wr.T := NIL; (* use wr if not specified *)
                       fatalWr   : Wr.T := NIL; (* use wr if not specified *)
                       warningWr : Wr.T := NIL; (* use wr if not specified *)
                       debugWr   : Wr.T := NIL; (* use wr if not specified *)
                       dWr       : Wr.T := NIL; (* use wr if not specified *)
                       tWr       : Wr.T := NIL; (* use wr if not specified *)
                       vWr       : Wr.T := NIL; (* use wr if not specified *)
                       ) : T =

  PROCEDURE AssignWr(VAR dest : Wr.T; par : Wr.T) =
    BEGIN
      IF par = NIL THEN
        dest := wr;
      ELSE
        dest := par;
      END;
    END AssignWr;

  BEGIN
    self.debugLevel := dLevel;
    self.traceLevel := tLevel;
    self.verboseLevel := vLevel;
    self.beepFlag := beep;
    AssignWr(self.errorWr, errorWr);
    AssignWr(self.fatalWr, fatalWr);
    AssignWr(self.warningWr, warningWr);
    AssignWr(self.debugWr, debugWr);
    AssignWr(self.dWr, dWr);
    AssignWr(self.tWr, tWr);
    AssignWr(self.vWr, vWr);
    RETURN self;
  END DetailedInit;

(*--------------------------------------------------------------------------*)
PROCEDURE Init(self : T; wr : Wr.T; dLevel := 0; tLevel := 0; vLevel := 0;
               beep := FALSE) : T =
  BEGIN
    RETURN self.detailedInit(wr, dLevel, tLevel, vLevel, beep,
                             wr, wr, wr, wr, wr, wr, wr);
  END Init;

(*--------------------------------------------------------------------------*)
PROCEDURE Close(self : T) =

  PROCEDURE CloseWr(VAR wr : Wr.T) =
    BEGIN
      IF wr # NIL THEN
        TRY Wr.Close(wr) EXCEPT ELSE END;
      END;
      wr := NIL;
    END CloseWr;

  BEGIN
    CloseWr(self.errorWr);
    CloseWr(self.warningWr);
    CloseWr(self.fatalWr);
    CloseWr(self.debugWr);
    CloseWr(self.dWr);
    CloseWr(self.tWr);
    CloseWr(self.vWr);
  END Close;

(*--------------------------------------------------------------------------*)
PROCEDURE Error(self : T; msg : TEXT) =
  BEGIN
    TRY
      IF self.beepFlag THEN
        Wr.PutChar(self.errorWr, '\007');
      END;
      Wr.PutText(self.errorWr, " *** error: " & msg & "\n");
      Wr.Flush(self.errorWr);
    EXCEPT ELSE END;
  END Error;

(*--------------------------------------------------------------------------*)
PROCEDURE Fatal(self : T; msg : TEXT) =
  BEGIN
    TRY
      IF self.beepFlag THEN
        Wr.PutChar(self.fatalWr, '\007');
      END;
      Wr.PutText(self.fatalWr, " ### fatal: " & msg & "\n");
      Wr.Flush(self.fatalWr);
    EXCEPT ELSE END;
  END Fatal;

(*--------------------------------------------------------------------------*)
PROCEDURE Warning(self : T; msg : TEXT) =
  BEGIN
    TRY
      IF self.beepFlag THEN
        Wr.PutChar(self.warningWr, '\007');
      END;
      Wr.PutText(self.warningWr, " !!! warning: " & msg & "\n");
      Wr.Flush(self.warningWr);
    EXCEPT ELSE END;
  END Warning;

(*--------------------------------------------------------------------------*)
PROCEDURE Debug(self : T; msg : TEXT; level := 1) =
  BEGIN
    IF self.debugLevel < level THEN RETURN END;
    TRY
      Wr.PutText(self.debugWr, " === debug: " & msg & "\n");
      Wr.Flush(self.debugWr);
    EXCEPT ELSE END;
  END Debug;

(*--------------------------------------------------------------------------*)
PROCEDURE Error2(self : T; proc, msg : TEXT) =
  BEGIN
    TRY
      IF self.beepFlag THEN
        Wr.PutChar(self.errorWr, '\007');
      END;
      Wr.PutText(self.errorWr, " *** error in " & proc & ": " & msg & "\n");
      Wr.Flush(self.errorWr);
    EXCEPT ELSE END;
  END Error2;

(*--------------------------------------------------------------------------*)
PROCEDURE Fatal2(self : T; proc, msg : TEXT) =
  BEGIN
    TRY
      IF self.beepFlag THEN
        Wr.PutChar(self.fatalWr, '\007');
      END;
      Wr.PutText(self.fatalWr, " ### fatal in " & proc & ": " & msg & "\n");
      Wr.Flush(self.fatalWr);
    EXCEPT ELSE END;
  END Fatal2;

(*--------------------------------------------------------------------------*)
PROCEDURE Warning2(self : T; proc, msg : TEXT) =
  BEGIN
    TRY
      IF self.beepFlag THEN
        Wr.PutChar(self.warningWr, '\007');
      END;
      Wr.PutText(self.warningWr, " !!! warning in " & proc & ": " &
        msg & "\n");
      Wr.Flush(self.warningWr);
    EXCEPT ELSE END;
  END Warning2;

(*--------------------------------------------------------------------------*)
PROCEDURE Debug2(self : T; proc, msg : TEXT; level := 1) =
  BEGIN
    IF self.debugLevel < level THEN RETURN END;
    TRY
      Wr.PutText(self.debugWr, " === debug in " & proc &
        ": " & msg & "\n");
      Wr.Flush(self.debugWr);
    EXCEPT ELSE END;
  END Debug2;

(*--------------------------------------------------------------------------*)
PROCEDURE V(self : T; msg : TEXT; unconditionalNewLine := TRUE; level := 1) =
  BEGIN
    IF self.verboseLevel >= level THEN
      TRY
        IF unconditionalNewLine OR NOT Text.Empty(msg) THEN
          Wr.PutText(self.vWr, msg & "\n");
          Wr.Flush(self.vWr);
        END;
      EXCEPT ELSE END;
    END;
  END V;

(*--------------------------------------------------------------------------*)
PROCEDURE V2(self : T; proc, msg : TEXT; unconditionalNewLine := TRUE;
             level := 1) =
  BEGIN
    IF self.verboseLevel >= level THEN
      TRY
        IF unconditionalNewLine OR NOT Text.Empty(msg) THEN
          Wr.PutText(self.vWr, proc & ": " & msg & "\n");
          Wr.Flush(self.vWr);
        END;
      EXCEPT ELSE END;
    END;
  END V2;

(*--------------------------------------------------------------------------*)
PROCEDURE T1(self : T; msg : TEXT; unconditionalNewLine := TRUE; level := 1) =
  BEGIN
    IF self.traceLevel >= level THEN
      TRY
        IF unconditionalNewLine OR NOT Text.Empty(msg) THEN
          Wr.PutText(self.tWr, msg & "\n");
          Wr.Flush(self.tWr);
        END;
      EXCEPT ELSE END;
    END;
  END T1;

(*--------------------------------------------------------------------------*)
PROCEDURE T2(self : T; proc, msg : TEXT; unconditionalNewLine := TRUE;
             level := 1) =
  BEGIN
    IF self.traceLevel >= level THEN
      TRY
        IF unconditionalNewLine OR NOT Text.Empty(msg) THEN
          Wr.PutText(self.tWr, proc & ": " & msg & "\n");
          Wr.Flush(self.tWr);
        END;
      EXCEPT ELSE END;
    END;
  END T2;

(*--------------------------------------------------------------------------*)
PROCEDURE D(self : T; msg : TEXT; unconditionalNewLine := TRUE; level := 1) =
  BEGIN
    IF self.debugLevel >= level THEN
      TRY
        IF unconditionalNewLine OR NOT Text.Empty(msg) THEN
          Wr.PutText(self.dWr, msg & "\n");
          Wr.Flush(self.dWr);
        END;
      EXCEPT ELSE END;
    END;
  END D;

(*--------------------------------------------------------------------------*)
PROCEDURE D2(self : T; proc, msg : TEXT; unconditionalNewLine := TRUE;
             level := 1) =
  BEGIN
    IF self.debugLevel >= level THEN
      TRY
        IF unconditionalNewLine OR NOT Text.Empty(msg) THEN
          Wr.PutText(self.dWr, proc & ": " & msg & "\n");
          Wr.Flush(self.dWr);
        END;
      EXCEPT ELSE END;
    END;
  END D2;

(*--------------------------------------------------------------------------*)
PROCEDURE New(wr : Wr.T; dLevel := 0; tLevel := 0; vLevel := 0;
              beep := FALSE) : T =
  BEGIN
    RETURN NEW(T).init(wr, dLevel, tLevel, vLevel, beep);
  END New;

(*--------------------------------------------------------------------------*)
PROCEDURE NewTextWr(dLevel := 0; tLevel := 0; vLevel := 0; beep := FALSE) : T =
  VAR wr := TextWr.New();
  BEGIN
    RETURN NEW(T).init(wr, dLevel, tLevel, vLevel, beep);
  END NewTextWr;

(*--------------------------------------------------------------------------*)
PROCEDURE NewFileWr(fn : Pathname.T; dLevel := 0; tLevel := 0; vLevel := 0;
                    beep := FALSE) : T
  RAISES {OSError.E} =
  VAR
    wr := FileWr.OpenAppend(fn);
  BEGIN
    RETURN NEW(T).init(wr, dLevel, tLevel, vLevel, beep);
  END NewFileWr;

(*--------------------------------------------------------------------------*)
PROCEDURE NewStdWr(dLevel := 0; tLevel := 0; vLevel := 0;
                   beep := FALSE) : T =
  BEGIN
    RETURN NEW(T).detailedInit(Stdio.stdout, dLevel, tLevel, vLevel, beep,
                               Stdio.stderr, Stdio.stderr);
  END NewStdWr;

BEGIN
END MsgIF.
