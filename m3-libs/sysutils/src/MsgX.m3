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
MODULE MsgX;

IMPORT SMsg AS Msg, MsgIF;

(*--------------------------------------------------------------------------*)
PROCEDURE Error(msgif : MsgIF.T; msg : TEXT) =
  BEGIN
    IF msgif = NIL THEN
      Msg.Error(msg);
    ELSE
      msgif.error(msg);
    END;
  END Error;

(*--------------------------------------------------------------------------*)
PROCEDURE Fatal(msgif : MsgIF.T; msg : TEXT; exitCode : INTEGER := 1) =
  BEGIN
    IF msgif = NIL THEN
      Msg.Fatal(msg, exitCode);
    ELSE
      msgif.fatal(msg);
    END;
  END Fatal;

(*--------------------------------------------------------------------------*)
PROCEDURE Warning(msgif : MsgIF.T; msg : TEXT) =
  BEGIN
    IF msgif = NIL THEN
      Msg.Warning(msg);
    ELSE
      msgif.warning(msg);
    END;
  END Warning;

(*--------------------------------------------------------------------------*)
PROCEDURE Debug(msgif : MsgIF.T; msg : TEXT; level := 1) =
  BEGIN
    IF msgif = NIL THEN
      Msg.Debug(msg);
    ELSE
      msgif.debug(msg, level);
    END;
  END Debug;

(*--------------------------------------------------------------------------*)
PROCEDURE Error2(msgif : MsgIF.T; proc, msg : TEXT) =
  BEGIN
    IF msgif = NIL THEN
      Msg.Error2(proc, msg);
    ELSE
      msgif.error2(proc, msg);
    END;
  END Error2;

(*--------------------------------------------------------------------------*)
PROCEDURE Fatal2(msgif : MsgIF.T; proc, msg : TEXT; exitCode : INTEGER := 1) =
  BEGIN
    IF msgif = NIL THEN
      Msg.Fatal2(proc, msg, exitCode);
    ELSE
      msgif.fatal2(proc, msg);
    END;
  END Fatal2;

(*--------------------------------------------------------------------------*)
PROCEDURE Warning2(msgif : MsgIF.T; proc, msg : TEXT) =
  BEGIN
    IF msgif = NIL THEN
      Msg.Warning2(proc, msg);
    ELSE
      msgif.warning2(proc, msg);
    END;
  END Warning2;

(*--------------------------------------------------------------------------*)
PROCEDURE Debug2(msgif : MsgIF.T; proc, msg : TEXT; level := 1) =
  BEGIN
    IF msgif = NIL THEN
      Msg.Debug2(proc, msg);
    ELSE
      msgif.debug2(proc, msg, level);
    END;
  END Debug2;

(*--------------------------------------------------------------------------*)
PROCEDURE V(msgif : MsgIF.T; msg : TEXT; unconditionalNewLine := TRUE;
            level := 1) =
  BEGIN
    IF msgif = NIL THEN
      Msg.V(msg, unconditionalNewLine);
    ELSE
      msgif.v(msg, unconditionalNewLine, level);
    END;
  END V;

(*--------------------------------------------------------------------------*)
PROCEDURE V2(msgif : MsgIF.T; proc, msg : TEXT; unconditionalNewLine := TRUE;
             level := 1) =
  BEGIN
    IF msgif = NIL THEN
      Msg.V2(proc, msg, unconditionalNewLine);
    ELSE
      msgif.v2(proc, msg, unconditionalNewLine, level);
    END;
  END V2;

(*--------------------------------------------------------------------------*)
PROCEDURE T(msgif : MsgIF.T; msg : TEXT; unconditionalNewLine := TRUE;
            level := 1) =
  BEGIN
    IF msgif = NIL THEN
      Msg.T(msg, unconditionalNewLine);
    ELSE
      msgif.t(msg, unconditionalNewLine, level);
    END;
  END T;

(*--------------------------------------------------------------------------*)
PROCEDURE T2(msgif : MsgIF.T; proc, msg : TEXT; unconditionalNewLine := TRUE;
             level := 1) =
  BEGIN
    IF msgif = NIL THEN
      Msg.T2(proc, msg, unconditionalNewLine);
    ELSE
      msgif.t2(proc, msg, unconditionalNewLine, level);
    END;
  END T2;

(*--------------------------------------------------------------------------*)
PROCEDURE D(msgif : MsgIF.T; msg : TEXT; unconditionalNewLine := TRUE;
            level := 1) =
  BEGIN
    IF msgif = NIL THEN
      Msg.D(msg, unconditionalNewLine);
    ELSE
      msgif.d(msg, unconditionalNewLine, level);
    END;
  END D;

(*--------------------------------------------------------------------------*)
PROCEDURE D2(msgif : MsgIF.T; proc, msg : TEXT; unconditionalNewLine := TRUE;
             level := 1) =
  BEGIN
    IF msgif = NIL THEN
      Msg.D2(proc, msg, unconditionalNewLine);
    ELSE
      msgif.d2(proc, msg, unconditionalNewLine, level);
    END;
  END D2;

BEGIN
END MsgX.
