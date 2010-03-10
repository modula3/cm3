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
INTERFACE MsgX;

IMPORT MsgIF;

(*--------------------------------------------------------------------------*)
(* MsgX is a switching layer above Msg and MsgIF interfaces. All procedures
   are the same as in Msg, except for their first parameter `msgif'.
   If `msgif' is NIL, the Msg module is used to output the message,
   else `msgif'.
*)

(*--------------------------------------------------------------------------*)
PROCEDURE Error(msgif : MsgIF.T; msg : TEXT);

(*--------------------------------------------------------------------------*)
PROCEDURE Fatal(msgif : MsgIF.T; msg : TEXT; exitCode : INTEGER := 1);

(*--------------------------------------------------------------------------*)
PROCEDURE Warning(msgif : MsgIF.T; msg : TEXT);

(*--------------------------------------------------------------------------*)
PROCEDURE Debug(msgif : MsgIF.T; msg : TEXT; level := 1);

(*--------------------------------------------------------------------------*)
PROCEDURE Error2(msgif : MsgIF.T; proc, msg : TEXT);

(*--------------------------------------------------------------------------*)
PROCEDURE Fatal2(msgif : MsgIF.T; proc, msg : TEXT; exitCode : INTEGER := 1);

(*--------------------------------------------------------------------------*)
PROCEDURE Warning2(msgif : MsgIF.T; proc, msg : TEXT);

(*--------------------------------------------------------------------------*)
PROCEDURE Debug2(msgif : MsgIF.T; proc, msg : TEXT; level := 1);

(*--------------------------------------------------------------------------*)
PROCEDURE V(msgif : MsgIF.T; msg : TEXT; unconditionalNewLine := TRUE;
            level := 1);

(*--------------------------------------------------------------------------*)
PROCEDURE V2(msgif : MsgIF.T; proc, msg : TEXT; unconditionalNewLine := TRUE;
             level := 1);

(*--------------------------------------------------------------------------*)
PROCEDURE T(msgif : MsgIF.T; msg : TEXT; unconditionalNewLine := TRUE;
            level := 1);

(*--------------------------------------------------------------------------*)
PROCEDURE T2(msgif : MsgIF.T; proc, msg : TEXT; unconditionalNewLine := TRUE;
             level := 1);

(*--------------------------------------------------------------------------*)
PROCEDURE D(msgif : MsgIF.T; msg : TEXT; unconditionalNewLine := TRUE;
            level := 1);

(*--------------------------------------------------------------------------*)
PROCEDURE D2(msgif : MsgIF.T; proc, msg : TEXT; unconditionalNewLine := TRUE;
             level := 1);

END MsgX.
