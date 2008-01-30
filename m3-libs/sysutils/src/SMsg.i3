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
 * $Id: SMsg.i3,v 1.1 2008-01-30 23:45:36 wagner Exp $ *)

(*--------------------------------------------------------------------------*)
INTERFACE SMsg;

IMPORT Wr;

(*--------------------------------------------------------------------------*)
PROCEDURE Error(msg : TEXT);
  (* write `msg' tagged as error to `errorWr' *)

(*--------------------------------------------------------------------------*)
PROCEDURE Fatal(msg : TEXT; exitCode : INTEGER := 1);
  (* write `msg' tagged as fatal error to `fatalWr' and terminate 
     the program *)

(*--------------------------------------------------------------------------*)
PROCEDURE Warning(msg : TEXT);
  (* write `msg' tagged as warning to `warningWr' *)

(*--------------------------------------------------------------------------*)
PROCEDURE Debug(msg : TEXT);
  (* write `msg' tagged as debug to `debugWr' *)

(*--------------------------------------------------------------------------*)
PROCEDURE Error2(proc, msg : TEXT);
  (* write `proc: msg' tagged as error to `errorWr' *)

(*--------------------------------------------------------------------------*)
PROCEDURE Fatal2(proc, msg : TEXT; exitCode : INTEGER := 1);
  (* write `proc: msg' tagged as fatal error to `fatalWr' and terminate 
     the program *)

(*--------------------------------------------------------------------------*)
PROCEDURE Warning2(proc, msg : TEXT);
  (* write `proc: msg' tagged as warning to `warningWr' *)

(*--------------------------------------------------------------------------*)
PROCEDURE Debug2(proc, msg : TEXT);
  (* write `proc: msg' tagged as debug to `debugWr' *)

(*--------------------------------------------------------------------------*)
PROCEDURE V(msg : TEXT; unconditionalNewLine := TRUE);
  (* write `msg' to `vWr' if vFlag set *)

(*--------------------------------------------------------------------------*)
PROCEDURE V2(proc, msg : TEXT; unconditionalNewLine := TRUE);
  (* write `proc: msg' to `vWr' if vFlag set *)

(*--------------------------------------------------------------------------*)
PROCEDURE T(msg : TEXT; unconditionalNewLine := TRUE);
  (* write `msg' to `tWr' if tFlag set *)

(*--------------------------------------------------------------------------*)
PROCEDURE T2(proc, msg : TEXT; unconditionalNewLine := TRUE);
  (* write `proc: msg' to `tWr' if tFlag set *)

(*--------------------------------------------------------------------------*)
PROCEDURE D(msg : TEXT; unconditionalNewLine := TRUE);
  (* write `msg' to `dWr' if dFlag set *)

(*--------------------------------------------------------------------------*)
PROCEDURE D2(proc, msg : TEXT; unconditionalNewLine := TRUE);
  (* write `proc: msg' to `dWr' if dFlag set *)

(*--------------------------------------------------------------------------*)
VAR
  errorWr   : Wr.T;
  fatalWr   : Wr.T;
  warningWr : Wr.T;
  debugWr   : Wr.T;
  vWr       : Wr.T;
  tWr       : Wr.T;
  dWr       : Wr.T;
  vFlag     : BOOLEAN;
  tFlag     : BOOLEAN;
  dFlag     : BOOLEAN;
  beepFlag  : BOOLEAN;
END SMsg.
