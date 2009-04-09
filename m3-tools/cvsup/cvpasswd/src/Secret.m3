(* Copyright 1999-2003 John D. Polstra.
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
 * $Id: Secret.m3,v 1.1.1.1 2009-04-09 17:01:43 jkrell Exp $ *)

UNSAFE MODULE Secret;

IMPORT AuthMD5, CText, Ctypes, M3toC, Upass;

PROCEDURE Make(realm: TEXT; user: TEXT; prompt: TEXT): TEXT
  RAISES {Error} =
  VAR
    pass, p: Ctypes.char_star;
    secret: TEXT;
  BEGIN
    WITH s = CText.SharedTtoS(prompt) DO
      pass := Upass.getpass(s);
      CText.FreeSharedS(prompt, s);
    END;
    TRY
      p := pass;
      WHILE p^ # 0 DO
	IF p^ = ORD(':') THEN
	  RAISE Error("password must not contain \":\" characters");
	END;
	INC(p);
      END;
      secret := AuthMD5.MakeSecret(realm, user, M3toC.StoT(pass));
      RETURN secret;
    FINALLY
      (* Obliterate the cleartext password. *)
      p := pass;
      WHILE p^ # 0 DO
	p^ := 0;
	INC(p);
      END;
    END;
  END Make;

BEGIN
END Secret.
