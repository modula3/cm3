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
 * $Id: Main.m3,v 1.1.1.1 2009-04-09 17:01:43 jkrell Exp $ *)

MODULE Main;

IMPORT ErrMsg, IO, Params, Pathname, Process, Secret, Text;

CONST HLine = "------------------------------------------------------" &
  "-------------------------\n";

PROCEDURE CountChar(t: TEXT; c: CHAR): CARDINAL =
  VAR
    pos := 0;
    n: CARDINAL := 0;
  BEGIN
    pos := Text.FindChar(t, c, pos);
    WHILE pos # -1 DO
      INC(n);
      pos := Text.FindChar(t, c, pos + 1);
    END;
    RETURN n;
  END CountChar;

PROCEDURE Usage() =
  VAR
    prog := Pathname.Last(Params.Get(0));
  BEGIN
    ErrMsg.Fatal("Usage: " & prog & " clientName serverName"
             & "\n       " & prog & " -v");
  END Usage;

VAR
  clientName, serverName: TEXT;
  sec, sec2: TEXT;
BEGIN
  IF Params.Count = 2 AND Text.Equal(Params.Get(1), "-v") THEN
    IO.Put("CVSup authentication key generator\n");
    IO.Put("Copyright 1999-2003 John D. Polstra\n");
    IO.Put("http://www.cvsup.org/\n");
    IO.Put("Report problems to cvsup-bugs@polstra.com\n");
    IO.Put("CVSup is a registered trademark of John D. Polstra\n");
    Process.Exit(0);
  END;
  IF Params.Count # 3 THEN
    Usage();
  END;
  clientName := Params.Get(1);
  serverName := Params.Get(2);
  IF CountChar(clientName, '@') # 1 OR CountChar(clientName, '.') = 0 THEN
    ErrMsg.Fatal("Client name must have the form of an e-mail address,"
      & "\ne.g., \"user@domain.com\"");
  END;
  IF CountChar(clientName, ':') # 0 THEN
    ErrMsg.Fatal("Client name must not contain \":\" characters");
  END;
  IF CountChar(serverName, '@') # 0 OR CountChar(serverName, '.') = 0 THEN
    ErrMsg.Fatal("Server name must be a fully-qualified domain name,"
      & "\ne.g., \"host.domain.com\"");
  END;
  IF CountChar(serverName, ':') # 0 THEN
    ErrMsg.Fatal("Server name must not contain \":\" characters");
  END;
  LOOP
    TRY
      sec := Secret.Make(serverName, clientName, "Enter password: ");
      sec2 := Secret.Make(serverName, clientName,
	"Enter same password again: ");
      IF Text.Equal(sec, sec2) THEN EXIT END;
      IO.Put("Passwords did not match.  Try again.\n");
    EXCEPT Secret.Error(msg) =>
      IO.Put("Sorry, " & msg & ".\n");
    END;
    IO.Put("\n");
  END;

  IO.Put("\nSend this line to the server administrator at " &
    serverName & ":\n");
  IO.Put(HLine);
  IO.Put(clientName & ":" & sec & "::\n");
  IO.Put(HLine);
  IO.Put("Be sure to send it using a secure channel!\n");

  IO.Put("\nAdd this line to your file \"$HOME/.cvsup/auth\", replacing " &
    "\"XXX\"\nwith the password you typed in:\n");
  IO.Put(HLine);
  IO.Put(serverName & ":" & clientName & ":XXX:\n");
  IO.Put(HLine);
  IO.Put("Make sure the file is readable and writable only by you!\n");
END Main.
