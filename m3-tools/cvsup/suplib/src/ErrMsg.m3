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
 * $Id: ErrMsg.m3,v 1.1.1.1 2009-04-09 17:01:53 jkrell Exp $ *)

UNSAFE MODULE ErrMsg;

IMPORT
  Atom, AtomList, Cstring, EscapedRd, IO, IP, M3toC, Process, Text, TokScan;

IMPORT SupTCP AS TCP;

FROM Stdio IMPORT stderr;

TYPE
  ErrRec = RECORD
    atom: Atom.T;
    msg: TEXT;
  END;

VAR
  ErrInfo := ARRAY [0..9] OF ErrRec{
    ErrRec{ IP.LookupFailure,	      "Host name lookup failed" },
    ErrRec{ IP.Unreachable,	      "Destination unreachable" },
    ErrRec{ IP.PortBusy,	      "Port in use" },
    ErrRec{ IP.NoResources,	      "Out of resources" },

    ErrRec{ TCP.Refused,	      "Connection refused" },
    ErrRec{ TCP.Closed,		      "Connection closed" },
    ErrRec{ TCP.Timeout,	      "Connection timed out" },
    ErrRec{ TCP.ConnLost,             "Connection lost" },

    ErrRec{ EscapedRd.InvalidEscape,  "Invalid \".\" escape" },
    ErrRec{ EscapedRd.PrematureEOF,   "Premature EOF in escaped text" }
  };

PROCEDURE Fatal(msg: TEXT; errList: AtomList.T := NIL) =
  BEGIN
    IF errList # NIL THEN
      msg := msg & ": " & StrError(errList);
    END;
    IO.Put(msg & "\n", stderr);
    Process.Exit(1);
  END Fatal;

PROCEDURE GetErrno(errList: AtomList.T; VAR (*OUT*) errno: INTEGER): BOOLEAN =
  VAR
    t: TEXT;
  BEGIN
    WHILE errList # NIL DO
      t := Atom.ToText(errList.head);
      IF Text.Equal(Text.Sub(t, 0, 6), "errno=") THEN
	t := Text.Sub(t, 6);
      END;
      TRY
	errno := TokScan.AtoI(t);
	RETURN TRUE;
      EXCEPT TokScan.Error => (* Continue *) END;
      errList := errList.tail;
    END;
    RETURN FALSE;
  END GetErrno;

PROCEDURE StrError(l: AtomList.T): TEXT =
  VAR
    msg: TEXT;
    ts: TokScan.T;
    errno: CARDINAL;
  BEGIN
    FOR i := FIRST(ErrInfo) TO LAST(ErrInfo) DO
      IF l.head = ErrInfo[i].atom THEN
	RETURN ErrInfo[i].msg;
      END;
    END;

    IF l.head = TCP.Unexpected AND l.tail # NIL THEN
      TRY
	errno := TokScan.AtoI(Atom.ToText(l.tail.head));
	RETURN M3toC.CopyStoT(Cstring.strerror(errno));
      EXCEPT TokScan.Error => (* Continue *) END;
    END;

    msg := Atom.ToText(l.head);
    l := l.tail;
    TRY
      ts := TokScan.New(msg, SET OF CHAR{'='});
      ts.getLiteral("errno");
      errno := ts.getInt();
      msg := M3toC.CopyStoT(Cstring.strerror(errno));
    EXCEPT TokScan.Error => (* Continue *) END;

    WHILE l # NIL DO  (* Tack on any extra context that might be there. *)
      msg := msg & ": " & Atom.ToText(l.head);
      l := l.tail;
    END;

    RETURN msg;
  END StrError;

BEGIN
END ErrMsg.
