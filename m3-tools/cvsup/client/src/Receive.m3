(* Copyright 1997-2003 John D. Polstra.
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
 * $Id: Receive.m3,v 1.1.1.1 2009-04-09 17:01:39 jkrell Exp $ *)

MODULE Receive;

(* Subroutines used by the various file updaters for receiving file data
   from the network. *)

IMPORT
  EscapedRd, RCSDelta, RCSError, RCSFile, RCSPhrase, RCSPhrases,
  RCSRevNum, RCSString, Rd, RdCopy, SupMisc, Text, TextWr, Thread,
  TokScan, Wr;

PROCEDURE Counted(rd: Rd.T;
                  wr: Wr.T;
		  size: CARDINAL;
		  withChecksum: BOOLEAN): TEXT
  RAISES {Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted, TokScan.Error,
	  Wr.Failure} =
  VAR
    ts: TokScan.T;
    term: TEXT;
    wantSum: TEXT;
    errMsg: TEXT;
  BEGIN
    IF RdCopy.ToWriter(rd, wr, size) < size THEN
      RAISE Rd.EndOfFile;  (* Premature EOF from the wire. *)
    END;
    term := SupMisc.GetCmdLine(rd);

    (* We used to raise an error if the file grew on the server while it
       was being transferred.  Now we just ignore that case and take the
       number of bytes we originally decided upon, assuming we'll get the
       rest on the next update.  The previous policy caused big problems
       for huge mail archive files which took a very long time to transfer
       but were also grown frequently on the server host. *)
    IF Text.Equal(term, ".") OR Text.Equal(term, ".>") THEN
      errMsg := NIL;
    ELSIF Text.Equal(term, ".<") THEN
      errMsg := "File shrank on server";
    ELSE
      RAISE TokScan.Error("Invalid terminator for counted transfer");
    END;

    IF withChecksum THEN
      ts := TokScan.New(SupMisc.GetCmdLine(rd));
      ts.getLiteral("5");
      wantSum := ts.getToken("checksum");
      ts.getEnd("end of checksum for counted transfer");
    ELSE
      wantSum := NIL;
    END;

    IF errMsg # NIL THEN RAISE Error(errMsg) END;

    RETURN wantSum;
  END Counted;

PROCEDURE Delta(rd: Rd.T;
                rf: RCSFile.T;
		revNum: RCSRevNum.T;
		diffBaseRev: RCSRevNum.T;
		date: TEXT;
		author: TEXT): RCSDelta.T
  RAISES {RCSError.E, Rd.EndOfFile, Rd.Failure, Thread.Alerted,
	  TokScan.Error} =
  VAR
    line: TEXT;
    ts: TokScan.T;
    cmd: TEXT;
    cmdCh: CHAR;
    state: TEXT := "";
    log: RCSString.T := NIL;
    text: RCSString.T := NIL;
    treePhrases: RCSPhrases.T := NIL;
    textPhrases: RCSPhrases.T := NIL;
    key: TEXT;
    diffBase: RCSDelta.T;
    wr: TextWr.T;
  BEGIN
    LOOP
      line := SupMisc.GetCmdLine(rd);
      IF Text.Equal(line, ".") THEN EXIT END;
      ts := TokScan.New(line);
      cmdCh := ts.getChar("AddDelta command");
      cmd := Text.FromChar(cmdCh);
      CASE cmdCh OF
      | 'L' =>  (* Log. *)
	  ts.getEnd("end of \"" & cmd & "\" command");
	  <* FATAL Wr.Failure *>
	  BEGIN
	    wr := TextWr.New();
	    EVAL Escaped(rd, wr, withChecksum := FALSE);
	    log := RCSString.FromText(TextWr.ToText(wr));
	  END;
      | 'N' =>  (* Tree newphrases. *)
	  key := ts.getToken("Newphrase key");
	  ts.getEnd("end of \"" & cmd & "\" command");
	  Phrase(rd, key, treePhrases);
      | 'n' =>  (* Text newphrases. *)
	  key := ts.getToken("Newphrase key");
	  ts.getEnd("end of \"" & cmd & "\" command");
	  Phrase(rd, key, textPhrases);
      | 'S' =>  (* State. *)
	  state := ts.getToken("AddDelta state");
	  ts.getEnd("end of \"" & cmd & "\" command");
      | 'T' =>  (* Text. *)
	  ts.getEnd("end of \"" & cmd & "\" command");
	  <* FATAL Wr.Failure *>
	  BEGIN
	    wr := TextWr.New();
	    EVAL Escaped(rd, wr, withChecksum := FALSE);
	    text := RCSString.FromText(TextWr.ToText(wr));
	  END;
      ELSE
	RAISE TokScan.Error("Invalid AddDelta command \"" & cmd & "\"");
      END;
    END;
    IF log = NIL THEN  (* Just use an empty log. *)
      log := RCSString.FromText("");
    END;
    IF text = NIL THEN
      RAISE TokScan.Error("New delta has no TEXT");
    END;
    IF Text.Equal(diffBaseRev, ".") THEN
      diffBase := NIL;
    ELSE
      diffBase := RCSFile.GetDelta(rf, diffBaseRev);
    END;
    RETURN RCSFile.AddDelta(rf,
      revNum := revNum,
      diffBase := diffBase,
      date := date,
      author := author,
      state := state,
      log := log,
      text := text,
      treePhrases := treePhrases,
      textPhrases := textPhrases);
  END Delta;

PROCEDURE Phrase(rd: Rd.T;
	         key: TEXT;
                 VAR phrases: RCSPhrases.T)
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted, TokScan.Error} =
  VAR
    line: TEXT;
    ts: TokScan.T;
    cmd: TEXT;
    cmdCh: CHAR;
    word: TEXT;
    phrase := RCSPhrase.New(key);
  BEGIN
    LOOP
      line := SupMisc.GetCmdLine(rd);
      IF Text.Equal(line, ".") THEN EXIT END;
      ts := TokScan.New(line);
      cmdCh := ts.getChar("AddDelta command");
      cmd := Text.FromChar(cmdCh);
      CASE cmdCh OF
      | 'W' =>
	  word := ts.getToken("Newphrase word");
	  ts.getEnd("end of \"" & cmd & "\" command");
	  RCSPhrase.Append(phrase, word, isString := FALSE);
      | 'S' =>
	  ts.getEnd("end of \"" & cmd & "\" command");
	  <* FATAL Wr.Failure *>
	  VAR
	    wr := TextWr.New();
	  BEGIN
	    EVAL Escaped(rd, wr, withChecksum := FALSE);
	    word := TextWr.ToText(wr);
	  END;
	  RCSPhrase.Append(phrase, word, isString := TRUE);
      ELSE
	RAISE TokScan.Error("Invalid newphrases command \"" & cmd & "\"");
      END;
    END;
    IF phrases = NIL THEN
      phrases := RCSPhrases.New();
    END;
    RCSPhrases.Append(phrases, phrase);
  END Phrase;

PROCEDURE Escaped(rd: Rd.T;
                  wr: Wr.T;
		  withChecksum: BOOLEAN): TEXT
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted, TokScan.Error,
	  Wr.Failure} =
  VAR
    eRd := NEW(EscapedRd.T).init(rd, closeChild := FALSE);
    ts: TokScan.T;
    checkSum: TEXT := NIL;
  BEGIN
    TRY
      TRY
	EVAL RdCopy.ToWriter(eRd, wr);
      EXCEPT Rd.Failure(l) =>
	IF l.head = EscapedRd.PrematureEOF THEN
	  RAISE Rd.EndOfFile;
	ELSE
	  RAISE Rd.Failure(l);
	END;
      END;
    FINALLY
      Rd.Close(eRd);
    END;
    IF withChecksum THEN
      ts := TokScan.New(SupMisc.GetCmdLine(rd));
      ts.getLiteral("5");
      checkSum := ts.getToken("checksum");
      ts.getEnd("end of checksum for counted transfer");
    END;
    RETURN checkSum;
  END Escaped;

BEGIN
END Receive.
