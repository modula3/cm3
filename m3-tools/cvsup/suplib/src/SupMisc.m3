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
 * $Id: SupMisc.m3,v 1.2 2009-04-12 05:33:21 jkrell Exp $ *)

MODULE SupMisc;

IMPORT
  FileAttr, Fmt, FS, Glob, GlobTree, IP, OSError, OSErrorPosix,
  Pathname, Process, Random, Rd, RdClass, RegEx, SupFileRec, Text,
  TextArraySort, TextSeq, Thread, TokScan, Uerror;
IMPORT TCP;

TYPE
  AtomicCounter = MUTEX OBJECT
    count: CARDINAL := 0;
  END;

CONST
  TempStemPrefix = "#cvs.cvsup-";

VAR
  TempStem: TEXT := NIL;
  tempCount := NEW(AtomicCounter);
  RCSSuffixLength := Text.Length(RCSSuffix);

  portMu := NEW(MUTEX);  (* Monitors "portRandom" and "nextPort". *)
  portRandom: Random.T := NIL;
  nextPort: IP.Port := IP.NullPort;

PROCEDURE AtticName(name: Pathname.T): Pathname.T =
  VAR
    fileName := PathLast(name);
    dirName := PathPrefix(name);
  BEGIN
    IF Text.Equal(PathLast(dirName), CVSAttic) THEN  (* Already in Attic. *)
      RETURN name;
    ELSE
      RETURN CatPath(CatPath(dirName, CVSAttic), fileName);
    END;
  END AtticName;

PROCEDURE CatPath(p1, p2: Pathname.T): Pathname.T =
  VAR
    nSlash: [0..2];
  BEGIN
    IF Text.Empty(p1) THEN RETURN p2 END;
    IF Text.Empty(p2) THEN RETURN p1 END;
    nSlash := 0;
    IF Text.GetChar(p1, Text.Length(p1)-1) = SlashChar THEN INC(nSlash) END;
    IF Text.GetChar(p2, 0) = SlashChar THEN INC(nSlash) END;
    CASE nSlash OF
    | 0 => RETURN Cat3(p1, SlashText, p2);
    | 1 => RETURN p1 & p2;
    | 2 => RETURN p1 & Text.Sub(p2, 1);
    END;
  END CatPath;

PROCEDURE CheckoutName(name: Pathname.T): Pathname.T =
  BEGIN
    (* FIXME - If the name is too short, i.e., not an RCS name, this
       gets a runtime error.  Is that OK? *)
    RETURN Text.Sub(name, 0, Text.Length(name) - RCSSuffixLength);
  END CheckoutName;

PROCEDURE CommonLength(a, b: TEXT): CARDINAL =
  BEGIN
    WITH len = MIN(Text.Length(a), Text.Length(b)) DO
      FOR i := 0 TO len-1 DO
	IF Text.GetChar(a, i) # Text.GetChar(b, i) THEN RETURN i END;
      END;
      RETURN len;
    END;
  END CommonLength;

PROCEDURE ExpandFilenames(prefix: Pathname.T;
                          names: TextSeq.T): TextSeq.T =
  VAR
    oldSize := names.size();
    newNames: TextSeq.T;
    arr: REF ARRAY OF TEXT;
    cur, prev: TEXT;
    curLen, prevLen: CARDINAL;
  BEGIN
    (* Copy the names sequence, expanding wildcards as we go. *)
    newNames := NEW(TextSeq.T).init(2 * oldSize);
    FOR i := 0 TO oldSize-1 DO
      cur := names.get(i);
      IF Text.FindChar(cur, '*') # -1
      OR Text.FindChar(cur, '?') # -1
      OR Text.FindChar(cur, '[') # -1
      OR Text.FindChar(cur, '{') # -1 THEN
	(* FIXME - Expand wildcards. *)
      ELSE
	TRY
	  IF FS.Status(CatPath(prefix, cur)).type = FS.DirectoryFileType THEN
	    newNames.addhi(cur);
	  ELSE
	    (* FIXME - Handle regular files. *)
	  END;
	EXCEPT OSError.E => (* Ignore non-existent files. *) END;
      END;
    END;

    (* Copy the expanded list of names into an array, and sort it. *)
    arr := NEW(REF ARRAY OF TEXT, newNames.size());
    FOR i := FIRST(arr^) TO LAST(arr^) DO
      arr[i] := newNames.get(i);
    END;
    TextArraySort.Sort(arr^, PathCompare);

    (* Copy the sorted array into a new sequence, eliminating overlaps. *)
    newNames := NEW(TextSeq.T).init(NUMBER(arr^));
    prev := NIL;
    FOR i := FIRST(arr^) TO LAST(arr^) DO
      cur := arr[i];
      curLen := Text.Length(cur);
      IF prev = NIL OR CommonLength(prev, cur) < prevLen
      OR curLen > prevLen AND Text.GetChar(cur, prevLen) # SlashChar
      THEN  (* No overlap. *)
	newNames.addhi(cur);
	prev := cur;
	prevLen := curLen;
      END;
    END;

    RETURN newNames;
  END ExpandFilenames;

PROCEDURE FindFile(base: Pathname.T;
                   searchPath: TEXT;
		   file: Pathname.T): Pathname.T =
  <* FATAL TokScan.Error *>
  VAR
    ts := TokScan.New(searchPath, SET OF CHAR{':'});
    dir: Pathname.T;
    path: Pathname.T;
  BEGIN
    WHILE ts.next(dir) DO
      path := ResolvePath(base, ResolvePath(dir, file));
      TRY
	EVAL FileAttr.FromPathname(path, follow := TRUE);
	RETURN path;
      EXCEPT OSError.E => (* Keep going. *) END;
    END;
    RETURN NIL;
  END FindFile;

PROCEDURE FilterPathList(searchPath: TEXT;
                         cl: TextPredicateClosure): TEXT =
  <* FATAL TokScan.Error *>
  VAR
    dir: TEXT;
    ts := TokScan.New(searchPath, SET OF CHAR{':'});
    res: TEXT := NIL;
  BEGIN
    WHILE ts.next(dir) DO
      IF cl.matches(dir) THEN
        IF res = NIL THEN
          res := dir;
        ELSE
          res := res & ":" & dir;
        END;
      END;
    END;
    IF res = NIL THEN
      res := "";
    END;
    RETURN res;
  END FilterPathList; 

(* This code is adapted from Rd.GetLine in DEC SRC's Modula-3 3.6 release. *)
PROCEDURE GetCmdLine(rd: Rd.T): TEXT
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  VAR
    txt := "";
    j, n: INTEGER;
  BEGIN
    RdClass.Lock (rd);
    TRY
      LOOP (* INV: txt contains the partial result *)
        IF rd.cur = rd.hi THEN
	  <* ASSERT NOT rd.closed *>
          IF rd.seek (rd.cur, FALSE) = RdClass.SeekResult.Eof THEN
            RAISE Rd.EndOfFile;
          END;
        END;
        (* rd is ready *)
        n := rd.hi - rd.lo + rd.st;
        j := rd.cur - rd.lo + rd.st;
        WHILE (j # n) AND rd.buff[j] # '\n' DO INC(j) END;
        VAR rd_cur := rd.cur - rd.lo + rd.st;
            len := j - rd_cur;
         BEGIN
          IF len >= 1 AND j # n  AND rd.buff[j-1] = '\r' THEN
            (* segment ends in \r\n *)
            txt := txt & Text.FromChars (SUBARRAY (rd.buff^, rd_cur, len-1));
            INC (rd.cur, len+1);
            RETURN txt;
          ELSIF j # n THEN
            (* segment ends in \n *)
            txt := txt & Text.FromChars (SUBARRAY (rd.buff^, rd_cur, len));
            INC (rd.cur, len+1);
            IF NOT Text.Empty(txt) AND
                 Text.GetChar(txt, Text.Length(txt)-1) = '\r' THEN
              txt := Text.Sub(txt, 0, Text.Length(txt)-1)
            END;
            RETURN txt;
          ELSE
            (* segment does not contain line break *)
            txt := txt & Text.FromChars (SUBARRAY (rd.buff^, rd_cur, len));
            INC (rd.cur, len);
          END;
        END;
      END; (* LOOP *)
    FINALLY
      RdClass.Unlock (rd);
    END;
  END GetCmdLine;

PROCEDURE IsBlankLine(t: TEXT): BOOLEAN =
  BEGIN
    FOR i := 0 TO Text.Length(t)-1 DO
      IF NOT Text.GetChar(t, i) IN SET OF CHAR{' ', '\t', '\r', '\n'} THEN
	RETURN FALSE;
      END;
    END;
    RETURN TRUE;
  END IsBlankLine;

PROCEDURE IsDirectory(name: Pathname.T): BOOLEAN =
  BEGIN
    TRY
      RETURN FS.Status(name).type = FS.DirectoryFileType;
    EXCEPT OSError.E =>
      RETURN FALSE;
    END;
  END IsDirectory;

PROCEDURE IsRCS(p: Pathname.T): BOOLEAN =
  BEGIN
    WITH suffixPos = Text.Length(p) - RCSSuffixLength DO
      IF suffixPos <= 0 THEN
	RETURN FALSE;
      END;
      FOR i := 0 TO RCSSuffixLength-1 DO
	IF Text.GetChar(p, suffixPos+i) # Text.GetChar(RCSSuffix, i) THEN
	  RETURN FALSE;
	END;
      END;
      RETURN TRUE;
    END;
  END IsRCS;

PROCEDURE LiveName(name: Pathname.T): Pathname.T =
  VAR
    fileName := PathLast(name);
    dirName := PathPrefix(name);
  BEGIN
    IF Text.Equal(PathLast(dirName), CVSAttic) THEN
      RETURN CatPath(PathPrefix(dirName), fileName);
    ELSE  (* Already live. *)
      RETURN name;
    END;
  END LiveName;

PROCEDURE MakeDirectories(path: Pathname.T; umask := -1)
  RAISES {OSError.E} =
  VAR
    head := PathPrefix(path);
    tail: TextSeq.T := NIL;
    attr := NEW(FileAttr.T).init(FileAttr.FileType.Directory);
  BEGIN
    attr := FileAttr.MergeDefault(attr);
    attr := FileAttr.Umask(attr, umask);
    (* Successively remove trailing directories until we find a path
       that exists. *)
    WHILE NOT Text.Empty(head) DO
      TRY
	EVAL FS.Status(head);
	EXIT;
      EXCEPT OSError.E(list) =>
	IF OSErrorPosix.AtomToErrno(list.head) = Uerror.ENOENT THEN
	  IF tail = NIL THEN tail := NEW(TextSeq.T).init(30) END;
	  WITH last = PathLast(head) DO
	    (* Without the tests for "Pathname.Current" and empty, we
	       would get a "file exists" error down below when we
	       tried to create the same directory twice. *)
	    IF NOT Text.Equal(last, Pathname.Current)
	    AND NOT Text.Empty(last) THEN
	      tail.addlo(last);
	    END;
	  END;
	  head := PathPrefix(head);
	ELSE
	  RAISE OSError.E(list);
	END;
      END;
    END;

    (* Make the needed directories and set their attributes. *)
    IF tail # NIL THEN
      WHILE tail.size() > 0 DO
	head := CatPath(head, tail.remlo());
	FileAttr.MakeNode(attr, head);
	EVAL FileAttr.Install(attr, head);
      END;
    END;
  END MakeDirectories;

PROCEDURE NewConnector(addr: IP.Address;
                       loPort, hiPort: IP.Port): TCP.Connector
  RAISES {IP.Error} =
  VAR
    ep: IP.Endpoint;
    firstPort: IP.Port;
  BEGIN
    ep.addr := addr;

    IF hiPort = IP.NullPort THEN
      hiPort := loPort;
    ELSIF loPort = IP.NullPort THEN
      loPort := hiPort;
    ELSIF loPort > hiPort THEN
      VAR t := loPort;
      BEGIN loPort := hiPort;  hiPort := t END;
    END;
    IF loPort = hiPort THEN
      ep.port := loPort;
      RETURN TCP.NewConnector(ep);
    END;

    LOCK portMu DO
      IF NOT (loPort <= nextPort AND nextPort <= hiPort) THEN
	IF portRandom = NIL THEN  (* First call by this process. *)
	  portRandom := NEW(Random.Default).init();
	END;
	nextPort := portRandom.integer(loPort, hiPort);
      END;
      firstPort := nextPort;
      LOOP
	ep.port := nextPort;
	IF nextPort >= hiPort THEN nextPort := loPort ELSE INC(nextPort) END;
	TRY
	  RETURN TCP.NewConnector(ep);
	EXCEPT IP.Error(list) =>
	  IF list.head # IP.PortBusy OR nextPort = firstPort THEN
	    RAISE IP.Error(list);
	  END;
	END;
      END;
    END;
  END NewConnector;

PROCEDURE ParseHost(t: TEXT; VAR (*OUT*) res: IP.Address): BOOLEAN
  RAISES {IP.Error} =
  BEGIN
    TRY
      res := ParseIPAddress(t);
      RETURN TRUE;
    EXCEPT BadAddress =>
      RETURN IP.GetHostByName(t, res);
    END;
  END ParseHost;

PROCEDURE ParseIPAddress(t: TEXT; netOK := FALSE): IP.Address
  RAISES {BadAddress} =
  CONST
    Digits = SET OF CHAR{'0'..'9'};
  VAR
    addr: IP.Address;
    octet: CARDINAL;
    len := Text.Length(t);
    pos := 0;
  BEGIN
    FOR i := FIRST(addr.a) TO LAST(addr.a) DO
      IF pos = len THEN  (* Default the trailing octets to 0. *)
	IF NOT netOK THEN RAISE BadAddress END;
	octet := 0;
      ELSE
	IF i # FIRST(addr.a) THEN  (* Get the '.' *)
	  IF Text.GetChar(t, pos) # '.' THEN RAISE BadAddress END;
	  INC(pos);
	  IF pos = len THEN RAISE BadAddress END;
	END;
	IF NOT Text.GetChar(t, pos) IN Digits THEN RAISE BadAddress END;
	octet := 0;
	REPEAT
	  octet := 10*octet + ORD(Text.GetChar(t, pos)) - ORD('0');
	  INC(pos);
	UNTIL pos = len OR NOT Text.GetChar(t, pos) IN Digits;
	IF octet < 0 OR octet > 255 THEN RAISE BadAddress END;
      END;
      addr.a[i] := octet;
    END;
    IF pos # len THEN RAISE BadAddress END;
    RETURN addr;
  END ParseIPAddress;

PROCEDURE PathLast(p: Pathname.T): Pathname.T =
  BEGIN
    WITH pos = Text.FindCharR(p, SlashChar) DO
      IF pos = -1 THEN
	RETURN p;
      ELSE
	RETURN Text.Sub(p, pos+1);
      END;
    END;
  END PathLast;

PROCEDURE PathPrefix(p: Pathname.T): Pathname.T =
  BEGIN
    WITH pos = Text.FindCharR(p, SlashChar) DO
      IF pos = -1 THEN
	RETURN "";
      ELSIF pos = 0 THEN
	RETURN SlashText;
      ELSE
	RETURN Text.Sub(p, 0, pos);
      END;
    END;
  END PathPrefix;

PROCEDURE PatternMatch(pattern: TEXT;
                       options := Glob.MatchOptions{}): GlobTree.T
                       RAISES {RegEx.Error} =
  BEGIN
    IF Text.Length(pattern) > 0 AND Text.GetChar(pattern, 0) = '+' THEN
      (* Remove the '+' and interpret it as a regular expression. *)
      pattern := Text.Sub(pattern, 1);
      (* Add anchors at the beginning and end, if needed. *)
      IF Text.Length(pattern) = 0 OR Text.GetChar(pattern, 0) # '^' THEN
        pattern := "^" & pattern;
      END;
      <* ASSERT Text.Length(pattern) > 0 *>
      IF Text.GetChar(pattern, Text.Length(pattern) - 1) # '$' THEN
        pattern := pattern & "$";
      END;
      IF Text.Equal(pattern, "^.*$") THEN
        RETURN GlobTree.True;
      END;
      RETURN GlobTree.RegExMatch(pattern);
    ELSE  (* Treat it as a shell pattern. *)
      IF Text.Equal(pattern, "*") THEN
        RETURN GlobTree.True;
      END;
      RETURN GlobTree.Match(pattern, options);
    END;
  END PatternMatch;

PROCEDURE RCSName(name: Pathname.T): Pathname.T =
  BEGIN
    RETURN name & RCSSuffix;
  END RCSName;

PROCEDURE ResolvePath(p1, p2: Pathname.T): Pathname.T =
  BEGIN
    IF Pathname.Absolute(p2) THEN
      RETURN p2;
    ELSE
      RETURN CatPath(p1, p2);
    END;
  END ResolvePath;

PROCEDURE StatusFileName(sfr: SupFileRec.T): Pathname.T =
  BEGIN
    RETURN "checkouts" & StatusFileSuffix(sfr);
  END StatusFileName;

PROCEDURE StatusFileSuffix(sfr: SupFileRec.T): TEXT =
  VAR
    suffix, tag: TEXT;
  BEGIN
    IF sfr.listSuffix # NIL THEN
      suffix := "." & sfr.listSuffix;
    ELSIF SupFileRec.Option.UseRelSuffix IN sfr.options THEN
      suffix := "";
      IF sfr.release # NIL THEN
	suffix := suffix & "." & sfr.release;
      END;
      IF SupFileRec.Option.CheckoutMode IN sfr.options THEN
	tag := sfr.checkoutTag;
	IF tag = NIL THEN tag := "." END;
	suffix := suffix & ":" & tag;
      END;
    ELSE
      suffix := "";
    END;
    RETURN suffix;
  END StatusFileSuffix;

PROCEDURE TempName(p: Pathname.T): Pathname.T =
  VAR
    count: CARDINAL;
  BEGIN
    LOCK tempCount DO
      IF TempStem = NIL THEN  (* First call. *)
	(* We do this here, rather than in the module initialization
	  code, because our process ID may have changed due to forking
	  in "daemon". *)
	TempStem := TempStemPrefix & Fmt.Int(Process.GetMyID());
      END;
      count := tempCount.count;
      INC(tempCount.count);
    END;

    WITH lastSlash = Text.FindCharR(p, SlashChar),
    name = TempStem & "." & Fmt.Int(count) DO
      IF lastSlash = -1 THEN
	RETURN name;
      ELSE
	RETURN Text.Sub(p, 0, lastSlash+1) & name;
      END;
    END;
  END TempName;

BEGIN
END SupMisc.
