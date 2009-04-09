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
 * $Id: RCSKeyword.m3,v 1.1.1.1 2009-04-09 17:01:58 jkrell Exp $ *)

MODULE RCSKeyword;

IMPORT
  Pathname, RCSDate, RCSDelta, RCSDeltaClass, RCSError, RCSString,
  SupMisc, Text, TextIntTbl, TextRefTbl;

EXCEPTION Fatal(TEXT);
<* FATAL Fatal *>

REVEAL
  T = Public BRANDED OBJECT
    keyTab: TextRefTbl.T;	(* Name => KeyRec. *)
    keyLenMin: CARDINAL;
    keyLenMax: CARDINAL;
  OVERRIDES
    init := Init;
    alias := Alias;
    enable := Enable;
    enableAll := EnableAll;
    expand := Expand;
  END;

TYPE
  Iterator = RCSString.Iterator OBJECT
    t: T;
    textIter: RCSString.Iterator;
    hideAttic: BOOLEAN;
    mode: ExpandMode;
    cvsRoot: Pathname.T;
    name: Pathname.T;		(* Relative to "cvsRoot". *)
    delta: RCSDelta.T;
    tag: TEXT;
    logText: REF ARRAY OF TEXT := NIL;
    logLineNum: CARDINAL := 0;
  OVERRIDES
    next := Next;
  END;

  KeyRec = REF RECORD
    keyword: Keyword;
    enabled: BOOLEAN;
  END;

  Keyword = {
    Author,
    CVSHeader,
    Date,
    Header,
    Id,
    Locker,
    Log,
    Name,
    RCSfile,
    Revision,
    Source,
    State
  };

  KeyInitRec = RECORD
    name: TEXT;
    keyword: Keyword;
  END;

  InitRec = RECORD
    name: TEXT;
    value: INTEGER;
  END;

CONST
  KeyInit = ARRAY OF KeyInitRec{
    KeyInitRec{ "Author",     Keyword.Author     },
    KeyInitRec{ "CVSHeader",  Keyword.CVSHeader  },
    KeyInitRec{ "Date",       Keyword.Date       },
    KeyInitRec{ "Header",     Keyword.Header     },
    KeyInitRec{ "Id",         Keyword.Id         },
    KeyInitRec{ "Locker",     Keyword.Locker     },
    KeyInitRec{ "Log",        Keyword.Log        },
    KeyInitRec{ "Name",       Keyword.Name       },
    KeyInitRec{ "RCSfile",    Keyword.RCSfile    },
    KeyInitRec{ "Revision",   Keyword.Revision   },
    KeyInitRec{ "Source",     Keyword.Source     },
    KeyInitRec{ "State",      Keyword.State      }
  };
  ExpandInit = ARRAY OF InitRec{
    InitRec{ ".",   ORD(ExpandMode.Default)        },
    InitRec{ "kv",  ORD(ExpandMode.KeyValue)       },
    InitRec{ "kvl", ORD(ExpandMode.KeyValueLocker) },
    InitRec{ "k",   ORD(ExpandMode.Key)            },
    InitRec{ "o",   ORD(ExpandMode.Old)            },
    InitRec{ "b",   ORD(ExpandMode.Binary)         },
    InitRec{ "v",   ORD(ExpandMode.Value)          }
  };

VAR
  decodeTab := NEW(TextIntTbl.Default).init(2*NUMBER(ExpandInit));
  encodeTab: ARRAY ExpandMode OF TEXT;

PROCEDURE Alias(self: T;
                newName, oldName: TEXT)
  RAISES {Unknown} =
  VAR
    oldRef: REFANY;
    newKeyRec: KeyRec;
  BEGIN
    IF NOT self.keyTab.get(oldName, oldRef) THEN
      RAISE Unknown;
    END;
    WITH oldKeyRec = NARROW(oldRef, KeyRec) DO
      newKeyRec := NEW(KeyRec);
      newKeyRec^ := oldKeyRec^;
      EVAL self.keyTab.put(newName, newKeyRec);
    END;
  END Alias;

PROCEDURE Enable(self: T;
                 name: TEXT;
		 enabled := TRUE)
  RAISES {Unknown} =
  VAR
    ref: REFANY;
  BEGIN
    IF NOT self.keyTab.get(name, ref) THEN
      RAISE Unknown;
    END;
    WITH keyRec = NARROW(ref, KeyRec) DO
      keyRec.enabled := enabled;
    END;
  END Enable;

PROCEDURE EnableAll(self: T;
		    enabled := TRUE) =
  VAR
    name: TEXT;
    ref: REFANY;
    iter: TextRefTbl.Iterator;
  BEGIN
    iter := self.keyTab.iterate();
    WHILE iter.next(name, ref) DO
      WITH keyRec = NARROW(ref, KeyRec) DO
	keyRec.enabled := enabled;
      END;
    END;
  END EnableAll;

PROCEDURE Expand(self: T;
                 textIter: RCSString.Iterator;
		 hideAttic: BOOLEAN;
		 cvsRoot: Pathname.T;
		 name: Pathname.T;
                 delta: RCSDelta.T; 
		 tag: TEXT := NIL;
		 mode := ExpandMode.Default): RCSString.Iterator =
  BEGIN
    IF mode = ExpandMode.Default THEN
      mode := delta.rcsFile.expand;
    END;

    CASE mode OF
    | ExpandMode.Old, ExpandMode.Binary => RETURN textIter;
    ELSE
      RETURN NEW(Iterator,
	t := self,
	textIter := textIter,
	hideAttic := hideAttic,
	mode := mode,
	cvsRoot := cvsRoot,
	name := name,
	delta := delta,
	tag := tag);
    END;
  END Expand;

PROCEDURE Init(self:T): T =
  BEGIN
    self.keyLenMin := LAST(CARDINAL);
    self.keyLenMax := FIRST(CARDINAL);
    self.keyTab := NEW(TextRefTbl.Default).init(2*NUMBER(KeyInit));
    FOR i := FIRST(KeyInit) TO LAST(KeyInit) DO
      EVAL self.keyTab.put(KeyInit[i].name, NEW(KeyRec,
	keyword := KeyInit[i].keyword,
	enabled := TRUE));
      WITH len = Text.Length(KeyInit[i].name) DO
	self.keyLenMin := MIN(self.keyLenMin, len);
	self.keyLenMax := MAX(self.keyLenMax, len);
      END;
    END;  
    RETURN self;
  END Init;

PROCEDURE Next(self: Iterator; VAR line: RCSString.T): BOOLEAN =
  VAR
    text: TEXT;
    dollar: INTEGER;    (* The leading '$'. *)
    keyStart: INTEGER;  (* First letter of keyword, just after the '$'. *)
    valStart: INTEGER;  (* The ':', if any, else the terminating '$'. *)
    valLim: INTEGER;    (* The terminating '$'. *)
    keyRecRef: REFANY;
    newVal: TEXT;
    changed: BOOLEAN;
  BEGIN
    IF self.logText # NIL THEN  (* Expanding a Log keyword. *)
      text := self.logText[self.logLineNum];
      line := RCSString.FromText(text);
      INC(self.logLineNum);
      IF self.logLineNum >= NUMBER(self.logText^) THEN  (* Reset. *)
	self.logText := NIL;
	self.logLineNum := 0;
      END;
      RETURN TRUE;
    END;

    IF NOT self.textIter.next(line) THEN
      RETURN FALSE;
    END;

    text := line.toText();
    changed := FALSE;
    valLim := -1;
    LOOP
      dollar := Text.FindChar(text, '$', valLim+1);
      IF dollar = -1 THEN  (* No more '$' characters, we're done. *)
	EXIT;
      END;
      keyStart := dollar + 1;
      valLim := Text.FindChar(text, '$', keyStart);
      IF valLim = -1 THEN  (* No more '$' characters, we're done. *)
	EXIT;
      END;
      valStart := Text.FindChar(text, ':', keyStart);
      IF valStart = -1 OR valStart > valLim THEN  (* There is no old value. *)
	valStart := valLim;
      END;

(* At this point, we have a layout like:
|     $Keyword: Value$
|     ||      |      |
|     ||      |      valLim
|     ||      valStart
|     |keyStart
|     dollar
|
|     $Keyword$
|     ||      |
|     ||      |
|     ||      valStart, valLim
|     |keyStart
|     dollar
*)

      IF valStart >= keyStart + self.t.keyLenMin
      AND valStart <= keyStart + self.t.keyLenMax
      AND self.t.keyTab.get(Text.Sub(text, keyStart, valStart-keyStart),
	keyRecRef)
      AND NARROW(keyRecRef, KeyRec).enabled THEN
	(* We found a valid RCS keyword.  Expand it. *)
	newVal := GetValue(self,
	  keyword := NARROW(keyRecRef, KeyRec).keyword,
	  logPrefix := Text.Sub(text, 0, dollar));
	CASE self.mode OF
	| ExpandMode.Default, ExpandMode.KeyValue, ExpandMode.KeyValueLocker =>
	    newVal := ": " & newVal & " ";
	    text := Text.Sub(text, 0, valStart) & newVal
	      & Text.Sub(text, valLim);
	    valLim := valStart + Text.Length(newVal);
	| ExpandMode.Key =>
	    text := Text.Sub(text, 0, valStart) & Text.Sub(text, valLim);
	    valLim := valStart;
	| ExpandMode.Value =>
	    text := Text.Sub(text, 0, dollar) & newVal
	      & Text.Sub(text, valLim+1);
	    valLim := dollar + Text.Length(newVal) - 1;  (* No trailing "$" *)
	| ExpandMode.Old, ExpandMode.Binary =>
	    <* ASSERT FALSE *>
	END;
	changed := TRUE;
      ELSE
	valLim := dollar;
      END;
    END;
    IF changed THEN line := RCSString.FromText(text) END;
    RETURN TRUE;
  END Next;

PROCEDURE GetValue(self: Iterator;
                   keyword: Keyword;
                   logPrefix: TEXT): TEXT =
  VAR
    name: Pathname.T;
    val: TEXT;
  BEGIN
    CASE keyword OF
    | Keyword.Author => val := Escape(self.delta.author);
    | Keyword.CVSHeader =>
	name := self.name;
	IF self.hideAttic THEN
	  name := SupMisc.LiveName(name);
	END;
	val := Escape(name)
	  & " " & self.delta.revision
	  & " " & ExpandDate(self.delta.date)
	  & " " & Escape(self.delta.author);
	IF self.delta.state # NIL THEN
	  val := val & " " & Escape(self.delta.state);
	END;
	(* FIXME - locker. *)
    | Keyword.Date => val := ExpandDate(self.delta.date);
    | Keyword.Header =>
	val := Escape(SupMisc.ResolvePath(self.cvsRoot, self.name))
	  & " " & self.delta.revision
	  & " " & ExpandDate(self.delta.date)
	  & " " & Escape(self.delta.author);
	IF self.delta.state # NIL THEN
	  val := val & " " & Escape(self.delta.state);
	END;
	(* FIXME - locker. *)
    | Keyword.Id =>
	val := Escape(SupMisc.PathLast(self.name))
	  & " " & self.delta.revision
	  & " " & ExpandDate(self.delta.date)
	  & " " & Escape(self.delta.author);
	IF self.delta.state # NIL THEN
	  val := val & " " & Escape(self.delta.state);
	END;
	(* FIXME - locker. *)
    | Keyword.Locker => val := "";  (* FIXME *)
    | Keyword.Log =>
	val := Escape(SupMisc.PathLast(self.name));

	VAR
	  logText: REF ARRAY OF TEXT;
	  starPos: INTEGER;
	  ch: CHAR;
	  logIter: RCSString.Iterator;
	  ok: BOOLEAN;
	  numNonEmpty: CARDINAL;
	  wsPos, wsLim: CARDINAL;
	  logLine: RCSString.T;
	BEGIN
	  (* Massage out '/' or '(' before a '*'. *)
	  starPos := Text.FindChar(logPrefix, '*');
	  IF starPos > 0 THEN
	    ch := Text.GetChar(logPrefix, starPos-1);
	    IF ch = '/' OR ch = '(' THEN
	      logPrefix := Text.Sub(logPrefix, 0, starPos-1)
		& " " & Text.Sub(logPrefix, starPos);
	    END;
	  END;

	  logText := NEW(REF ARRAY OF TEXT, self.delta.log.numLines());
	  logIter := self.delta.log.iterate();

	  (* Unpack the log text, and note trailing blank lines. *)
	  numNonEmpty := 0;
	  FOR i := FIRST(logText^) TO LAST(logText^) DO
	    ok := logIter.next(logLine);
	    <* ASSERT ok *>
	    logText[i] := logLine.toText();
	    IF NOT SupMisc.IsBlankLine(logText[i]) THEN
	      numNonEmpty := i+1;
	    END;
	  END;

	  (* Remove the trailing blank lines, and add our own leading
	     and trailing lines. *)
	  WITH newLogText = NEW(REF ARRAY OF TEXT, numNonEmpty+2) DO
	    newLogText[0] := "Revision"
	      & " " & self.delta.revision
	      & "  " & ExpandDate(self.delta.date)
	      & "  " & self.delta.author
	      & "\n";
	    SUBARRAY(newLogText^, 1, numNonEmpty) :=
	      SUBARRAY(logText^, 0, numNonEmpty);
	    newLogText[numNonEmpty+1] := "\n";
	    logText := newLogText;
	  END;

	  (* Tack the log prefix onto each line, and remove trailing
	     white space. *)
	  FOR i := FIRST(logText^) TO LAST(logText^) DO
	    logText[i] := logPrefix & logText[i];
	    wsLim := Text.Length(logText[i]);
	    IF wsLim > 0 AND Text.GetChar(logText[i], wsLim-1) = '\n'
	    THEN
	      DEC(wsLim);
	      IF wsLim > 0 AND Text.GetChar(logText[i], wsLim-1) = '\r'
	      THEN
		DEC(wsLim);
	      END;
	    END;
	    wsPos := wsLim;
	    WHILE wsPos > 0 AND Text.GetChar(logText[i], wsPos-1) IN
	      SET OF CHAR{' ', '\t'}
	    DO
	      DEC(wsPos);
	    END;
	    IF wsPos # wsLim THEN  (* There is some trailing white space. *)
	      logText[i] := Text.Sub(logText[i], 0, wsPos) &
		Text.Sub(logText[i], wsLim);
	    END;
	  END;

	  self.logText := logText;
	END;
    | Keyword.Name =>
	IF self.tag # NIL THEN val := Escape(self.tag) ELSE val := "" END;
    | Keyword.RCSfile => val := Escape(SupMisc.PathLast(self.name));
    | Keyword.Revision => val := self.delta.revision;
    | Keyword.Source =>
	val := Escape(SupMisc.ResolvePath(self.cvsRoot, self.name));
    | Keyword.State =>
	IF self.delta.state # NIL THEN
	  val := Escape(self.delta.state);
	ELSE
	  val := "";
	END;
    END;

    RETURN val;
  END GetValue;

PROCEDURE Escape(t: TEXT): TEXT =
  VAR
    len := Text.Length(t);
    start: CARDINAL := 0;
    r := "";
  BEGIN
    FOR pos := 0 TO len-1 DO
      WITH ch = Text.GetChar(t, pos) DO
	CASE ch OF
	| '\t' =>
	    r := r & Text.Sub(t, start, pos-start) & "\\t";
	    start := pos + 1;
	| '\n' =>
	    r := r & Text.Sub(t, start, pos-start) & "\\n";
	    start := pos + 1;
	| ' ' =>
	    r := r & Text.Sub(t, start, pos-start) & "\\040";
	    start := pos + 1;
	| '$' =>
	    r := r & Text.Sub(t, start, pos-start) & "\\044";
	    start := pos + 1;
	| '\\' =>
	    r := r & Text.Sub(t, start, pos-start) & "\\\\";
	    start := pos + 1;
	ELSE (* Ignore *) END;
      END;
    END;
    IF start = 0 THEN  (* No substitutions. *)
      r := t;
    ELSIF start < len THEN  (* Copy last chunk. *)
      r := r & Text.Sub(t, start);
    END;
    RETURN r;
  END Escape;

PROCEDURE ExpandDate(date: RCSDate.T): TEXT =
  VAR
    t: TEXT;
  BEGIN
    WITH yrLen = Text.FindChar(date, '.') DO
      IF yrLen = 2 THEN
	date := "19" & date;
      ELSIF yrLen # 4 THEN
	RAISE Fatal("Invalid date \"" & date & "\"");
      END;
    END;

    IF Text.Length(date) # 19
    OR Text.GetChar(date, 7) # '.'
    OR Text.GetChar(date, 10) # '.'
    OR Text.GetChar(date, 13) # '.'
    OR Text.GetChar(date, 16) # '.' THEN
      RAISE Fatal("Invalid date \"" & date & "\"");
    END;

    t := Text.Sub(date, 0, 4)
      & "/" & Text.Sub(date, 5, 2)
      & "/" & Text.Sub(date, 8, 2)
      & " " & Text.Sub(date, 11, 2)
      & ":" & Text.Sub(date, 14, 2)
      & ":" & Text.Sub(date, 17, 2);

    RETURN t;
  END ExpandDate;

PROCEDURE EncodeExpand(x: ExpandMode): TEXT =
  BEGIN
    RETURN encodeTab[x];
  END EncodeExpand;

PROCEDURE DecodeExpand(t: TEXT): ExpandMode
  RAISES {RCSError.E} =
  VAR
    expOrd: INTEGER;
  BEGIN
    IF decodeTab.get(t, expOrd) THEN
      RETURN VAL(expOrd, ExpandMode);
    ELSE
      RAISE RCSError.E("Invalid expansion mode \"" & t & "\"");
    END;
  END DecodeExpand;

BEGIN
  FOR i := FIRST(ExpandInit) TO LAST(ExpandInit) DO
    EVAL decodeTab.put(ExpandInit[i].name, ExpandInit[i].value);
    encodeTab[VAL(ExpandInit[i].value, ExpandMode)] := ExpandInit[i].name;
  END;
END RCSKeyword.
