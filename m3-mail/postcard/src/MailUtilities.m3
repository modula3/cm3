(* Copyright 1989 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* Last modified on Wed Feb  1 12:18:17 PST 1995 by kalsow     *)
(*      modified on Tue Jul 27 17:31:23 PDT 1993 by birrell    *)
(*      modified on Fri Jun 14 11:31:44 PDT 1991 by chan       *)
(*      modified on Thu Mar 14 10:21:20 PST 1991 by mbrown     *)
(*      modified on Wed May 31 17:00:19 PDT 1989 by discolo    *)

MODULE MailUtilities;

IMPORT Fmt;
IMPORT ASCII, Atom, MiscUtils, OSUtils, Rd, RefList,
  TextRefTbl, Sx, Text, TextList, TextRd, TextWr, Thread,
  Time, Wr;

<*FATAL Wr.Failure*>
<*FATAL Rd.Failure*>
<*FATAL Rd.EndOfFile*>
<*FATAL Thread.Alerted*>

(**************)
(* WhiteSpace *)
(**************)

PROCEDURE LTrim(s: Text.T; whiteSpace: SET OF CHAR := DefaultWS): Text.T =
  VAR
    cnt := 0; (*count of the number of leading whitespace characters*)
    len := Text.Length(s);
  BEGIN
    cnt := 0;
    WHILE (cnt<len) AND (Text.GetChar(s, cnt) IN whiteSpace) DO INC(cnt) END;
    IF cnt = 0 THEN RETURN s ELSE RETURN Text.Sub(s, cnt, LAST(CARDINAL)) END
  END LTrim;


(*********)
(* Lists *)
(*********)

PROCEDURE ConcatText(text1, text2: Text.T; separator: Text.T := "";
                     whiteSpace: SET OF CHAR := DefaultWS): Text.T RAISES {} =
  (* If text1 and text2 both contain non-whitespace characters, returns
     Text.Cat(text1, separator, text2); otherwise, returns Text.Cat(text1,
     text2). *)
  PROCEDURE PureWhite(t: TEXT): BOOLEAN =
    VAR rd: Rd.T;
    BEGIN
      rd := TextRd.New(t);
      WHILE NOT Rd.EOF(rd)
      DO IF NOT Rd.GetChar(rd) IN whiteSpace THEN RETURN FALSE END END;
      RETURN TRUE
    END PureWhite;
  BEGIN
    IF NOT PureWhite(text1) AND NOT PureWhite(text2) THEN
      RETURN text1 & separator & text2;
    END;
    RETURN text1 & text2;
  END ConcatText;


(************)
(* CaseFold *)
(************)

PROCEDURE ToLower(s: Text.T): Text.T =
  VAR rd: Rd.T; wr: Wr.T;
  BEGIN
    IF Text.Empty(s) THEN RETURN s;  END;
    rd := TextRd.New(s);
    wr := TextWr.New();
    WHILE  NOT Rd.EOF(rd) DO
      Wr.PutChar(wr, ASCII.Lower[Rd.GetChar(rd)]);
    END;
    RETURN TextWr.ToText(wr);
  END ToLower;


(************)
(* Messages *)
(************)

PROCEDURE NewMessage(to, cc, fcc, subject, inReplyTo,
                     body: Text.T): Text.T RAISES {} =
  VAR wr: Wr.T;
  BEGIN
    wr := TextWr.New();
    Wr.PutText(wr, "To: " & to & "\nCc: " & cc);
    IF NOT Text.Empty(fcc) THEN Wr.PutText(wr, "\nFcc: " & fcc);  END;
    Wr.PutText(wr, "\nSubject: " & subject);
    IF NOT Text.Empty(inReplyTo) THEN
      Wr.PutText(wr, "\nIn-Reply-To: " & inReplyTo);
    END;
    Wr.PutText(wr, "\n------\n\n");
    IF NOT Text.Empty(body) THEN
      Wr.PutText(wr, body);
      IF Text.GetChar(body, Text.Length(body)-1) # '\n' THEN
        Wr.PutChar(wr, '\n');
      END;
    END;
    RETURN TextWr.ToText(wr);
  END NewMessage;

PROCEDURE GetHeader(rd: Rd.T): TEXT RAISES { Rd.Failure } =
  VAR wr := TextWr.New(); c: CHAR; startOfLine := TRUE;
  BEGIN
    TRY
      LOOP
        c := Rd.GetChar(rd);
        IF startOfLine AND ( (c = '\n') OR (c = '-') ) THEN
          Rd.UnGetChar(rd); EXIT
        END;
        startOfLine := (c = '\n');
        Wr.PutChar(wr, c);
      END;
    EXCEPT Rd.EndOfFile =>
    END;
    RETURN TextWr.ToText(wr);
  END GetHeader;

PROCEDURE FilterHeader(header: TEXT; fields: TextList.T): TEXT =
  VAR
    nameS, nameE, valueS, valueE, nth: INTEGER;
    separator: CHAR;
    wr := TextWr.New();
    l: TextList.T;
  BEGIN
    l := fields;
    WHILE l # NIL DO
      nth := 1;
      WHILE GetFieldIndices(header, l.head, nameS, nameE, valueS, valueE,
                            separator, nth) DO
        Wr.PutText(wr, l.head);
        Wr.PutChar(wr, separator);
        Wr.PutText(wr, Text.Sub(header, valueS, valueE - valueS));
        Wr.PutChar(wr, '\n');
        INC(nth);
      END;
      l := l.tail;
    END;
    RETURN TextWr.ToText(wr);
  END FilterHeader;

PROCEDURE GetFieldValue(header: TEXT; fieldname: TEXT; nth: INTEGER := 1): TEXT =
  VAR nameS, nameE, valueS, valueE: INTEGER; separator: CHAR;
  BEGIN
    IF GetFieldIndices(header, fieldname, nameS, nameE, valueS, valueE,
                       separator, nth) THEN
      RETURN Text.Sub(header, valueS, valueE - valueS);
    ELSE
      RETURN ""
    END;
  END GetFieldValue;

PROCEDURE GetAllFieldValues(header: Text.T; fieldname: Text.T;
                            separator: Text.T := " "): Text.T RAISES {} =
  VAR
    nameS, nameE, valueS, valueE, nth: INTEGER;
    dummy: CHAR;
    result: Text.T;
  BEGIN
    nth := 1;
    result := "";
    LOOP
      IF NOT GetFieldIndices(header, fieldname, nameS, nameE, valueS, valueE,
                             dummy, nth) THEN
        EXIT;
      END;
      result := ConcatText(result,
                          Text.Sub(header, valueS, valueE - valueS), separator);
      INC(nth);
    END;
    RETURN result;
  END GetAllFieldValues;

PROCEDURE GetFieldIndices(header: Text.T; fieldname: Text.T;
                          VAR (*OUT*) nameS, nameE, valueS, valueE: INTEGER;
                          VAR (*OUT*) separator: CHAR;
                          nth: INTEGER): BOOLEAN =
  VAR
    pos, found: INTEGER;
    nlHeader := "\n" & header;        (* newline makes "Find" easier to use *)
    nlFieldName := "\n" & fieldname;
    headerLength := Text.Length(header);
    nameLength := Text.Length(fieldname);
    c: CHAR;
  BEGIN
    pos := 0; found := 0;
    LOOP
      (* pos is index of next char in nlHeader *)
      (* Note that pos might be beyond end of header; then Find returns -1 *)
      pos := MiscUtils.Find(nlHeader, pos, nlFieldName, TRUE);
      IF pos < 0 THEN RETURN FALSE END;
      nameS := pos; nameE := nameS + nameLength;
      INC(pos, nameLength+1); (* allow for prefixed NL in nlFieldName *)
      IF pos >= headerLength+1 (* prefixed NL *) THEN
        INC(found) (* fieldName with no value at end of header *)
      ELSE
        c := Text.GetChar(nlHeader, pos);
        IF c IN SET OF CHAR {':', ' ', '\t'} THEN
          INC(pos);
          INC(found); (* fieldName followed by ':' or whitespace *)
          separator := c;
        (* otherwise, fieldname was followed by other stuff, so it didn't match *)
        END;
      END;
      IF found = nth THEN EXIT END;
    END;
    DEC(pos); (* convert from index in nlHeader to index in header *)
    valueS := pos;
    LOOP
      (* pos is index of next char in header *)
      (* scan forward until newline not followed by whitespace *)
      IF pos >= headerLength THEN EXIT END;
      IF Text.GetChar(header, pos) = '\n' THEN
        IF pos+1 >= headerLength THEN EXIT END;
        IF NOT (Text.GetChar(header, pos+1) IN SET OF CHAR {' ','\t'}) THEN
          EXIT
        END;
      END;
      INC(pos);
    END;
    valueE := pos;
    RETURN TRUE;
  END GetFieldIndices;


(***********)
(* Aliases *)
(***********)

PROCEDURE SetAlias(alias: Text.T; expansion: REFANY) RAISES {Error} =
    (* Create an alias-to-expansion mapping.  Recursively defined aliases will
       be handled in GetAliasInternal(). *)
  VAR ra: REFANY;
  BEGIN
    alias := ToLower(alias);
    LOCK aliasLock DO
      IF expansion = NIL THEN
        IF aliases.delete(alias, ra) THEN END;
      ELSE
        TYPECASE expansion OF
          | Text.T, RefList.T =>
              IF aliases.put(alias, expansion) THEN END;
        ELSE
          RaiseError(alias, ": Syntax error in alias expression.");
        END;
      END;
    END;
  END SetAlias;

PROCEDURE GetAliasInternal(alias: Text.T;
                           alreadyExpanded: RefList.T := NIL): RefList.T
                           RAISES {Error} =
    (* LL = aliasLock.  Returns NIL if 'alias' does not have an expansion. *)
  VAR ra: REFANY; t: Text.T; aliaslist, l, expansion: RefList.T;
  BEGIN
    alias := ToLower(alias);
    IF  NOT aliases.get(alias, ra) THEN RETURN NIL END;
          (* Check for recursively defined aliases. *)
    IF RefList.Member(alreadyExpanded, alias) THEN
      RaiseError(alias, ": Alias recursively defined");
    END;
    alreadyExpanded := RefList.Cons(alias, alreadyExpanded);
    TYPECASE ra OF
      | Text.T(z_0) =>
          t := z_0;
          expansion := GetAliasInternal(t, alreadyExpanded);
          IF expansion = NIL THEN expansion := RefList.List1(t);  END;
          RETURN expansion;
      | RefList.T(z_1) =>
          l := z_1;
          aliaslist := NIL;
          WHILE l # NIL DO
            t := NARROW(l.head, Text.T);
            expansion := GetAliasInternal(t, alreadyExpanded);
            IF expansion = NIL THEN
              aliaslist := RefList.AppendD(aliaslist, RefList.Cons(t, NIL));
            ELSE
              aliaslist := RefList.AppendD(aliaslist, expansion);
            END;
            l := l.tail;
          END;
          RETURN aliaslist;
    ELSE
      RETURN NIL
    END;
  END GetAliasInternal;

PROCEDURE GetAlias(alias: Text.T): Text.T RAISES {Error} =
  VAR l: RefList.T;  result: Text.T := "";
  BEGIN
    LOCK aliasLock DO l := GetAliasInternal(alias);  END;
    WHILE l # NIL DO
      IF NOT Text.Equal (result, "") THEN result := result & ", ";  END;
      result := result & NARROW(l.head, Text.T);
      l := l.tail;
    END;
    RETURN result;
  END GetAlias;

PROCEDURE ExpandField(header: Text.T; fieldname: Text.T): Text.T
                      RAISES {Error} =
  VAR
    alias, expansion, fieldValue: Text.T;
    nameS, nameE, valueS, valueE: INTEGER;
    dummy: CHAR;
    count, elementBodyLen: INTEGER;
    rd: Rd.T; mainWr, elementWr: Wr.T; c: CHAR;
    found: BOOLEAN;
  CONST
    Sep = ',';
  (* In Pat Chan's version, this used TextList.  Straight in-line code seems
     to me to be a lot simpler than converting TextList. ADB *)
  BEGIN
    count := 1;
    LOOP
      found := FALSE;
      IF NOT GetFieldIndices(header, fieldname, nameS, nameE, valueS, valueE,
                             dummy, count) THEN
        EXIT;
      END;
      fieldValue := Text.Sub(header, valueS, valueE - valueS);
      rd := TextRd.New(fieldValue);
      mainWr := TextWr.New();
      WHILE NOT Rd.EOF(rd) DO (* for each element *)
        (* skip whitespace and sep *)
        REPEAT
          c := Rd.GetChar(rd);
          IF (c IN DefaultWS) OR (c = Sep) THEN
            Wr.PutChar(mainWr, c)
          ELSE
            Rd.UnGetChar(rd);
            EXIT
          END;
        UNTIL Rd.EOF(rd);
        IF NOT Rd.EOF(rd) THEN
          (* process the element starting with the next char *)
          elementWr := TextWr.New();
          elementBodyLen := 0;
          REPEAT
            c := Rd.GetChar(rd);
            IF c = Sep THEN
              Rd.UnGetChar(rd);
              EXIT
            ELSE
              Wr.PutChar(elementWr, c);
              IF NOT (c IN DefaultWS) THEN
                (* update number of chars before trailing whitespace *)
                elementBodyLen := Wr.Index(elementWr);
              END;
            END;
          UNTIL Rd.EOF(rd);
          alias := TextWr.ToText(elementWr);
          expansion := GetAlias(Text.Sub(alias, 0, elementBodyLen));
          IF Text.Empty(expansion) THEN
            (* No match; write original element, with trailing whitespace *)
            Wr.PutText(mainWr, alias)
          ELSE
            (* Expanded; write expansion, then original trailing whitespace *)
            Wr.PutText(mainWr, expansion);
            Wr.PutText(mainWr, Text.Sub(alias, elementBodyLen, LAST(CARDINAL)));
            found := TRUE;
          END;
        END;
      END;
      IF found THEN
        header:= MiscUtils.Replace(header, valueS, valueE - valueS,
                              TextWr.ToText(mainWr));
      END;
      INC(count);
    END;
    RETURN header;
  END ExpandField;

PROCEDURE ExpandAliases(message: Text.T): Text.T RAISES {Error} =
  VAR header, oldHeader: Text.T; rd := TextRd.New(message);
  BEGIN
    IF NeedToReadAliasFile() THEN ReadAliasFile();  END;
    header := GetHeader(rd);
    oldHeader := header;
    header := ExpandField(header, "to");
    header := ExpandField(header, "cc");
    header := ExpandField(header, "bcc");
    IF oldHeader = header THEN
      RETURN message
    ELSE
      RETURN header & Rd.GetText(rd, LAST(CARDINAL));
    END
  END ExpandAliases;


(*************************)
(* Process MailAliasFile *)
(*************************)

PROCEDURE Eval(l: RefList.T) RAISES {Error} =
  VAR args: INTEGER; name: Text.T; pair: RefList.T;
  BEGIN
    name := Atom.ToText(l.head);
    args := RefList.Length(l) - 1;
    l := l.tail;
    IF Text.Equal(name, "Aliases") THEN
      WHILE args > 0 DO
        pair := NARROW(l.head, RefList.T);
        IF (pair = NIL) OR (pair.tail = NIL) THEN
          RaiseError("Sx.ReadError in Aliases expression.");
        END;
        TYPECASE pair.head OF
          | Text.T =>
              SetAlias(NARROW(pair.head, Text.T), pair.tail.head);
        ELSE
          RaiseError("Unexpected type of value in Aliases expression.");
        END;
        l := l.tail;
        DEC(args);
      END;
    END;
  END Eval;

PROCEDURE SetAliasFile(filename: Text.T) RAISES {} =
  BEGIN
    LOCK aliasLock DO aliasFilename := filename;  END;
    ForceAliasFileRead();
  END SetAliasFile;

PROCEDURE GetAliasFile(): Text.T RAISES {} =
  BEGIN
    LOCK aliasLock DO RETURN aliasFilename;  END;
  END GetAliasFile;

PROCEDURE NeedToReadAliasFile(): BOOLEAN RAISES {} =
  VAR time: Time.T;
  BEGIN
    IF Text.Empty(aliasFilename) THEN RETURN FALSE END;
    TRY
      time := aliasFileMTime;
      EVAL OSUtils.GetInfo(aliasFilename, aliasFileMTime);
      RETURN aliasFileMTime > time;
    EXCEPT
      | OSUtils.FileNotFound => RETURN FALSE;
      | OSUtils.FileError => RETURN TRUE;
    END;
  END NeedToReadAliasFile;

PROCEDURE ReadAliasFile() RAISES {Error} =
  VAR rd: Rd.T; sx: REFANY; wr: Wr.T;
  BEGIN
    wr := TextWr.New();
    TRY
      LOCK aliasLock DO
        IF Text.Empty(aliasFilename) THEN RETURN ;  END;
        EVAL OSUtils.GetInfo(aliasFilename, aliasFileMTime);
        rd := OSUtils.OpenRead(aliasFilename);
      END;
      ClearAliases();
      LOOP
        sx := Sx.Read(rd);
        IF ISTYPE(sx, RefList.T) THEN
          Eval(NARROW(sx, RefList.T))
        ELSE
          Wr.PutText(wr, aliasFilename & " (near character "
                         & Fmt.Int(Rd.Index(rd))
                         & "): syntax Sx.ReadError.");
          RaiseError(TextWr.ToText(wr));
        END;
      END;
    EXCEPT
      | Sx.ReadError(sxEC) =>
          Wr.PutText(wr, aliasFilename & " (near character "
                         & Fmt.Int(Rd.Index(rd)) & "): " & sxEC);
          RaiseError(TextWr.ToText(wr));
      | OSUtils.FileNotFound => RaiseError(aliasFilename & ": not found");
      | OSUtils.FileError(fileEC) =>
            Wr.PutText(wr, aliasFilename & ": " & fileEC);
            RaiseError(TextWr.ToText(wr));
      | Rd.EndOfFile => 
    END;
    Rd.Close(rd);
  END ReadAliasFile;

PROCEDURE ClearAliases() RAISES {} =
  BEGIN
    LOCK aliasLock DO EVAL aliases.init();  END;
  END ClearAliases;

PROCEDURE ForceAliasFileRead() RAISES {} =
  BEGIN
    LOCK aliasLock DO
      aliasFileMTime := 0.0d0;
    END;
  END ForceAliasFileRead;


(********)
(* Main *)
(********)

PROCEDURE RaiseError(arg1, arg2, arg3, arg4, arg5,
                      arg6, arg7, arg8, arg9, arg10: Text.T := "")
   RAISES {Error} =
  BEGIN
    RAISE Error(arg1 & arg2 & arg3 & arg4 & arg5 &
                arg6 & arg7 & arg8 & arg9 & arg10);
  END RaiseError;

VAR
  aliasLock := NEW(MUTEX); (* protects the global alias variables. *)
  aliases: TextRefTbl.Default;
  aliasFilename: Text.T;
  aliasFileMTime: Time.T;


BEGIN
  aliases := NEW(TextRefTbl.Default).init();
  aliasFilename := "";
  aliasFileMTime := 0.0d0;
END MailUtilities.


(* Created on Tue Dec  1 17:51:06 1987 by chan *)
