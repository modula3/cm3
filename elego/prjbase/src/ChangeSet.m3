(*---------------------------------------------------------------------------*)
MODULE ChangeSet;

IMPORT Text, TextSeq, TextTextTbl, Time, Rd, Wr, FileRd, FileWr, 
       TextWr, Thread, OSError, MxConfig;
IMPORT RCS_Date, TextUtils, TextReadingUtils;

(*---------------------------------------------------------------------------*)
REVEAL
  T = Public BRANDED "ChangeSet 0.0" OBJECT
    name  : TEXT;
    desc  : TEXT;
    user  : TEXT;
    date  : Time.T;
    pre   : TextTextTbl.T;
    post  : TextTextTbl.T;
  METHODS
  OVERRIDES
    init := Init;
    getName := GetName;
    getDate := GetDate;
    getUser := GetUser;
    description := Description;
    packages := Packages;
    preState := PreState;
    postState := PostState;
    setName := SetName;
    setDate := SetDate;
    setUser := SetUser;
    setDescription := SetDescription;
    add := Add;
    getPreTag := GetPreTag;
    getPostTag := GetPostTag;
    parse := Parse;
    read := Read;
    write := Write;
    save := Save;
    toText := ToText;
    logText := LogText;
  END;

(*---------------------------------------------------------------------------*)
PROCEDURE Init(self : T; name : TEXT) : T =
  BEGIN
    self.name := name;
    self.desc := "";
    self.user := "unknown";
    self.date := Time.Now();
    self.pre := NEW(TextTextTbl.Default).init();
    self.post := NEW(TextTextTbl.Default).init();
    RETURN self;
  END Init;

(*---------------------------------------------------------------------------*)
PROCEDURE GetName(self : T) : TEXT =
  BEGIN
    RETURN self.name;
  END GetName;

(*---------------------------------------------------------------------------*)
PROCEDURE GetDate(self : T) : Time.T =
  BEGIN
    RETURN self.date;
  END GetDate; 

(*---------------------------------------------------------------------------*)
PROCEDURE GetUser(self : T) : TEXT =
  BEGIN
    RETURN self.user;
  END GetUser; 

(*---------------------------------------------------------------------------*)
PROCEDURE Description(self : T) : TEXT =
  BEGIN
    RETURN self.desc;
  END Description;

(*---------------------------------------------------------------------------*)
PROCEDURE Packages(self : T) : TextSeq.T =
  VAR 
    iter := self.pre.iterate();
    res  := NEW(TextSeq.T).init(self.pre.size());
    pkg, tag  :  TEXT;
  BEGIN
    WHILE iter.next(pkg, tag) DO
      res.addhi(pkg);
    END;
    RETURN res;
  END Packages;

(*---------------------------------------------------------------------------*)
PROCEDURE PreState(self : T) : TextTextTbl.T =
  BEGIN
    RETURN self.pre;
  END PreState;

(*---------------------------------------------------------------------------*)
PROCEDURE PostState(self : T) : TextTextTbl.T =
  BEGIN
    RETURN self.post;
  END PostState;

(*---------------------------------------------------------------------------*)
PROCEDURE SetName(self : T; name : TEXT) =
  BEGIN
    self.name := name;
  END SetName;

(*---------------------------------------------------------------------------*)
PROCEDURE SetDate(self : T; date : Time.T) =
  BEGIN
    self.date := date;
  END SetDate; 

(*---------------------------------------------------------------------------*)
PROCEDURE SetUser(self : T; user : TEXT) =
  BEGIN
    self.user := user;
  END SetUser; 

(*---------------------------------------------------------------------------*)
PROCEDURE SetDescription(self : T; desc : TEXT) =
  BEGIN
    self.desc := desc;
  END SetDescription;

(*---------------------------------------------------------------------------*)
PROCEDURE Add(self : T; pkg, pre, post: TEXT) =
  BEGIN
    EVAL self.pre.put(pkg, pre);
    EVAL self.post.put(pkg, post);
  END Add;

(*---------------------------------------------------------------------------*)
PROCEDURE GetPreTag(self : T; pkg : TEXT) : TEXT =
  VAR tag : TEXT;
  BEGIN
    IF self.pre.get(pkg, tag) THEN
      RETURN tag;
    ELSE
      RETURN NIL;
    END;
  END GetPreTag;

(*---------------------------------------------------------------------------*)
PROCEDURE GetPostTag(self : T; pkg : TEXT) : TEXT =
  VAR tag : TEXT;
  BEGIN
    IF self.post.get(pkg, tag) THEN
      RETURN tag;
    ELSE
      RETURN NIL;
    END;
  END GetPostTag;

(*---------------------------------------------------------------------------*)
PROCEDURE Read(self : T; fn : TEXT) RAISES {Error} =
  VAR
    rd : Rd.T;
  BEGIN
    TRY
      rd := FileRd.Open(fn);
      TRY
        Parse(self, rd, fn);
      FINALLY
        Rd.Close(rd);
      END;
    EXCEPT
      OSError.E => RAISE Error("Cannot open file " & fn);
    | Rd.Failure => RAISE Error("Error reading file " & fn);
    | Thread.Alerted => RAISE Error("Interrupted reading file " & fn);
    END;
  END Read;

(*---------------------------------------------------------------------------*)
PROCEDURE Parse(self : T; rd : Rd.T; fn : TEXT) RAISES {Error} =
  VAR
    key, date, pkg, tag1, tag2 : TEXT;

  PROCEDURE ReadSet() RAISES {Rd.Failure, Thread.Alerted, Rd.EndOfFile} = 
    BEGIN
      WHILE NOT Rd.EOF(rd) DO
        key := TextReadingUtils.GetTokenOrString(rd);
        IF Text.Equal(key, "end") THEN
          RETURN;
        END;
        pkg := key;
        tag1 := TextReadingUtils.GetTokenOrString(rd);
        tag2 := TextReadingUtils.GetTokenOrString(rd);
        self.add(pkg, tag1, tag2);
      END;
    END ReadSet;

  PROCEDURE ReadDesc() RAISES {Rd.Failure, Thread.Alerted, Rd.EndOfFile} =
    VAR 
      lines := NEW(TextSeq.T).init();
      cont := TRUE;
    BEGIN
      EVAL Rd.GetLine(rd);
      WHILE NOT Rd.EOF(rd) AND cont DO
        key := Rd.GetLine(rd);
        IF Text.Equal(key, "end") THEN
          cont := FALSE;
        ELSE
          lines.addhi(key);
        END;
      END;
      self.desc := TextUtils.TextSeqToText(lines, sep := NL);
    END ReadDesc;

  BEGIN
    TRY
      WHILE NOT Rd.EOF(rd) DO
        key := TextReadingUtils.GetTokenOrString(rd);
        IF Text.Equal(key, "changeset") THEN
          self.name := TextReadingUtils.GetTokenOrString(rd);
        ELSIF Text.Equal(key, "changes") THEN
          ReadSet();
        ELSIF Text.Equal(key, "description") THEN
          ReadDesc();
        ELSIF Text.Equal(key, "creator") THEN
          self.user := TextReadingUtils.GetTokenOrString(rd);
        ELSIF Text.Equal(key, "date") THEN
          date := TextReadingUtils.GetTokenOrString(rd);
          self.date := RCS_Date.ToTimeApprox(date);
        ELSE
          RAISE Error("unknown keyword in changeset file " & fn & ": " & key);
        END;
      END;
    EXCEPT
    | Thread.Alerted => RAISE Error("Interrupted reading file " & fn);
    | Rd.EndOfFile => (* skip *)
    | Rd.Failure => RAISE Error("Error reading file " & fn);
    END;
  END Parse; 

(*---------------------------------------------------------------------------*)
PROCEDURE Write(self : T; wr : Wr.T; fn : TEXT) RAISES {Error} =
  CONST 
    SDQ = " \"";
    DQSDQ = "\" \"";
  VAR
    DQNL := "\"" & NL;
    iter := self.pre.iterate();
    pkg, tag1, tag2 : TEXT;
  BEGIN
    TRY
      Wr.PutText(wr, "changeset" & SDQ & self.name & DQNL);
      Wr.PutText(wr, "creator" & SDQ & self.user & DQNL);
      Wr.PutText(wr, "date" & SDQ & RCS_Date.FromTime(self.date) & DQNL);
      Wr.PutText(wr, "changes" & NL);
      WHILE iter.next(pkg, tag1) DO
        EVAL self.post.get(pkg, tag2);
        Wr.PutText(wr, "  " & pkg & SDQ & tag1 & DQSDQ & tag2 & DQNL);
      END;
      Wr.PutText(wr, "end" & NL);
      Wr.PutText(wr, "description" & NL);
      Wr.PutText(wr, self.desc);
      Wr.PutText(wr, NL & "end" & NL);
      Wr.Flush(wr);
    EXCEPT
    | Wr.Failure =>  RAISE Error("Error writing file " & fn);
    | Thread.Alerted =>  RAISE Error("Interrupted writing file " & fn);
    END;
  END Write;

(*---------------------------------------------------------------------------*)
PROCEDURE Save(self : T; fn : TEXT) RAISES {Error} =
  VAR
    wr : Wr.T;
  BEGIN
    TRY
      wr := FileWr.Open(fn);
      TRY
        Write(self, wr, fn);
      FINALLY
        Wr.Close(wr);
      END;
    EXCEPT
      OSError.E => RAISE Error("Cannot open file " & fn & " for writing");
    | Wr.Failure =>  RAISE Error("Error writing file " & fn);
    | Thread.Alerted =>  RAISE Error("Interrupted writing file " & fn);
    END;
  END Save;

(*---------------------------------------------------------------------------*)
PROCEDURE ToText(self : T) : TEXT =
  VAR 
    res := TextWr.New();
    again := TRUE;
  BEGIN
    WHILE again DO
      TRY
        Write(self, res, "<in-memory file>");
        again := FALSE;
      EXCEPT
        Error => again := TRUE;
      END;
    END;
    RETURN TextWr.ToText(res);
  END ToText;

(*---------------------------------------------------------------------------*)
PROCEDURE LogText(self : T) : TEXT =
  VAR res : TEXT;
  BEGIN
    res := "change set " & self.name & NL &
           "created by " & self.user & " at " & 
           RCS_Date.FromTime(self.date) & NL &
           "description:" & NL &
           self.desc & NL &
           "package changes:" & NL;
    WITH pkgs = self.packages() DO
      FOR j := 0 TO pkgs.size() - 1 DO
        WITH pkg = pkgs.get(j) DO
          WITH pre = self.getPreTag(pkg), post = self.getPostTag(pkg) DO
            res := res & "  " & pkg & "\t" & pre & "\t" & post & NL;
          END;
        END
      END;
    END;
    RETURN res;
  END LogText;

VAR NL := "\n";
BEGIN
  IF MxConfig.HOST_OS_TYPE() = MxConfig.OS_TYPE.WIN32 THEN
    NL := "\r\n";
  END;
END ChangeSet.
