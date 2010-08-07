(*---------------------------------------------------------------------------*)
MODULE ConfigRsrcService;

IMPORT Rd, Thread, TextSeq, TextTextTbl, TextTextSeqTbl, Pathname,
       Rsrc, OSError, FileRd, TextRd, Text, ASCII;
IMPORT RsrcService, FSUtils, TextUtils, TextReadingUtils, ProcessEnv,
       MsgIF, MsgX;

(*---------------------------------------------------------------------------*)
REVEAL
  T = Public BRANDED "ComPactRsrcService 0.1" OBJECT
    tab   : TextTextSeqTbl.T;
    rpath : Rsrc.Path;
    msg   : MsgIF.T;
  METHODS
  OVERRIDES
    init := Init;
    readConfig := ReadConfig;
    setRsrcPath := SetRsrcPath;
    getRsrcPath := GetRsrcPath;
    getRsrcReader := GetRsrcReader;
    getRsrcAsText := GetRsrcAsText;
  END;

(*---------------------------------------------------------------------------*)
PROCEDURE Init(self : T; internalRsrcs : Rsrc.Path := NIL;
               msgif : MsgIF.T := NIL) : T
  RAISES {} =
  BEGIN
    self.msg := msgif;
    self.rpath := internalRsrcs;
    self.tab := NEW(TextTextSeqTbl.Default).init();
    RETURN self;
  END Init;

(*---------------------------------------------------------------------------*)
PROCEDURE ReadConfig(self : T; fn : TEXT)
  RAISES {RsrcService.E} =

  PROCEDURE Read(rd : Rd.T) RAISES {RsrcService.E} =
    VAR
      line, name, value : TEXT;
      trd : TextRd.T;
      seq : TextSeq.T;
    BEGIN
      TRY
        WHILE NOT Rd.EOF(rd) DO
          line := TextUtils.Compress(Rd.GetLine(rd));
          MsgX.D(self.msg, " |" & line);
          IF line # NIL AND NOT Text.Empty(line) AND
             Text.GetChar(line, 0) # '#' AND Text.GetChar(line, 0) # ';' THEN
            trd := TextRd.New(line);
            WHILE NOT Rd.EOF(trd) DO
              name := TextReadingUtils.GetTokenOrString(trd);
              value := TextReadingUtils.GetTokenOrString(trd);
              seq := TextUtils.Tokenize(value, ASCII.Set{';'});
              EVAL self.tab.put(name, seq);
            END;
          END;
        END;
      EXCEPT
        Rd.Failure => RAISE RsrcService.E("read failure on config file");
      | Rd.EndOfFile => (* skip *)
      | Thread.Alerted => 
        RAISE RsrcService.E("interrupted reading config file");
      END;
      TRY
        Rd.Close(rd);
      EXCEPT ELSE END;
    END Read;

  VAR (* ReadConfig *)
    rd : Rd.T;
  BEGIN
    IF FSUtils.IsFile(fn) THEN
      MsgX.D(self.msg, "reading initialization file " & fn);
      TRY
        rd := FileRd.Open(fn);
        Read(rd);
      EXCEPT
        OSError.E => 
        RAISE RsrcService.E("cannot read initialization file: " & fn);
      END;
    ELSE
      TRY
        MsgX.D(self.msg, "initializing from resource " & fn);
        rd := TextRd.New(Rsrc.Get("compactrc", self.rpath));
        Read(rd);
      EXCEPT
        Rsrc.NotFound => RAISE RsrcService.E("resource not found: " & fn)
      | Rd.Failure => RAISE RsrcService.E("can't read resource: " & fn)
      | Thread.Alerted =>
        RAISE RsrcService.E("interrupted reading resource: " & fn)
      ELSE END;
    END;
  END ReadConfig;

(*---------------------------------------------------------------------------*)
PROCEDURE SetRsrcPath(self : T; name : TEXT; rp : TextSeq.T)
  RAISES {} =
  BEGIN
    MsgX.D2(self.msg, "SetRsrcPath", " name = " & name);
    EVAL self.tab.put(name, rp);
  END SetRsrcPath;

(*---------------------------------------------------------------------------*)
PROCEDURE GetRsrcPath(self : T; name : TEXT) : TextSeq.T
  RAISES {} =
  VAR rp : TextSeq.T;
  BEGIN
    MsgX.D2(self.msg, "GetRsrcPath", " name = " & name);
    IF self.tab.get(name, rp) THEN
      MsgX.D(self.msg, "  path found");
      RETURN rp;
    ELSE
      MsgX.D(self.msg, "  path not found");
      RETURN NIL;
    END;
  END GetRsrcPath;

(*---------------------------------------------------------------------------*)
PROCEDURE GetRsrcReader(self : T; name : TEXT; 
                        env : TextTextTbl.T := NIL) : Rd.T
  RAISES {RsrcService.E} =
  VAR
    loc : TEXT;
    rd  : Rd.T := NIL;
  BEGIN
    MsgX.D2(self.msg, "GetRsrcReader", " name = " & name);
    MsgX.D(self.msg, "  trying resource path for name " & name);
    loc := FindLocation(self, name, name, env);
    IF loc = NIL THEN
      MsgX.D(self.msg, "  trying default resource path");
      loc := FindLocation(self, name, "default", env);
    END;
    IF loc = NIL THEN
      TRY
        MsgX.D(self.msg, "  trying global resource path");
        loc := Rsrc.Get(name, self.rpath);
        rd := TextRd.New(loc);
      EXCEPT
        Rsrc.NotFound => (* skip *)
      | Rd.Failure => (* skip *)
      | Thread.Alerted => (* skip *)
      END;
    ELSE
      TRY
        rd := FileRd.Open(loc);
      EXCEPT
        OSError.E => RAISE RsrcService.E("cannot open file " & loc);
      END;
    END;
    IF rd = NIL THEN
      MsgX.D(self.msg, "  not found");
    ELSE
      MsgX.D(self.msg, "  returning resource " & name);
    END;
    RETURN rd;
  END GetRsrcReader;

(*---------------------------------------------------------------------------*)
PROCEDURE GetRsrcAsText(self : T; name : TEXT; 
                        env : TextTextTbl.T := NIL) : TEXT
  RAISES {RsrcService.E, Thread.Alerted} =
  VAR
    loc : TEXT;
    rd  : Rd.T := NIL;
    res : TEXT := NIL;
  BEGIN
    MsgX.D2(self.msg, "GetRsrcAsText", " name = " & name);
    MsgX.D(self.msg, "  trying resource path for name " & name);
    loc := FindLocation(self, name, name, env);
    IF loc = NIL THEN
      MsgX.D(self.msg, "  trying default resource path");
      loc := FindLocation(self, name, "default", env);
    END;
    IF loc = NIL THEN
      TRY
        MsgX.D(self.msg, "  trying global resource path");
        res := Rsrc.Get(name, self.rpath);
      EXCEPT
        Rsrc.NotFound => (* skip *)
      | Rd.Failure => (* skip *)
      END;
    ELSE
      TRY
        rd := FileRd.Open(loc);
        res := Rd.GetText(rd, LAST(INTEGER));
        Rd.Close(rd);
      EXCEPT
        OSError.E => RAISE RsrcService.E("cannot open file " & loc);
      | Rd.Failure => RAISE RsrcService.E("cannot read file " & loc);
      END;
    END;
    IF res = NIL THEN
      MsgX.D(self.msg, "  not found");
    ELSE
      MsgX.D(self.msg, "  returning resource " & name);
    END;
    RETURN res;
  END GetRsrcAsText;

(*---------------------------------------------------------------------------*)
PROCEDURE FindLocation(self : T; name, rpname : TEXT; 
                       env : TextTextTbl.T := NIL) : TEXT 
  RAISES {RsrcService.E} =
  VAR
    rp : TextSeq.T;
  BEGIN
    IF self.tab.get(rpname, rp) THEN
      FOR i := 0 TO rp.size() - 1 DO
        IF Pathname.Absolute(name) THEN
          MsgX.D(self.msg, "    checking absolute pn " & name);
          IF FSUtils.IsFile(name) THEN
            MsgX.D(self.msg, "      found " & name);
            RETURN name;
          END;
        ELSE
          WITH prefix = rp.get(i),
               fn = Pathname.Join(prefix, name, NIL) DO
            IF env = NIL THEN
              env := ProcessEnv.Current();
            END;
            TRY
              WITH fns = TextUtils.SubstituteVariables(fn, env) DO
                MsgX.D(self.msg, "    checking pn " & fns);
                IF FSUtils.IsFile(fns) THEN
                  MsgX.D(self.msg, "      found " & fns);
                  RETURN fns;
                END;
              END;
            EXCEPT
              TextUtils.Error(e) => RAISE RsrcService.E(e);
            END;
          END;
        END;
      END;
    END;
    MsgX.D(self.msg, "    nothing found for " & name);
    RETURN NIL;
  END FindLocation;

BEGIN
END ConfigRsrcService.
