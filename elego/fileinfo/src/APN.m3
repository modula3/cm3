(*--------------------------------------------------------------------------*)
MODULE APN;

IMPORT Word, Text, TextSeq, Pathname, MxConfig;
IMPORT TextExtras AS TextEx, TextUtils, SMsg AS Msg;

(*--------------------------------------------------------------------------*)
TYPE
  REVEAL T = Default BRANDED "APNType 0.1" OBJECT
    p : Pathname.T;
    hashValue : Word.T := 0;
  METHODS
    hashMe() := HashMe;
    (* empty *)
  OVERRIDES
    init := Init;
    denotation := Denotation;
    isValid := IsValid;
    isAbsolute := IsAbsolute;
  END;

(*--------------------------------------------------------------------------*)
PROCEDURE Init(self : T; pn : TEXT; type : Type) : T =
  VAR s : TextSeq.T;
  BEGIN
    IF type = Type.Posix THEN
      s := TextUtils.Split(pn, "/");
      IF Text.GetChar(pn, 0) = '/' THEN
        s.addlo("/");
      ELSE
        s.addlo(NIL);
      END;
      TRY
        self.p := Pathname.Compose(s);
      EXCEPT
        Pathname.Invalid => Msg.Error("invalid pathname: " & pn);
                            self.p := pn;
      END;
    ELSIF type = Type.Win THEN
      s := TextUtils.Split(pn, "\\");
      IF Text.GetChar(pn, 0) = '\\' THEN
        s.addlo("\\");
      ELSIF Text.Length(pn) > 1 AND Text.GetChar(pn, 1) = ':' THEN
        (* FIXME: add support for the more complicated cases of 
                  windows file names here *)
        (* skip *)
      ELSE
        s.addlo(NIL);
      END;
      TRY
        self.p := Pathname.Compose(s);
      EXCEPT
        Pathname.Invalid => Msg.Error("invalid pathname: " & pn);
                            self.p := pn;
      END;
    ELSE
      self.p := pn;
    END;
    RETURN self;
  END Init;

(*--------------------------------------------------------------------------*)
PROCEDURE HashMe(self : T) =
  BEGIN
    IF NOT caseSensitivePathnames THEN
      self.hashValue := Text.Hash(TextUtils.Lower(self.p));
    ELSE
      self.hashValue := Text.Hash(self.p);
    END;
  END HashMe;

(*--------------------------------------------------------------------------*)
PROCEDURE Denotation(self : T; type : Type) : Pathname.T =
  BEGIN
    IF type = Type.Posix OR 
       type = Type.Native AND posix THEN
      (* change all backslash characters to slash *)
      IF Text.FindChar(self.p, '\\') > -1 THEN
        self.p := TextUtils.SubstChar(self.p, '\\', '/');
      END;
    ELSIF type = Type.Win OR
          type = Type.Native AND win32 THEN
      (* change all slash characters to backslash *)
      IF Text.FindChar(self.p, '/') > -1 THEN
        self.p :=TextUtils.SubstChar(self.p, '/', '\\');
      END;
    ELSIF NOT posix AND NOT win32 THEN
      Msg.Warning("unknown HOST_OS_TYPE in MxConfig, not converting pathnames");
    END;
    RETURN self.p;
  END Denotation;

(*--------------------------------------------------------------------------*)
PROCEDURE IsValid(self : T) : BOOLEAN =
  BEGIN
    RETURN Pathname.Valid(self.p);
  END IsValid;

(*--------------------------------------------------------------------------*)
PROCEDURE IsAbsolute(self : T) : BOOLEAN =
  BEGIN
    RETURN Pathname.Absolute(self.p);
  END IsAbsolute;

(*--------------------------------------------------------------------------*)
PROCEDURE New(pn : TEXT; type := Type.Default) : T =
  BEGIN
    RETURN NEW(T).init(pn, type);
  END New;

(*--------------------------------------------------------------------------*)
PROCEDURE Equal(p, q : T) : BOOLEAN =
  BEGIN
    (* better to compare words than strings *)
    IF p.hashValue = 0 THEN p.hashMe() END;
    IF q.hashValue = 0 THEN q.hashMe() END;
    IF caseSensitivePathnames THEN
      RETURN (p.hashValue = q.hashValue) AND Text.Equal(p.p, q.p);
    ELSE
      RETURN (p.hashValue = q.hashValue) AND TextEx.CIEqual(p.p, q.p);
    END;
  END Equal;

(*--------------------------------------------------------------------------*)
PROCEDURE Hash(pn : T) : Word.T =
  BEGIN
    IF pn.hashValue = 0 THEN pn.hashMe() END;
    RETURN pn.hashValue;
  END Hash;

(*--------------------------------------------------------------------------*)
PROCEDURE Valid(pn: T): BOOLEAN =
  BEGIN
    RETURN Pathname.Valid(pn.p);
  END Valid;

(*--------------------------------------------------------------------------*)
PROCEDURE Absolute(pn: T): BOOLEAN =
  BEGIN
    RETURN Pathname.Absolute(pn.p);
  END Absolute;

(*--------------------------------------------------------------------------*)
PROCEDURE Prefix(pn: T): T =
  BEGIN
    RETURN New(Pathname.Prefix(pn.p));
  END Prefix;

(*--------------------------------------------------------------------------*)
PROCEDURE Last(pn: T): T =
  BEGIN
    RETURN New(Pathname.Last(pn.p));
  END Last;

(*--------------------------------------------------------------------------*)
PROCEDURE Base(pn: T): T =
  BEGIN
    RETURN New(Pathname.Base(pn.p));
  END Base;

(*--------------------------------------------------------------------------*)
PROCEDURE Join(pn, base: T; ext: TEXT): T =
  BEGIN
    RETURN New(Pathname.Join(pn.p, base.p, ext));
  END Join;

(*--------------------------------------------------------------------------*)
PROCEDURE JoinS(pn:T; base, ext: TEXT): T =
  BEGIN
    RETURN New(Pathname.Join(pn.p, base, ext));
  END JoinS;

(*--------------------------------------------------------------------------*)
PROCEDURE LastBase(pn: T): T =
  BEGIN
    RETURN New(Pathname.LastBase(pn.p));
  END LastBase;

(*--------------------------------------------------------------------------*)
PROCEDURE LastExt(pn: T): TEXT =
  BEGIN
    RETURN Pathname.LastExt(pn.p);
  END LastExt;

(*--------------------------------------------------------------------------*)
PROCEDURE ReplaceExt(pn: T; ext: TEXT): T =
  BEGIN
    RETURN New(Pathname.ReplaceExt(pn.p, ext));
  END ReplaceExt;

(*--------------------------------------------------------------------------*)
VAR
  posix := MxConfig.HOST_OS_TYPE() = MxConfig.OS_TYPE.POSIX;
  win32 := MxConfig.HOST_OS_TYPE() = MxConfig.OS_TYPE.WIN32;
BEGIN
  caseSensitivePathnames := NOT win32;
END APN.

