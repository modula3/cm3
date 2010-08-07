(*--------------------------------------------------------------------------*)
MODULE Version;

IMPORT ASCII, Text, TextExtras AS TextEx, Fmt, Word; 

(*--------------------------------------------------------------------------*)
PROCEDURE Init(self : T) : T = 
  BEGIN
    self.major := Undefined; 
    self.minor:= Undefined; 
    self.patchlevel := Undefined;
    self.kind := 'r';
    self.valid := TRUE;
    RETURN self;
  END Init;

(*--------------------------------------------------------------------------*)
PROCEDURE IsValid(self : T) : BOOLEAN =
  BEGIN
    RETURN self.valid;
  END IsValid;

(*--------------------------------------------------------------------------*)
PROCEDURE Defined(self : T) : BOOLEAN =
  BEGIN
    RETURN self.valid AND (self.major # Undefined) AND
       (IsRelease(self) OR IsAlpha(self) OR IsBeta(self) OR 
       IsGamma(self) OR IsDevel(self) OR IsLatest(self)) OR
       IsChange(self) OR IsFeature(self) OR IsFix(self);
  END Defined;

(*--------------------------------------------------------------------------*)
PROCEDURE ExactDefined(self : T) : BOOLEAN =
  BEGIN
    RETURN Defined(self) AND self.minor # Undefined AND
           self.patchlevel # Undefined;
  END ExactDefined;

(*--------------------------------------------------------------------------*)
PROCEDURE FromTextI(self : T; t : TEXT; seps : SET OF CHAR) : T =

  PROCEDURE AddDigit(VAR n : CARDINAL; c : CHAR) =
    VAR val := ORD(c) - ORD('0');
    BEGIN
      IF n = Undefined THEN
        n := val;
      ELSE
        n := n * 10 + val;
      END;
    END AddDigit;

  TYPE State = {Error, Kind,
                Major, MajorDig, MajorDone,
                Minor, MinorDig, MinorDone,
                PL, PLDig, PLDone};
  VAR 
    len := Text.Length(t);
    s   := State.Kind;
    c   : CHAR := ' ';
    sep : CHAR := ASCII.NUL;
    i : CARDINAL := 0;
  BEGIN (* FromText *)
    EVAL Init(self);
    self.valid := FALSE;

    WHILE i < len DO
      c := Text.GetChar(t, i);

      IF c IN ASCII.Digits THEN
        CASE s OF
          State.Kind, State.Major, State.MajorDig =>
          BEGIN
            s := State.MajorDig;
            AddDigit(self.major, c);
          END;
        | State.Minor, State.MinorDig =>
          BEGIN
            s := State.MinorDig;
            AddDigit(self.minor, c);
          END;
        | State.PL, State.PLDig =>
          BEGIN
            s := State.PLDig;
            AddDigit(self.patchlevel, c);
          END;
        ELSE
          s := State.Error;
        END;
      ELSIF c = 'x' THEN
        CASE s OF
          State.Kind, State.Major => s := State.MajorDone;
        | State.Minor => s := State.MinorDone;
        | State.PL => s := State.PLDone;
        ELSE
          s := State.Error;
        END;
      ELSIF c IN SET OF CHAR{'d', 'a', 'b', 'g', 'l', 'r', 'c', 'f', 'F'}  THEN
        IF s = State.Kind THEN
          s := State.Major;
          self.kind := c;
        ELSE
          s := State.Error;
        END;
      ELSIF c IN seps THEN
        IF sep = ASCII.NUL THEN
          (* Remember the first seperator char. *)
          sep := c;
        END;
        IF c # sep THEN
          (* Mixed use of seperator chars. Bad! *)
          s := State.Error;
        ELSE
          CASE s OF
            State.MajorDig, State.MajorDone => s := State.Minor;
          | State.MinorDig, State.MinorDone => s := State.PL;
          ELSE
            s := State.Error;
          END;
        END;
      ELSIF c IN ASCII.Spaces AND s = State.Kind THEN
        (* Skip leading whitespace. *)
      ELSE
        (* This is some crazy garbage. *)
        s := State.Error;
      END;

      IF s = State.Error THEN
        RETURN self;
      END;

      INC(i);
    END;
    
    <* ASSERT(s IN SET OF State {State.Kind, State.Major,
                                 State.MajorDig, State.MajorDone,
                                 State.MinorDig, State.MinorDone,
                                 State.PLDig, State.PLDone}) *>
    self.valid := TRUE;
    RETURN self;
  END FromTextI;

(*--------------------------------------------------------------------------*)
PROCEDURE ToTextI(self : T; sep : TEXT) : TEXT =
  
  PROCEDURE MemberAsText(n : CARDINAL) : TEXT =
    BEGIN
      IF n = Undefined THEN
        RETURN "x";
      ELSE
        RETURN Fmt.Int(n);
      END;
    END MemberAsText;

  VAR
    maj := MemberAsText(self.major);
    min := MemberAsText(self.minor);
    pat := MemberAsText(self.patchlevel);
  BEGIN
    IF IsRelease(self) THEN
      RETURN maj & sep & min & sep & pat;
    END;
    RETURN Text.FromChar(self.kind) & maj & sep & min & sep & pat;
  END ToTextI;

(*--------------------------------------------------------------------------*)
PROCEDURE FromText(self : T; t : TEXT) : T =
  BEGIN
    RETURN FromTextI(self, t, SET OF CHAR {'.'});
  END FromText;

(*--------------------------------------------------------------------------*)
PROCEDURE ToText(self : T) : TEXT =
  BEGIN
    RETURN ToTextI(self, ".");
  END ToText;

(*--------------------------------------------------------------------------*)
PROCEDURE FromDir(self : T; t : TEXT) : T =
  BEGIN
    RETURN FromTextI(self, t, SET OF CHAR {'_'});
  END FromDir;

(*--------------------------------------------------------------------------*)
PROCEDURE ToDir(self : T) : TEXT =
  BEGIN
    RETURN ToTextI(self, "_");
  END ToDir;

(*--------------------------------------------------------------------------*)
PROCEDURE FromDirOrText(self : T; t : TEXT) : T =
  BEGIN
    RETURN FromTextI(self, t, SET OF CHAR {'_', '.'});
  END FromDirOrText;

(*--------------------------------------------------------------------------*)
PROCEDURE Equal(self : T; v : T) : BOOLEAN =
  BEGIN
    RETURN (self.kind = v.kind)
    AND    (self.major = Undefined OR 
            v.major = Undefined OR 
            self.major = v.major)
    AND    (self.minor = Undefined OR 
            v.minor = Undefined OR 
            self.minor = v.minor) 
    AND    (self.patchlevel = Undefined OR
            v.patchlevel = Undefined OR 
            self.patchlevel = v.patchlevel);
  END Equal;

(*--------------------------------------------------------------------------*)
PROCEDURE IsRelease(self : T) : BOOLEAN =
  BEGIN
    RETURN self.kind = 'r';
  END IsRelease;

(*--------------------------------------------------------------------------*)
PROCEDURE IsAlpha(self : T) : BOOLEAN =
  BEGIN
    RETURN self.kind = 'a';
  END IsAlpha;

(*--------------------------------------------------------------------------*)
PROCEDURE IsBeta(self : T) : BOOLEAN =
  BEGIN
    RETURN self.kind = 'b';
  END IsBeta;

(*--------------------------------------------------------------------------*)
PROCEDURE IsGamma(self : T) : BOOLEAN =
  BEGIN
    RETURN self.kind = 'g';
  END IsGamma;

(*--------------------------------------------------------------------------*)
PROCEDURE IsDevel(self : T) : BOOLEAN =
  BEGIN
    RETURN self.kind = 'd';
  END IsDevel;

(*--------------------------------------------------------------------------*)
PROCEDURE IsLatest(self : T) : BOOLEAN =
  BEGIN
    RETURN self.kind = 'l';
  END IsLatest; 

(*--------------------------------------------------------------------------*)
PROCEDURE IsChange(self : T) : BOOLEAN =
  BEGIN
    RETURN self.kind = 'c';
  END IsChange; 

(*--------------------------------------------------------------------------*)
PROCEDURE IsFeature(self : T) : BOOLEAN =
  BEGIN
    RETURN self.kind = 'f';
  END IsFeature; 

(*--------------------------------------------------------------------------*)
PROCEDURE IsFix(self : T) : BOOLEAN =
  BEGIN
    RETURN self.kind = 'F';
  END IsFix; 

(*--------------------------------------------------------------------------*)
PROCEDURE Compatible(self : T; v : T; latestOverride := FALSE) : BOOLEAN =
  BEGIN
    RETURN latestOverride OR
           (self.kind = v.kind) AND 
           (self.major = v.major) AND (self.minor = v.minor);
  END Compatible;

(*--------------------------------------------------------------------------*)
PROCEDURE Hash(self : T) : Word.T =
  BEGIN
    RETURN Text.Hash(self.toText());
  END Hash;

(*--------------------------------------------------------------------------*)
PROCEDURE Less(self : T; v : T; latestOverride := FALSE) : BOOLEAN =
  BEGIN
    IF self.kind = v.kind THEN
      IF self.major = Undefined OR v.major = Undefined OR 
	 self.major < v.major THEN
	RETURN TRUE;
      END;
      IF self.major > v.major THEN RETURN FALSE; END;
      (* self.major = v.major *)
      IF self.minor = Undefined OR v.minor = Undefined OR 
	 self.minor < v.minor THEN
	RETURN TRUE;
      END;
      IF self.minor > v.minor THEN RETURN FALSE; END;
      (* self.major = v.major AND self.minor = v.minor *)
      IF self.patchlevel = Undefined OR v.patchlevel = Undefined OR
	 self.patchlevel < v.patchlevel THEN
	RETURN TRUE;
      END;
      RETURN FALSE;
    ELSE (* self.kind # v.kind *)
      IF IsLatest(self) THEN
        RETURN NOT latestOverride;
      ELSIF IsLatest(v) THEN
        RETURN latestOverride;
      END;
      IF IsDevel(self) AND NOT IsDevel(v) THEN
        RETURN TRUE;
      ELSIF NOT IsDevel(self) AND IsDevel(v) THEN
        RETURN FALSE;
      END;
      (* 'd' is the smallest version, all others are ordered alphabetically *)
      RETURN self.kind < v.kind;
    END;
  END Less;

(*--------------------------------------------------------------------------*)
PROCEDURE LessOrEqual(self : T; v : T; latestOverride := FALSE) : BOOLEAN =
  BEGIN
    RETURN Less(self, v, latestOverride) OR Equal(self, v);
  END LessOrEqual;

(*--------------------------------------------------------------------------*)
PROCEDURE NextMajor(self : T) =
  BEGIN
    IF self.major = Undefined THEN
      self.major := 0;
    ELSE
      INC(self.major);
    END;
    self.minor := 0; self.patchlevel := 0;
  END NextMajor;

(*--------------------------------------------------------------------------*)
PROCEDURE NextMinor(self : T) =
  BEGIN
    IF self.major = Undefined THEN
      self.major := 0; self.minor := 0; 
    ELSIF self.minor = Undefined THEN
      self.minor := 0;
    ELSE
      INC(self.minor);
    END;
    self.patchlevel := 0;
  END NextMinor;

(*--------------------------------------------------------------------------*)
PROCEDURE NextPatchLevel(self : T) =
  BEGIN
    IF self.major = Undefined THEN
      self.major := 0; self.minor := 0; self.patchlevel := 0;
    ELSIF self.minor = Undefined THEN
      self.minor := 0; self.patchlevel := 0;
    ELSIF self.patchlevel = Undefined THEN
      self.patchlevel := 0;
    ELSE
      INC(self.patchlevel);
    END;
  END NextPatchLevel;

(*--------------------------------------------------------------------------*)
PROCEDURE RangeInit(self : Range) : Range =
  BEGIN
    self.from := NEW(T).init();
    self.to := NEW(T).init();
    RETURN self;
  END RangeInit;

(*--------------------------------------------------------------------------*)
PROCEDURE Contains(self : Range; v : T; latestOverride := FALSE) : BOOLEAN =
  BEGIN
    RETURN latestOverride AND IsLatest(v) OR 
           LessOrEqual(self.from, v) AND LessOrEqual(v, self.to);
  END Contains;

(*--------------------------------------------------------------------------*)
PROCEDURE RangeFromText(self : Range; t : TEXT) =
  VAR
    len := Text.Length(t);
    i   : CARDINAL;
  BEGIN
    IF self.from = NIL THEN
      self.from := NEW(T).init();
    ELSE
      EVAL self.from.init();
    END;
    IF self.to = NIL THEN
      self.to := NEW(T).init();
    ELSE
      EVAL self.to.init();
    END;
    IF TextEx.FindChar(t, '-', i) THEN
      EVAL FromText(self.from, Text.Sub(t, 0, i));
      EVAL FromText(self.to, Text.Sub(t, i+1, len -i -1));
    ELSE
      EVAL FromText(self.from, t);
      self.to := self.from;
    END;      
  END RangeFromText;

(*--------------------------------------------------------------------------*)
PROCEDURE RangeToText(self : Range) : TEXT =
  VAR 
    f := ToText(self.from);
    t := ToText(self.to);
  BEGIN
    RETURN f & "-" & t;
  END RangeToText;

BEGIN (* Version *)
END Version.

  

