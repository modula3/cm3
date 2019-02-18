(* $Id$ *)

MODULE HMTime;
IMPORT TextReader, Scan, Lex, FloatMode, Date, Fmt;
IMPORT FinDate, Word;
IMPORT Text, TextUtils;

CONST TE = Text.Equal;

PROCEDURE Format(t : T) : TEXT =
  CONST
    I = Fmt.Int;
  BEGIN
    RETURN Fmt.F("%02s:%02s:%02s", I(t.hour), I(t.minute), I(t.second))
  END Format;

PROCEDURE FormatHMs(t : T) : TEXT =
  CONST
    I = Fmt.Int;
  VAR 
    res := Fmt.F("%02s:%02s", I(t.hour), I(t.minute));
  BEGIN
    IF t.second = 0 THEN
      RETURN res
    ELSE
      RETURN res & Fmt.F(":%02s", I(t.second))
    END
  END FormatHMs;

PROCEDURE Check(h,m,s : INTEGER) RAISES { ParseError } =
  BEGIN
    (* since we don't have the date here, we permit a leap second at
       the end of any day.  in reality, leap seconds can only occur at 
       the end of the month, and HAVE only occurred on the last days of
       June and December. *)
    IF h < 0 OR h > 23 OR 
       m < 0 OR m > 59 OR 
       s < 0 OR (s > 59 AND (h < 23 OR m < 59)) OR s > 60 THEN
      RAISE ParseError
    END
  END Check;

PROCEDURE Parse(t : TEXT; f : F1224) : T RAISES { ParseError } = 
  BEGIN
    TRY

      PROCEDURE ParseAmPm(lower : TEXT) : BOOLEAN =
        BEGIN
          IF TE(lower, "am") THEN (* skip *)
          ELSIF TE(lower, "pm") THEN INC(h,12)
          ELSE RETURN FALSE
          END;
          RETURN TRUE
        END ParseAmPm;

      VAR
        reader := NEW(TextReader.T).init(t);
        h, m := Scan.Int(reader.nextE(":"));
        s := 0;
      BEGIN
        (* hmm *)

        IF f = F1224.F24 THEN
          IF NOT reader.empty() THEN
            WITH s = Scan.Int(reader.nextE("")) DO
              Check(h,m,s);
            END
          END
        ELSE
          WITH trail = TextUtils.ToLower(reader.nextE(":")) DO
            IF NOT ParseAmPm(trail) THEN
              s := Scan.Int(trail); 
              IF NOT ParseAmPm(TextUtils.ToLower(reader.nextE(""))) THEN
                RAISE ParseError
              END
            END;
          END
        END;
        RETURN T { h, m, s }
      END
    EXCEPT
      TextReader.NoMore, Lex.Error, FloatMode.Trap => RAISE ParseError
    END
  END Parse;

PROCEDURE ParseSubsecond(t : TEXT; VAR sub : LONGREAL) : T RAISES { ParseError } = 
  BEGIN
    TRY
      VAR
        reader := NEW(TextReader.T).init(t);
        h, m := Scan.Int(reader.nextE(":"));
      BEGIN
        IF NOT reader.empty() THEN
          WITH s = Scan.Int(reader.nextE(".")) DO
            Check(h,m,s);
            IF NOT reader.empty() THEN 
              WITH subT = "0." & reader.nextE("") DO
                sub := Scan.LongReal(subT)
              END
            ELSE
              sub := 0.0d0
            END;
            RETURN T { h, m, s }
          END
        ELSE
          sub := 0.0d0;
          RETURN T { h, m, 0 }
        END
      END
    EXCEPT
      TextReader.NoMore, Lex.Error, FloatMode.Trap => RAISE ParseError
    END
  END ParseSubsecond;

PROCEDURE Truncate(READONLY d : Date.T) : T =
  BEGIN RETURN T { d.hour, d.minute, d.second } END Truncate;

PROCEDURE Assemble(READONLY t : T;
                   READONLY f : FinDate.T;
                   proto : Date.T) : Date.T =
  BEGIN
    proto.hour   := t.hour;
    proto.minute := t.minute;
    proto.second := t.second;

    proto.year  := f.year;
    proto.month := VAL(f.month-1,Date.Month);
    proto.day   := f.day;

    RETURN proto
  END Assemble;

PROCEDURE Compare(READONLY a, b : T) : [-1..1] =
  BEGIN
    IF    a.hour   > b.hour   THEN RETURN  1 
    ELSIF a.hour   < b.hour   THEN RETURN -1
    ELSIF a.minute > b.minute THEN RETURN  1
    ELSIF a.minute < b.minute THEN RETURN -1
    ELSIF a.second > b.second THEN RETURN  1
    ELSIF a.second < b.second THEN RETURN -1
    ELSE
      RETURN 0
    END
  END Compare;

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN = BEGIN RETURN a = b END Equal;

PROCEDURE Hash(READONLY a : T) : Word.T =
  BEGIN RETURN SecondInDay(a) END Hash;

PROCEDURE SecondInDay(READONLY a : T) : CARDINAL =
  BEGIN RETURN (a.hour * 60 + a.minute) * 60 + a.second END SecondInDay;

PROCEDURE Advance(READONLY a : T; bySeconds : CARDINAL) : T 
  RAISES { Overflow } =
  BEGIN RETURN FromSeconds(SecondInDay(a)+bySeconds) END Advance;

PROCEDURE FromSeconds(s : CARDINAL) : T RAISES { Overflow } =
  CONST
    SecsInDay = 86400;
  VAR
    t : T;
  BEGIN
    IF s >= SecsInDay THEN RAISE Overflow(s - SecsInDay) END;
    
    t.second :=  s MOD 60;
    t.minute := (s DIV 60) MOD 60;
    t.hour   :=  s DIV 3600;

    RETURN t
  END FromSeconds;

PROCEDURE ParseF1224(t : TEXT) : F1224 RAISES { ParseError } = 
  BEGIN
    FOR i := FIRST(F1224) TO LAST(F1224) DO
      IF TE(t, F1224Names[i]) THEN RETURN i END
    END;
    RAISE ParseError
  END ParseF1224;

BEGIN END HMTime.
