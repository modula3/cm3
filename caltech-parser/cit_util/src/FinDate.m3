(* $Id$ *)

MODULE FinDate;
IMPORT Scan, TextReader, FloatMode, Lex;
IMPORT Fmt, Text, Integer;
IMPORT Date, XTime AS Time;
IMPORT Word;
IMPORT IntFinDateTbl;

CONST TE = Text.Equal;

PROCEDURE ParseCmdLine(c : TEXT) : T RAISES { ParseError } =
  BEGIN
    TRY
      VAR
        year := Scan.Int(Text.Sub(c, 0, 4));
        month := Scan.Int(Text.Sub(c, 4, 2));
        day := Scan.Int(Text.Sub(c, 6, 2));
      BEGIN
        ValiDate(year, month, day);
        RETURN
          T { year, month, day }
      END
    EXCEPT
      FloatMode.Trap, Lex.Error => RAISE ParseError 
    END
  END ParseCmdLine;

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN = BEGIN RETURN a = b END Equal;

PROCEDURE Compare(READONLY a, b : T) : [-1..1] =
  BEGIN
    IF    a.year > b.year THEN   RETURN 1
    ELSIF a.year < b.year THEN   RETURN -1
    ELSIF a.month > b.month THEN RETURN 1
    ELSIF a.month < b.month THEN RETURN -1
    ELSE                         RETURN Integer.Compare(a.day, b.day)
    END
  END Compare;

PROCEDURE ValiDate(year, month, day : INTEGER) RAISES { ParseError } =
  BEGIN
    IF month < FIRST(Month) OR month > LAST(Month) THEN
      RAISE ParseError
    END;

    IF day < FIRST(Day) OR day > MaxDays[month] THEN
      RAISE ParseError
    END;

    IF month = 2 AND day > GregorianFebDays(year) THEN
      RAISE ParseError
    END
  END ValiDate;

PROCEDURE ParseFed(date :TEXT) : T RAISES { ParseError } =
  VAR
    reader := NEW(TextReader.T).init(date);
  BEGIN
    TRY
      VAR
        c1 := reader.nextE("-");
        c2 := reader.nextE("-");
        c3 := reader.nextE("-");
        year :=  Scan.Int(c1);
        month := Scan.Int(c2);
        day :=   Scan.Int(c3);
      BEGIN
        IF NOT reader.isEmpty() THEN RAISE ParseError END;
        
        ValiDate(year, month, day);
        
        RETURN T { year, month, day }
      END
    EXCEPT
      FloatMode.Trap, Lex.Error, TextReader.NoMore => RAISE ParseError
    END
  END ParseFed;

PROCEDURE ParseBritish(date :TEXT) : T RAISES { ParseError } =
  VAR
    reader := NEW(TextReader.T).init(date);
  BEGIN
    TRY
      VAR
        c1 := reader.nextE("/");
        c2 := reader.nextE("/");
        c3 := reader.nextE("/");
        day :=   Scan.Int(c1);
        month := Scan.Int(c2);
        year :=  Scan.Int(c3);
      BEGIN
        IF NOT reader.isEmpty() THEN RAISE ParseError END;
        
        ValiDate(year, month, day);
        
        RETURN T { year, month, day }
      END
    EXCEPT
      FloatMode.Trap, Lex.Error, TextReader.NoMore => RAISE ParseError
    END
  END ParseBritish;

PROCEDURE ParseAmerican(date :TEXT) : T RAISES { ParseError } =
  VAR
    reader := NEW(TextReader.T).init(date);
  BEGIN
    TRY
      VAR
        c1 := reader.nextE("/");
        c2 := reader.nextE("/");
        c3 := reader.nextE("/");
        month := Scan.Int(c1);
        day :=   Scan.Int(c2);
        year :=  Scan.Int(c3);
      BEGIN
        IF NOT reader.isEmpty() THEN RAISE ParseError END;
        
        ValiDate(year, month, day);
        
        RETURN T { year, month, day }
      END
    EXCEPT
      FloatMode.Trap, Lex.Error, TextReader.NoMore => RAISE ParseError
    END
  END ParseAmerican;

PROCEDURE ParseDaily(date : TEXT) : T RAISES { ParseError } =
  BEGIN
    VAR
      reader := NEW(TextReader.T).init(date);
    BEGIN
      TRY
        VAR
          c1 := reader.nextE("-");
          c2 := reader.nextE("-");
          c3 := reader.nextE("-");
          day := Scan.Int(c1);
          month := ParseMonth(c2);
          year := Scan.Int(c3);
        BEGIN
          IF NOT reader.isEmpty() THEN RAISE ParseError END;
          (* if year is two-digit, make it 19XX or 20XX *)
          IF year < 100 THEN
            (* Y2050 bug... *)
            IF year <= JustNow.year - 2000 THEN 
              year := year + 2000 
            ELSE 
              year := year + 1900 
            END
          END;

          ValiDate(year, month, day);

          RETURN T { year, month, day }
        END
      EXCEPT
        FloatMode.Trap, Lex.Error, TextReader.NoMore => RAISE ParseError
      END
    END
  END ParseDaily;

VAR JustNow := Today(NIL);

PROCEDURE Format(READONLY d : T) : TEXT =
  BEGIN
    RETURN Fmt.Int(d.day) & "-" & MonthNames[d.month] & "-" & Fmt.Int(d.year)
  END Format;

PROCEDURE FormatSortable(READONLY d : T) : TEXT =
  BEGIN
    RETURN Fmt.Int(d.year) & "-" & 
           Fmt.F("%02s", Fmt.Int(d.month)) & "-" &
           Fmt.F("%02s", Fmt.Int(d.day))
  END FormatSortable;

PROCEDURE FormatFIX(READONLY d : T) : TEXT =
  BEGIN
    RETURN 
      Fmt.F("%04s%02s%02s",Fmt.Int(d.year), Fmt.Int(d.month), Fmt.Int(d.day))
  END FormatFIX;

PROCEDURE Julian(READONLY d : T) : CARDINAL =
  (* see http://aa.usno.navy.mil/faq/docs/JD_Formula.html *)
  VAR
    K := FLOAT(d.year);
    M := FLOAT(d.month);
    I := FLOAT(d.day);
    t1 := 367*TRUNC(K);
    t2 := -TRUNC(7.0*(K+FLOAT(TRUNC((M+9.0)/12.0)))/4.0);
    t3 := TRUNC(275.0*M/9.0);
    t4 := ROUND(I);
    t5 := 1721014; (* noon GMT *)
  BEGIN
    RETURN t1 + t2 + t3 + t4 + t5
  END Julian;

PROCEDURE ParseMonth(month : TEXT) : [1..12] RAISES { ParseError } =
  BEGIN
    FOR i := FIRST(Month) TO LAST(Month) DO
      IF Text.Equal(month, MonthNames[i]) THEN RETURN i END
    END;
    RAISE ParseError
  END ParseMonth;

CONST 
  MaxDays = ARRAY OF [0..31] { 0, 
              31, 29, 31, 30, 31, 30,
              31, 31, 30, 31, 30, 31 };

PROCEDURE Today(zone : Date.TimeZone) : T =
  BEGIN RETURN FromTime(Time.Now(),zone) END Today;

PROCEDURE FromTime(t : Time.T; zone : Date.TimeZone) : T =
  VAR
    d := Date.FromTime(t, z := zone);
  BEGIN
    RETURN T { year := d.year, month := ORD(d.month) + 1, day := d.day }
  END FromTime;

PROCEDURE FromDate(d : Date.T) : T =
  BEGIN
    RETURN T { year := d.year, month := ORD(d.month) + 1, day := d.day }
  END FromDate;

PROCEDURE Morning(t : T) : Date.T =
  BEGIN
    RETURN Date.T { year := t.year,
                    month := VAL(t.month-1,Date.Month),
                    day := t.day,
                    hour := 0,
                    minute := 0,
                    second := 0, 
                    offset := 0,
                    zone := NIL,
                    weekDay := FIRST(Date.WeekDay) }
  END Morning;

PROCEDURE DayOfWeek(t : T) : Date.WeekDay =
  <*FATAL Date.Error*>
  BEGIN
    WITH morning  = Morning(t),
         tm       = Date.ToTime(morning),  (* probably uses UTC *)
         d        = Date.FromTime(tm, z := Date.UTC) DO
      RETURN d.weekDay
    END
  END DayOfWeek;

PROCEDURE Yesterday(zone : Date.TimeZone) : T =
  CONST
    Steps = ARRAY Date.WeekDay OF [1..3] { 2, 3, 1, 1, 1, 1, 1 };
  VAR
    t := Time.Now();
    d := Date.FromTime(t, z := zone);
    res := Today(zone);
  BEGIN
    FOR i := 1 TO Steps[d.weekDay] DO
      res := Prev(res)
    END;
    RETURN res
  END Yesterday;

PROCEDURE Hash(READONLY a : T) : Word.T = 
  BEGIN
    RETURN Word.Times(a.day, Word.Times(a.month, a.year))
  END Hash;

PROCEDURE Min(READONLY a, b : T) : T =
  BEGIN
    IF Compare(a, b) < 0 THEN RETURN a ELSE RETURN b END
  END Min;

PROCEDURE Max(READONLY a, b : T) : T =
  BEGIN
    IF Compare(a, b) > 0 THEN RETURN a ELSE RETURN b END
  END Max;

PROCEDURE GregorianFebDays(y : INTEGER) : [28..29] =
  BEGIN
    IF y MOD 100 = 0 THEN
      IF y MOD 400 = 0 THEN 
        RETURN 29
      ELSE
        RETURN 28
      END
    ELSIF y MOD 4 = 0 THEN
      RETURN 29
    ELSE
      RETURN 28
    END
  END GregorianFebDays;

PROCEDURE Next(READONLY a : T) : T = 
  BEGIN
    IF a.month = 12 AND a.day = 31 THEN
      RETURN T { a.year + 1, 1, 1 }
    ELSIF a.month # 2 AND a.day = MaxDays[a.month] OR 
          a.month = 2 AND a.day = GregorianFebDays(a.year) THEN
      RETURN T { a.year, a.month + 1, 1 }
    ELSE
      RETURN T { a.year, a.month, a.day + 1}
    END
  END Next;

PROCEDURE Advance(READONLY a : T; by : CARDINAL) : T =
  VAR
    res := a;
  BEGIN
    FOR i := 1 TO by DO
      res := Next(res)
    END;
    RETURN res
  END Advance;

PROCEDURE Prev(READONLY a : T) : T = 
  BEGIN
    IF a.month = 1 AND a.day = 1 THEN
      RETURN T { a.year - 1, 12, 31 }
    ELSIF a.day = 1 AND a.month # 3 THEN
      RETURN T { a.year, a.month - 1, MaxDays[a.month-1] }
    ELSIF a.day = 1 AND a.month = 3 THEN
      RETURN T { a.year, 2, GregorianFebDays(a.year) }
    ELSE
      RETURN T { a.year, a.month, a.day - 1 }
    END
  END Prev;

PROCEDURE CalendarSub(READONLY a, b : T) : INTEGER =
  BEGIN RETURN Julian(a) - Julian(b) END CalendarSub;

VAR
  mu := NEW(MUTEX);
  tbl := NEW(IntFinDateTbl.Default).init();

PROCEDURE FromJulian(j : CARDINAL) : T RAISES { OutOfRange } =
  VAR
    d := JustNow;
    success := FALSE;
    jj : CARDINAL;
  BEGIN
    IF j < LongAgoJulian OR j > FarFutureJulian THEN
      RAISE OutOfRange
    END;
    LOCK mu DO
      IF NOT tbl.get(j, d) THEN
        REPEAT
          jj := Julian(d);
          EVAL tbl.put(jj,d);
          IF jj > j THEN
            d := Prev(d)
          ELSIF jj < j THEN
            d := Next(d)
          ELSE
            success := TRUE
          END
        UNTIL success
      END;
      RETURN d
    END
  END FromJulian;

VAR ZeroByMicrosoftOLE := Julian(T { 1899, 12, 30 });

PROCEDURE FromMicrosoftOLE(m : CARDINAL) : T RAISES { OutOfRange } =
  BEGIN
    RETURN FromJulian(ZeroByMicrosoftOLE + m)
  END FromMicrosoftOLE;

PROCEDURE ParseJulian(t : TEXT) : T RAISES { ParseError } =
  BEGIN
    TRY
      RETURN FromJulian(Scan.Int(t))
    EXCEPT
      Lex.Error, FloatMode.Trap, OutOfRange => (* skip *)
    END;

    TRY
      RETURN FromJulian(ROUND(Scan.LongReal(t)))
    EXCEPT
      Lex.Error, FloatMode.Trap, OutOfRange => (* skip *)
    END;
  
    RAISE ParseError
  END ParseJulian;

PROCEDURE Parse(t : TEXT) : T RAISES { ParseError } =
  BEGIN
    TRY
      RETURN ParseJulian(t)
    EXCEPT
      ParseError => (* skip *)
    END;

    TRY
      RETURN ParseDaily(t)
    EXCEPT
      ParseError => (* skip *)
    END;
    
    TRY
      RETURN ParseFed(t)
    EXCEPT
      ParseError => (* skip *)
    END;

    TRY
      RETURN ParseCmdLine(t)
    EXCEPT
      ParseError => (* skip *)
    END;

    TRY 
      RETURN ParseAmerican(t)
    EXCEPT
      ParseError => (* skip *)
    END;

    RAISE ParseError
  END Parse;

(**********************************************************************)

PROCEDURE MYFormatFIX(READONLY my : MonthYear) : TEXT =
  BEGIN
    RETURN 
      Fmt.F("%04s%02s",Fmt.Int(my.year), Fmt.Int(my.month))
  END MYFormatFIX;

PROCEDURE MYParseFIX(c : TEXT) : MonthYear RAISES { ParseError } =
  BEGIN
    TRY
      VAR
        year := Scan.Int(Text.Sub(c, 0, 4));
        month := Scan.Int(Text.Sub(c, 4, 2));
      BEGIN
        RETURN MonthYear { year, month }
      END
    EXCEPT
      FloatMode.Trap, Lex.Error => RAISE ParseError 
    END
  END MYParseFIX;

PROCEDURE MYEqual(READONLY a, b : MonthYear) : BOOLEAN = 
  BEGIN RETURN a = b END MYEqual;

(**********************************************************************)

PROCEDURE ParseSpecificFormat(t : TEXT) : SpecificFormat RAISES { ParseError }=
  BEGIN
    FOR i := FIRST(SpecificFormat) TO LAST(SpecificFormat) DO
      IF TE(t, SpecificFormatNames[i]) THEN RETURN i END
    END;
    RAISE ParseError
  END ParseSpecificFormat;

VAR
  LongAgoJulian := Julian(LongAgo);
  FarFutureJulian := Julian(FarFuture);
BEGIN END FinDate.
