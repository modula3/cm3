MODULE DateOps;

IMPORT
  Date,
  Fmt,
  Text,
  Time;

IMPORT
  TextOps;

PROCEDURE Now(): T =
  BEGIN
    RETURN FromTime(Time.Now());
  END Now;

PROCEDURE ToText(t: T; mask: TEXT := DefaultMask; lang: Language := Language.Serbian): TEXT =
  VAR
    i: CARDINAL := 0;
    res: TEXT := "";
  BEGIN
    WHILE i < Text.Length(mask) DO
      CASE Text.GetChar(mask, i) OF
      | 'd' => res:=res & Fmt.F("%s", Fmt.Int(t.day));
      | 'D' => res:=res & Fmt.F("%02s", Fmt.Int(t.day));
      | 'm' => res:=res & Fmt.F("%s", Fmt.Int(ORD(t.month)+1));
      | 'M' => res:=res & Fmt.F("%02s", Fmt.Int(ORD(t.month)+1));
      | 'y' => res:=res & Fmt.F("%02s", Fmt.Int(t.year MOD 100));
      | 'Y' => res:=res & Fmt.F("%s", Fmt.Int(t.year));
      | 'h' => res:=res & Fmt.F("%02s", Fmt.Int(t.hour));
      | 'i' => res:=res & Fmt.F("%02s", Fmt.Int(t.minute));
      | 's' => res:=res & Fmt.F("%02s", Fmt.Int(t.second));
      | 'a' => res:=res & Text.Sub(DayName[lang, t.weekDay], 0, 3);
      | 'A' => res:=res & DayName[lang, t.weekDay];
      | 'b' => res:=res & Text.Sub(MonthName[lang, t.month], 0, 3);
      | 'B' => res:=res & MonthName[lang, t.month];
      ELSE
        res:=res & Text.FromChar(Text.GetChar(mask, i));
      END;
      INC(i);
    END;

    RETURN res;
  END ToText;

PROCEDURE FromText(text: TEXT): T =
  VAR
    t: T;
    d: Date.T;
    date, time: TEXT;
  BEGIN
    IF text = NIL THEN
      RETURN NIL;
    END;

    text:=TextOps.RemoveSpaces(text);
    date:=TextOps.RemoveSpaces(TextOps.ItemS(text, " ", 0));
    time:=TextOps.RemoveSpaces(TextOps.ItemS(text, " ", 1));

    d.day:=TextOps.ToInt(TextOps.ItemS(date, "/", 0), min:=1, max:=31);
    d.month:=VAL(TextOps.ToInt(TextOps.ItemS(date, "/", 1), min:=1, max:=12)-1, Date.Month);
    d.year:=TextOps.ToInt(TextOps.ItemS(date, "/", 2));

    d.hour:=TextOps.ToInt(TextOps.ItemS(time, ":", 0));
    d.minute:=TextOps.ToInt(TextOps.ItemS(time, ":", 1));
    d.second:=TextOps.ToInt(TextOps.ItemS(time, ":", 2));

    d.zone:=NIL;
    d.offset:=0;

    t:=NEW(T);
    t^:=d;
    t.zone:=NIL;
    t.offset:=0;
    
    RETURN t;
  END FromText;
  
PROCEDURE FromTime(t: Time.T): T =
  VAR
    d: T;
  BEGIN
    d:=NEW(T);
    d^:=Date.FromTime(t);
    d.offset:=0;
    d.zone:=NIL;
    RETURN d;
  END FromTime;

PROCEDURE FromDate(d: Date.T): T =
  VAR
    t: T;
  BEGIN
    t:=NEW(T);
    t^:=d;
    RETURN t;
  END FromDate;

PROCEDURE ToTime(READONLY d: T): Time.T RAISES { Date.Error } =
  BEGIN
    d.offset:=Date.FromTime(Date.ToTime(d^)).offset;
    RETURN Date.ToTime(d^);
  END ToTime;

PROCEDURE Copy(READONLY d: T): T =
  VAR
    t: T;
  BEGIN
    t:=NEW(T);
    t^:=d^;
    RETURN t;
  END Copy;

BEGIN
END DateOps.


