(* $Id$ *)

MODULE AscTimeParse;
IMPORT TZ, XTime AS Time, Date, Text, Scan;
IMPORT FloatMode, Lex;

PROCEDURE ParseDay(day : TEXT) : Date.WeekDay RAISES { IllegalFormat } =
  CONST
    Names = ARRAY Date.WeekDay OF TEXT {
    "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat" 
    };
  BEGIN
    FOR i := FIRST(Names) TO LAST(Names) DO
      IF Text.Equal(day, Names[i]) THEN RETURN i END
    END;
    RAISE IllegalFormat
  END ParseDay;

PROCEDURE ParseMon(mon : TEXT) : Date.Month RAISES { IllegalFormat } =
  CONST
    Names = ARRAY Date.Month OF TEXT {
    "Jan", "Feb", "Mar", "Apr", "May", "Jun", 
    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
    };
  BEGIN
    FOR i := FIRST(Names) TO LAST(Names) DO
      IF Text.Equal(mon, Names[i]) THEN RETURN i END
    END;
    RAISE IllegalFormat
  END ParseMon;

PROCEDURE Parse(tz : TZ.T; ascTime : TEXT; ripOutTZ : BOOLEAN) : Time.T
  RAISES { IllegalFormat } =
  VAR time : Time.T;
      d : Date.T;
  BEGIN
  (*      Mon Nov 24 18:22:48 1986    *)
  (*      Mon Nov 24 18:22:48 PDT 1986    *)
  (*      0    0    1    1    2    2  *)
  (*      01234567890123456789012345  *)

    IF ripOutTZ THEN 
      IF Text.Length(ascTime) # 28 THEN RAISE IllegalFormat END;
      WITH beginning = Text.Sub(ascTime, 0, 19),
           end       = Text.Sub(ascTime, 23, 5) DO
        ascTime := beginning & end
      END
    END;

    TRY
      WITH t = ascTime,
           day = ParseDay(Text.Sub(t,  0, 3)),
           mon = ParseMon(Text.Sub(t,  4, 3)),
           dte = Scan.Int(Text.Sub(t,  8, 2)),
           hou = Scan.Int(Text.Sub(t, 11, 2)),
           min = Scan.Int(Text.Sub(t, 14, 2)),
           sec = Scan.Int(Text.Sub(t, 17, 2)),
           yer = Scan.Int(Text.Sub(t, 20, 4)) DO
        d := Date.T { yer, mon, dte, hou, min, sec, 
                      offset := 0, (* ignored *)
                      zone := "",  (* ignored *)
                      weekDay := Date.WeekDay.Mon (* ignored *) };
        time := tz.mktime(d);
        IF tz.localtime(time).weekDay # day THEN RAISE IllegalFormat END;
        RETURN time
      END
    EXCEPT
      FloatMode.Trap, Lex.Error => RAISE IllegalFormat
    END
  END Parse;

BEGIN END AscTimeParse.
