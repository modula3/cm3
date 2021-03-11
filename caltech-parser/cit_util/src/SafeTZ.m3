(* $Id$ *)

MODULE SafeTZ EXPORTS TZ;
IMPORT XTime AS Time, Fmt, FinDate, Date, Debug;
IMPORT TextReader, HMTime;
IMPORT TextRefTbl;
IMPORT TextWr, Wr, Thread;
IMPORT OSError;
IMPORT DebugClass; (* force DebugClass to be initialized before us *)

PROCEDURE Floor(x : LONGREAL) : LONGREAL =
  BEGIN
    WITH y = x / 1.0d9, 
         billions = FLOOR(y), 
         z = x-FLOAT(billions,LONGREAL)*1.0d9,
         zf = FLOOR(z) DO
      RETURN FLOAT(billions,LONGREAL)*1.0d9+FLOAT(zf,LONGREAL)
    END
  END Floor;

PROCEDURE FormatSubsecond(tz                                 : T; 
                          t                                  : Time.T; 
                          prec                               : [0..6];
                          simplified, printDate, printMillis : BOOLEAN) : TEXT =
  VAR
    sub : TEXT;
  CONST 
    Thousand = 1000.0d0;

    Mult = ARRAY OF LONGREAL { 1.0d0, 
                               10.0d0, 
                               100.0d0, 
                               1.0d0 * Thousand, 
                               10.0d0 * Thousand, 
                               100.0d0 * Thousand, 
                               Thousand * Thousand};

  CONST L = Fmt.LongReal; I = Fmt.Int;
  BEGIN
    WITH tfloor = Floor(t),
         d = tz.localtime(tfloor) DO

      WITH delta = t-tfloor DO
        IF Debug.GetLevel() >= 20 THEN 
          Debug.Out(Fmt.F("SafeTZ.FormatSubsecond: t=%s, tfloor=%s, delta=%s, year=%s, ORD(month)=%s, ",
                    L(t),L(tfloor),L(delta), I(d.year), I(ORD(d.month))) &
                    Fmt.F("day=%s, hour=%s, minute=%s, second=%s", I(d.day), I(d.hour), I(d.minute), I(d.second)))


        END;

        IF    prec = 0 THEN
          sub := ""
        ELSIF delta = 0.0d0 THEN
          sub := "." & Fmt.Pad("", padChar := '0', length := prec)
        ELSE
          (* time isn't rounded but truncated... *)
          WITH floor = FLOOR(Mult[prec] * delta) DO
            <*ASSERT floor < ROUND(Mult[prec])*>
            sub := "." & 
                       Fmt.Pad(Fmt.Int(floor), padChar := '0', length := prec)
          END
        END
      END;

      <* FATAL Thread.Alerted, Wr.Failure *>
      VAR wr := NEW(TextWr.T).init(); 
      BEGIN
        IF printDate THEN
          WITH d = FinDate.T { d.year, ORD(d.month)+1, d.day} DO
            IF simplified THEN 
              Wr.PutText(wr, FinDate.FormatFIX(d));
              Wr.PutChar(wr, ' ')
            ELSE
              Wr.PutText(wr, FinDate.Format(d));
              Wr.PutChar(wr, '@')
            END
          END
        END;

        Wr.PutText(wr, Fmt.F("%02s:%02s:%02s",
                             Fmt.Int(d.hour),
                             Fmt.Int(d.minute),
                             Fmt.Int(d.second)));

        IF printMillis THEN
          Wr.PutText(wr, sub)
        END;

        RETURN TextWr.ToText(wr)
      END
    END
  END FormatSubsecond;

PROCEDURE ParseSubsecond(tz : T;
                         txt : TEXT; mode : ParseMode) : Time.T 
  RAISES { Error } =
  BEGIN
    CASE mode OF
      ParseMode.Normal => RETURN ParseNormalSubsecond(tz,txt) 
    |
      ParseMode.Simplified => RETURN ParseSimplifiedSubsecond(tz,txt)
    |
      ParseMode.Automatic =>
      TRY
        RETURN ParseNormalSubsecond(tz,txt) 
      EXCEPT
        Error => RETURN ParseSimplifiedSubsecond(tz,txt)
      END
    END
  END ParseSubsecond;

PROCEDURE ParseNormalSubsecond(tz : T; 
                               txt : TEXT) : Time.T RAISES { Error } = 
  BEGIN
    TRY
      WITH r = NEW(TextReader.T).init(txt),
           dt = r.nextE("@"), 
           tt = r.nextE(""),
           d = FinDate.Parse(dt) DO
        RETURN PP(tz,d,tt)
      END
    EXCEPT
      TextReader.NoMore, FinDate.ParseError => RAISE Error
    END
  END ParseNormalSubsecond;

PROCEDURE ParseSimplifiedSubsecond(tz : T;
                                   txt : TEXT) : Time.T RAISES { Error } = 
  BEGIN
    TRY
      WITH r = NEW(TextReader.T).init(txt),
           dt = r.nextE(" ", skipNulls := TRUE), 
           tt = r.nextE(""),
           d = FinDate.ParseFIX(dt) DO
        RETURN PP(tz,d,tt)
      END
    EXCEPT
      TextReader.NoMore, FinDate.ParseError => RAISE Error
    END
  END ParseSimplifiedSubsecond;

PROCEDURE PP(tz : T; d : FinDate.T; tt : TEXT) : Time.T RAISES { Error } =
  VAR   sub : LONGREAL;
  BEGIN
    TRY
      WITH t    = HMTime.ParseSubsecond(tt,sub),
           date = Date.T { d.year, VAL(d.month-1,Date.Month), d.day,
                           t.hour, t.minute, t.second,
                           
                           (* hopefully all the following is completely ignored*)
                           0, "", FIRST(Date.WeekDay) } DO
        RETURN tz.mktime(date) + sub
      END
    EXCEPT
      HMTime.ParseError => RAISE Error
    END
  END PP;

PROCEDURE New(tz : TEXT; disableChecking : BOOLEAN) : T RAISES { OSError.E } =
  VAR r : REFANY; BEGIN
    LOCK mu DO
      IF NOT cache.get(tz,r) THEN 
        r := NEW(T).init(tz,disableChecking); 
        EVAL cache.put(tz,r)
      END
    END;
    RETURN r
  END New;

<* FATAL Date.Error *>
VAR
  cache := NEW(TextRefTbl.Default).init();
  mu := NEW(MUTEX);

BEGIN
  DebugClass.DoInit();
  Debug.Out("Setting UnixEpoch");
  UnixEpoch := Date.ToTime( Date.T { 1970,
                                     Date.Month.Jan,
                                     1,
                                     0, 0, 0,
                                     0, "UTC", 
                                     FIRST(Date.WeekDay) });
  Debug.Out("UnixEpoch=" & Fmt.LongReal(UnixEpoch));

  (* this is crazy -- see my emails to m3devel about London's British Standard Time of 1968-1971 *)
  IF ABS(UnixEpoch) < 1.0d5 THEN UnixEpoch := 0.0d0 END;
END SafeTZ.
