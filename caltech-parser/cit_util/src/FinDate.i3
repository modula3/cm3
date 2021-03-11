(* $Id$ *)

INTERFACE FinDate;
IMPORT Date, Time;
IMPORT Word;

TYPE 
  Month = [1..12];
  Day = [1..31];

  T = RECORD
    year  : INTEGER;
    month : Month;
    day   : Day;
  END;

CONST LongAgo   = T { 1800, 01, 01 };
CONST FarFuture = T { 2199, 12, 31 };
  
EXCEPTION ParseError;


PROCEDURE ParseCmdLine(c : TEXT) : T RAISES { ParseError };
  (* parse from format YYYYMMDD *)


PROCEDURE Equal(READONLY a, b : T) : BOOLEAN;
PROCEDURE Compare(READONLY a, b : T) : [-1..1];
PROCEDURE Hash(READONLY a : T) : Word.T;

PROCEDURE ParseDaily(date : TEXT) : T RAISES { ParseError };
  (* parse from format DD-Mon-YY or DD-Mon-YYYY *)

PROCEDURE ParseFed(date : TEXT) : T RAISES { ParseError };
  (* parse from format YYYY-MM-DD *)

PROCEDURE ParseAmerican(date : TEXT) : T RAISES { ParseError };
  (* parse from format MM/DD/YYYY *)

PROCEDURE ParseBritish(date : TEXT) : T RAISES { ParseError };
  (* parse from format DD/MM/YYYY *)

PROCEDURE ParseJulian(date : TEXT) : T RAISES { ParseError };
  (* parse from integer or floating Julian format *)

TYPE 
  SpecificFormat = { VMS, ISO, US, UK, FIX, Julian, Permissive };

CONST
  SpecificFormatNames  = ARRAY SpecificFormat OF TEXT { "VMS", "ISO", "US", "UK", "FIX", "Julian", "Permissive" };

PROCEDURE ParseSpecificFormat(fname : TEXT) : SpecificFormat RAISES { ParseError };

TYPE SpecificParser = PROCEDURE (x : TEXT) : T RAISES { ParseError };

CONST
  SpecificParsers = ARRAY SpecificFormat OF SpecificParser { ParseDaily, ParseFed, ParseAmerican, ParseBritish, ParseCmdLine, ParseJulian, Parse };

CONST ParseFIX = ParseCmdLine;
  (* parse from format YYYYMMDD *)

PROCEDURE Parse(t : TEXT) : T RAISES { ParseError };
  (* try parsing:
       1. as integer Julian day number
       2. as floating Julian day number
       3. in Daily format, see above
       4. in Fed format, see above
       5. in CmdLine/FIX format, see above
  *)

PROCEDURE Format(READONLY d : T) : TEXT;
  (* format in format DD-Mon-YYYY *)

PROCEDURE FormatSortable(READONLY d : T) : TEXT;
  (* format in format YYYY-MM-DD *)

PROCEDURE FormatFIX(READONLY d : T) : TEXT;
  (* format in format YYYYMMDD *)

EXCEPTION OutOfRange;

PROCEDURE Julian(READONLY d : T) : CARDINAL;
  (* Julian day number valid from 1900 to...?? *)

PROCEDURE FromJulian(j : CARDINAL) : T RAISES { OutOfRange };

PROCEDURE FromMicrosoftOLE(m : CARDINAL) : T RAISES { OutOfRange };

PROCEDURE CalendarSub(READONLY a, b : T) : INTEGER;
  (* subtract Julian day numbers; value neg if b is after a *)

PROCEDURE FromTime(t : Time.T; 
                   zone : Date.TimeZone := NIL (* Local if NIL *)) : T;

PROCEDURE FromDate(d : Date.T) : T;

PROCEDURE Morning(t : T) : Date.T; 
  (* midnight this morning, producing a Date.T without the time zone
     or weekDay fields set to anything sensible.  This is suitable for
     passing to a TZ's mktime method. *)

PROCEDURE DayOfWeek(t : T) : Date.WeekDay;

PROCEDURE Today(zone : Date.TimeZone := NIL (* Local if NIL *)) : T;

PROCEDURE Yesterday(zone : Date.TimeZone := NIL (* Local if NIL *)) : T;
  (* returns previous working day (week day: M-F) *)
  
PROCEDURE Min(READONLY a, b : T) : T;

PROCEDURE Max(READONLY a, b : T) : T;

PROCEDURE Next(READONLY a : T) : T;
  (* next date in Gregorian (new style, valid in British Empire since 1752) 
     calendar.  See cal(1) manual page. *)

PROCEDURE Advance(READONLY a : T; by : CARDINAL) : T;

PROCEDURE Prev(READONLY a : T) : T;
  (* prev date in Gregorian calendar *)


TYPE
  MonthYear = RECORD  (* used only by FIX code *)
    year : INTEGER;
    month : Month;
  END;

PROCEDURE MYParseFIX(my : TEXT) : MonthYear RAISES { ParseError };
  (* parse from format YYYYMM *)

PROCEDURE MYFormatFIX(READONLY my : MonthYear) : TEXT;
  (* format as YYYYMM *)

PROCEDURE MYEqual(READONLY a, b : MonthYear) : BOOLEAN;

CONST Brand = "FinDate";

TYPE R = REF T;

CONST MonthNames = ARRAY Month OF TEXT { 
                 "Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                 "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" };


END FinDate.
