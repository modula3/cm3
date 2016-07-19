INTERFACE DateOps;

IMPORT
  Date,
  Time;

TYPE
  T = REF Date.T;

  Language = { English, Serbian };

CONST
  DefaultMask = "D/M/Y h:i:s";

  DayName = ARRAY Language, Date.WeekDay OF TEXT {
    ARRAY Date.WeekDay OF TEXT {
      "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Sunday"
    },
    ARRAY Date.WeekDay OF TEXT {
      "Nedelja", "Ponedeljak", "Utorak", "Sreda", "Cetvrtak", "Petak", "Subota"
    }
  };

  MonthName = ARRAY Language, Date.Month OF TEXT {
    ARRAY Date.Month OF TEXT {
      "January", "February", "March", "April", "May", "June",
      "July", "August", "September", "October", "November", "December"
    },
    ARRAY Date.Month OF TEXT {
      "Januar", "Februar", "Mart", "April", "Maj", "Jun",
      "Jul", "Avgust", "Septembar", "Oktobar", "Novembar", "Decembar"
    }
  };

PROCEDURE Now(): T;

PROCEDURE ToText(t: T; mask: TEXT := DefaultMask; lang: Language := Language.Serbian): TEXT;
(* d - day,   D - day   (with leading zeros), a - day name (short),   A - day name (full)
   m - month, M - month (with leading zeros), b - month name (short), B - month name (full)
   y - year,  Y - year  (4 digits)
   h - hour
   i - minute
   s - second

*)

PROCEDURE FromText(text: TEXT): T;
(* 'text' must be formatted with DefaultMask
*)

PROCEDURE FromTime(t: Time.T): T;

PROCEDURE FromDate(d: Date.T): T;

PROCEDURE ToTime(READONLY d: T): Time.T RAISES { Date.Error };

PROCEDURE Copy(READONLY d: T): T;

END DateOps.
