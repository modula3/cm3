(* Copyright 1989 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* Util.m3 *)
(* Last modified on Fri Apr 23 14:33:28 PDT 1993 by wobber  *)
(*      modified on Fri Jul  3  9:28:16 GMT+2:00 1992 by prusker *)

MODULE Util;

IMPORT Param, ServerLog, Text, Time, Fmt;

    (* returns a unique cardinal *)
VAR
  unique: CARDINAL := 0;
  uniqueMutex: MUTEX;

PROCEDURE Unique(): CARDINAL =
  BEGIN
    LOCK uniqueMutex DO
      INC(unique);
      RETURN unique;
    END;
  END Unique;


    (* date and duration *)
    
TYPE
  E = RECORD
        str:   Text.T;
        inter: CARDINAL;
      END;
  
VAR
  tab := ARRAY [0..3] OF E {
      E{" day",  86400},
      E{" hour", 3600},
      E{" min",  60},
      E{" sec",  1}};

PROCEDURE Interval(t: Time.T) : TEXT =
  VAR
    n: INTEGER := ROUND(t);
    res: TEXT;
  BEGIN
    IF n <= 0 THEN RETURN "now"; END;
    FOR i := 0 TO LAST(tab) DO
      WITH e = tab[i] DO
        IF n >= e.inter THEN
          n := n DIV e.inter;
          res := Fmt.Int(n) & e.str;
          IF n # 1 THEN res := res & "s"; END;
          EXIT;
        END;
      END;
    END;
    RETURN res;
  END Interval;

PROCEDURE IntervalSince(t: Time.T) : TEXT =
  BEGIN
    RETURN Interval(Time.Now() - t);
  END IntervalSince;

PROCEDURE LogText(text: Text.T) =
  BEGIN
    ServerLog.WriteText(Param.log, text);
  END LogText;


BEGIN
  uniqueMutex := NEW(MUTEX);
END Util.
