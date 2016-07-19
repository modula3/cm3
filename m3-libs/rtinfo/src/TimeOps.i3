INTERFACE TimeOps;

TYPE
  T = REF RECORD
    hour: CARDINAL := 0;
    minute,
    second: [0..59] := 0;
    plus: BOOLEAN := TRUE;
  END;

PROCEDURE Add(t1, t2: T): T;

PROCEDURE Sub(t1, t2: T): T;

PROCEDURE ToSeconds(t: T): INTEGER;

PROCEDURE FromSeconds(s: INTEGER): T;

PROCEDURE ToText(t: T): TEXT;

PROCEDURE FromText(text: TEXT): T;

END TimeOps.
