(* $Id$ *)

UNSAFE MODULE UCTime;
IMPORT XTime AS Time;
IMPORT UtimeOpsC;
IMPORT M3toC;
FROM Ctypes IMPORT char_star, long_star;
IMPORT Text;
IMPORT UtimeWrap;

PROCEDURE ctime(clock : Time.T; keepNL, showTZ : BOOLEAN) : TEXT =
  VAR
    clockbuff : ARRAY [0..4] OF INTEGER; (* portable, hopefully... *)
    buff : ARRAY [0..25] OF CHAR;
    tm := UtimeWrap.make_T();
  BEGIN
    TRY
    UtimeOpsC.write_double_clock(clock,ADR(clockbuff));
    WITH clockP = LOOPHOLE(ADR(clockbuff), long_star),
         
         ct = M3toC.CopyStoT(UtimeOpsC.ctime_r(clockP,
                                            LOOPHOLE(ADR(buff), char_star))),
         noNL = Text.Sub(ct, 0, Text.Length(ct)-1) DO
      IF showTZ THEN
        WITH locl = UtimeOpsC.localtime_r(clock, tm),
             tzName = M3toC.CopyStoT(UtimeOpsC.Get_zone(locl)) DO
          IF keepNL THEN
            RETURN noNL & " " & tzName & "\n"
          ELSE
            RETURN noNL & " " & tzName 
          END
        END
      ELSE
        IF keepNL THEN
          RETURN ct
        ELSE
          RETURN noNL
        END
      END
    END
    FINALLY
      UtimeWrap.delete_T(tm)
    END
  END ctime;

BEGIN END UCTime.
