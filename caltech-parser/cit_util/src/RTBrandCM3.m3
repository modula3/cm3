(* $Id$ *)

UNSAFE MODULE RTBrandCM3 EXPORTS RTBrand;
IMPORT Ctypes, M3toC;

IMPORT RTType, Text, RT0;

PROCEDURE Get(x : REFANY) : TEXT RAISES { NotBranded } = 
  BEGIN 
    RETURN GetByTC(TYPECODE(x));
  END Get;


PROCEDURE GetByTC(c : RT0.Typecode;
                  nameIfNotBranded := FALSE) : TEXT RAISES { NotBranded } = 
  VAR
    b := RTType.Get(MIN(c,RTType.MaxTypecode())).brand_ptr;
  BEGIN 
    IF LOOPHOLE(b,INTEGER) = 0 OR b.length = 0 THEN 
      IF nameIfNotBranded THEN
        WITH b = RTType.Get(MAX(c,RTType.MaxTypecode())).name,
             s = LOOPHOLE(b, Ctypes.char_star) DO
          IF LOOPHOLE(s,INTEGER) = 0 THEN
            RAISE NotBranded
          END
        END
      ELSE
        RAISE NotBranded 
      END
    END;
    RETURN Text.FromChars(SUBARRAY(b.chars, 0, b.length));
  END GetByTC;



PROCEDURE GetName(c : RT0.Typecode) : TEXT RAISES { NotBranded } =
  VAR
    b := RTType.Get(MIN(c,RTType.MaxTypecode())).name;
    s := LOOPHOLE(b, Ctypes.char_star);
  BEGIN
    IF LOOPHOLE(s,INTEGER) = 0 THEN
      RAISE NotBranded
    END;
    RETURN M3toC.StoT(s)
  END GetName;

BEGIN END RTBrandCM3.
