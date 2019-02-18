(* $Id$ *)

UNSAFE MODULE RTBrand;

IMPORT M3toC, RT0, RTType, Ctypes;

PROCEDURE Get(x : REFANY) : TEXT RAISES { NotBranded } = 
  VAR
    b := RTType.Get(TYPECODE(x)).brand;
    s := LOOPHOLE(b, Ctypes.char_star);
  BEGIN 
    IF LOOPHOLE(s,INTEGER) = 0 THEN RAISE NotBranded END;
    RETURN M3toC.StoT(s) 
  END Get;


PROCEDURE GetByTC(c : RT0.Typecode;
                  nameIfNotBranded := FALSE) : TEXT RAISES { NotBranded } = 
  VAR
    b := RTType.Get(MIN(c,RTType.MaxTypecode())).brand;
    s := LOOPHOLE(b, Ctypes.char_star);
  BEGIN 
    IF LOOPHOLE(s,INTEGER) = 0 THEN
      IF nameIfNotBranded THEN
        b := RTType.Get(c).name;
        s := LOOPHOLE(b, Ctypes.char_star);
        IF LOOPHOLE(s,INTEGER) = 0 THEN
          RAISE NotBranded
        END;
      ELSE
        RAISE NotBranded
      END;
    END;
    RETURN M3toC.StoT(s) 
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

BEGIN END RTBrand.
