(* $Id: RTBrand.m3,v 1.1.1.1 2001-09-19 11:40:10 wagner Exp $ *)

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


PROCEDURE GetByTC(c : RT0.Typecode) : TEXT RAISES { NotBranded } = 
  VAR
    b := RTType.Get(c).brand;
    s := LOOPHOLE(b, Ctypes.char_star);
  BEGIN 
    IF LOOPHOLE(s,INTEGER) = 0 THEN RAISE NotBranded END;
    RETURN M3toC.StoT(s) 
  END GetByTC;

BEGIN END RTBrand.
