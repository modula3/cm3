(* $Id: RTBrand.m3,v 1.2 2001-09-19 14:07:43 wagner Exp $ *)

UNSAFE MODULE RTBrand;

IMPORT (* M3toC, *) RT0, RTType, Ctypes, Text8CString;

PROCEDURE Get(x : REFANY) : TEXT RAISES { NotBranded } = 
  VAR
    b := RTType.Get(TYPECODE(x)).brand_ptr;
    s := LOOPHOLE(b, Ctypes.char_star);
  BEGIN 
    IF LOOPHOLE(s,INTEGER) = 0 THEN RAISE NotBranded END;
    (* RETURN M3toC.StoT(s) *)
    RETURN Text8CString.New(s);
  END Get;


PROCEDURE GetByTC(c : RT0.Typecode) : TEXT RAISES { NotBranded } = 
  VAR
    b := RTType.Get(c).brand_ptr;
    s := LOOPHOLE(b, Ctypes.char_star);
  BEGIN 
    IF LOOPHOLE(s,INTEGER) = 0 THEN RAISE NotBranded END;
    (* RETURN M3toC.StoT(s) *)
    RETURN Text8CString.New(s);
  END GetByTC;

BEGIN END RTBrand.
