(* $Id: RTBrand.i3,v 1.2 2001-09-19 14:07:43 wagner Exp $ *)
INTERFACE RTBrand;
IMPORT RT0;

EXCEPTION NotBranded;

PROCEDURE Get(x : REFANY) : TEXT RAISES { NotBranded } ;
PROCEDURE GetByTC(tc : RT0.Typecode) : TEXT RAISES { NotBranded } ;
END RTBrand.
