(* $Id$ *)
INTERFACE RTBrand;
IMPORT RT0;

EXCEPTION NotBranded;

PROCEDURE Get(x : REFANY) : TEXT RAISES { NotBranded } ;
PROCEDURE GetByTC(tc : RT0.Typecode) : TEXT RAISES { NotBranded } ;
END RTBrand.
