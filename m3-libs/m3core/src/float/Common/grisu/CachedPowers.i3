(* Copyright (C) 2016 Peter McKinna. All rights reserved. *)
(* See file COPYRIGHT-BSD for details. *)

INTERFACE CachedPowers;

FROM SimFP IMPORT GFP;

PROCEDURE GetCachedPowerForBinaryExponentRange(
    min_exponent, max_exponent : INTEGER;
    VAR power : GFP;
    VAR decimal_exponent : INTEGER);

PROCEDURE GetCachedPowerForDecimalExponent(
    requested_exponent : INTEGER;
    VAR power : GFP;
    VAR found_exponent : INTEGER);
    
END CachedPowers.