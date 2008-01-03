GENERIC INTERFACE UsualUnit(R);
(*
  Data type for units that are of common use.
  Where units like meter 'm' and second 's' are base units,
  the unit 'Pa=N/m²' is a composed unit,
  but it is commonly used.
*)

IMPORT PhysicalUnit AS U;

CONST Brand = "UsualUnit";

TYPE
  ScaledUnit = RECORD
                 symbol: TEXT;
                 mag   : R.T;
               END;

  Flags = {Independent           (* don't use this unit as component of a
                                    composed unit, e.g.  Hz *)
          };
  FlagSet = SET OF Flags;

  T = RECORD
        unit  : U.T;
        scales: REF ARRAY OF ScaledUnit;
        defScale: CARDINAL;      (* index of the default scale *)
        flags   : SET OF Flags;
      END;

PROCEDURE Equal (READONLY k1, k2: T; ): BOOLEAN;

END UsualUnit.
