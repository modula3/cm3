GENERIC INTERFACE UnitDatabase(R);

IMPORT PhysicalUnit AS U;

TYPE
  (* A database of units is a list of physical measures
     where each one holds an array of (prefixed)units for different scales *)
  T =
    RECORD
      first : UsualUnit := NIL;
    END;

  UsualUnitFlags = {
    independent   (* don't use this unit as component of a composed unit, e.g. Hz *)
    };
  UsualUnitFlagSet = SET OF UsualUnitFlags;

  UsualUnit =
    REF RECORD
      next     : UsualUnit;
      unit     : U.T;
      scales   : REF ARRAY OF ScaledUnit;
      defScale : CARDINAL; (* index of the default scale *)
      flags    : SET OF UsualUnitFlags;
    END;

  ScaledUnit =
    RECORD
      symbol : TEXT;
      mag    : R.T;
    END;

  CompositeUnit =
    REF RECORD
      next : CompositeUnit;
      uu   : UsualUnit;
      exp  : U.ExpType;
    END;


  ScaledUnitFlags = {
    isUnit,       (* use this constant for unit output,
                     if not set, this unit is ignored
                     but could be used for unit input some day *)
    default       (* this is the default scaling *)
    };
  ScaledUnitFlagSet = SET OF ScaledUnitFlags;

  ScaledUnitInit =
    RECORD
      symbol : TEXT;
      mag    : R.T;
      flags  := ScaledUnitFlagSet{};
    END;
  ScaledUnitInitArray = ARRAY OF ScaledUnitInit;

PROCEDURE AddUnit(VAR db:T;
                  READONLY unit:ARRAY OF U.ExpType;
                  flags:=UsualUnitFlagSet{};
                  READONLY scales:ScaledUnitInitArray);

PROCEDURE DecomposeUnit(READONLY db:T;unit:U.T):CompositeUnit;

END UnitDatabase.
