GENERIC INTERFACE UnitDatabase(R,UU,UUList,CUList);

IMPORT PhysicalUnit AS U;

TYPE
  (* A database of units is a list of physical measures
     where each one holds an array of (prefixed)units for different scales *)
  T =
    RECORD
      first : UUList.T := NIL;
    END;

TYPE
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

(*scales must be given in increasing order!*)
PROCEDURE AddUnit(VAR db:T;
                  READONLY unit:ARRAY OF U.ExpType;
                  flags:=UU.FlagSet{};
                  READONLY scales:ScaledUnitInitArray);

PROCEDURE DecomposeUnit(READONLY db:T;unit:U.T):CUList.T;
PROCEDURE ComposeUnit(cu:CUList.T):U.T;

END UnitDatabase.
