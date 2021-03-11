(* $Id$  *)
(* vim: tabstop=2:expandtab:shiftwidth=2 
*)

INTERFACE Config;

TYPE OptionType = {boolean, integer, string};
TYPE T = REF RECORD
  option : TEXT;
  id : TEXT;
  type : OptionType;
END;

PROCEDURE DefineFlag(flag, id : TEXT; type : OptionType := OptionType.boolean);
PROCEDURE CheckInput(usageMsg : TEXT := "");
PROCEDURE ReadInFlags() RAISES {UsageError};
PROCEDURE IsSet(key : TEXT) : BOOLEAN;
PROCEDURE Set(key : TEXT) : BOOLEAN;
PROCEDURE IntVal(key : TEXT) : INTEGER;
PROCEDURE StringVal(key : TEXT) : TEXT;
PROCEDURE LastIndex() : CARDINAL;

PROCEDURE Equal(k1, k2 : T) : BOOLEAN;
CONST Brand = "Config";
EXCEPTION UsageError(TEXT);
END Config.
