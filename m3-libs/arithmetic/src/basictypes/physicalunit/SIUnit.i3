INTERFACE SIUnit;

IMPORT LongRealUnitDatabase AS DB,
       PhysicalUnit AS U;

(* Default values added by AddDefaultConstants *)
TYPE
  SIUnit = ARRAY OF U.ExpType;

CONST
  (*                     U  t  s  m     T     *)
  (*                     V  s  m kg  °  K bit *)
  noUnit       = SIUnit{ 0, 0, 0, 0, 0, 0, 0};
  angle        = SIUnit{ 0, 0, 0, 0, 1, 0, 0};
  time         = SIUnit{ 0, 1, 0, 0, 0, 0, 0};
  frequency    = SIUnit{ 0,-1, 0, 0, 0, 0, 0};
  length       = SIUnit{ 0, 0, 1, 0, 0, 0, 0};
  area         = SIUnit{ 0, 0, 2, 0, 0, 0, 0};
  volume       = SIUnit{ 0, 0, 3, 0, 0, 0, 0};
  speed        = SIUnit{ 0,-1, 1, 0, 0, 0, 0};
  acceleration = SIUnit{ 0,-2, 1, 0, 0, 0, 0};
  mass         = SIUnit{ 0, 0, 0, 1, 0, 0, 0};
  force        = SIUnit{ 0,-2, 1, 1, 0, 0, 0};
  pressure     = SIUnit{ 0,-2,-1, 1, 0, 0, 0};
  energy       = SIUnit{ 0,-2, 2, 1, 0, 0, 0};
  power        = SIUnit{ 0,-3, 2, 1, 0, 0, 0};
  charge       = SIUnit{ 1, 0, 0, 0, 0, 0, 0};
  current      = SIUnit{ 1,-1, 0, 0, 0, 0, 0};
  voltage      = SIUnit{-1,-2, 2, 1, 0, 0, 0};
  resistance   = SIUnit{-2,-1, 2, 1, 0, 0, 0};
  capacity     = SIUnit{ 2, 2,-2,-1, 0, 0, 0};
  temperature  = SIUnit{ 0, 0, 0, 0, 0, 1, 0};
  information  = SIUnit{ 0, 0, 0, 0, 0, 0, 1};
  datarate     = SIUnit{ 0,-1, 0, 0, 0, 0, 1};

PROCEDURE CreateDatabase():DB.T;

END SIUnit.
