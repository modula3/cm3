(*$$MC68881:=TRUE *)
(*$$Symbols:=TRUE *)
(* $$LargeCode:=TRUE *)
MODULE SIUnit;

IMPORT LongRealTrans AS RT,
       LongRealUnitDatabase AS DB,
       LongRealUsualUnit AS UU;


PROCEDURE CreateDatabase():DB.T=
VAR
  db:DB.T;

CONST
  sixty     = 60.0D0;
  daySecs   = sixty*sixty*24.0D0;  (*86400.0D0*)
  yearSecs  = daySecs*365.2422D0;
  earthAcc  = 9.80665D0;
  K2        = 1024.0D0;
  deg180    = 180.0D0;
  radPerDeg = RT.Pi/deg180;
  bytesize  = 8.0D0;

  pico  = 1.0D-12;
  nano  = 1.0D-9;
  micro = 1.0D-6;
  milli = 1.0D-3;
  centi = 1.0D-2;
  deci  = 1.0D-1;
  one   = 1.0D0;
(*deca  = 1.0D1;*)
  hecto = 1.0D2;
  kilo  = 1.0D3;
  mega  = 1.0D6;
  giga  = 1.0D9;

TYPE
  SUA     = DB.ScaledUnitInitArray;
  SU      = DB.ScaledUnitInit;

CONST
  isUnit      = DB.ScaledUnitFlagSet{DB.ScaledUnitFlags.isUnit};
  defScale    = DB.ScaledUnitFlagSet{DB.ScaledUnitFlags.default};
  independent = UU.FlagSet{UU.Flags.independent};

BEGIN
  DB.AddUnit(db,voltage,scales:=SUA{
    SU{"pi",    RT.Pi},
    SU{"e",     RT.E},
(*    SU{"i",     0.,1.}, *)
    SU{"%",     0.01D0},

    SU{"¼",     0.25D0},
    SU{"½",     0.50D0},
    SU{"¾",     0.75D0}
  });

  DB.AddUnit(db,voltage,scales:=SUA{
    SU{"mV",    milli,                    flags := isUnit},
    SU{"V",     one,                      flags := isUnit+defScale},
    SU{"kV",    kilo,                     flags := isUnit},
    SU{"MV",    mega,                     flags := isUnit},
    SU{"GV",    giga,                     flags := isUnit}
  });

  DB.AddUnit(db,angle,scales:=SUA{
    SU{"''",    radPerDeg/(sixty*sixty),  flags := isUnit},
    SU{"'",     radPerDeg/(sixty),        flags := isUnit},
    SU{"grad",  RT.Pi/200.0D0},
    SU{"°",     radPerDeg,                flags := isUnit+defScale},
    SU{"rad",   one}
  });

  DB.AddUnit(db,time,scales:=SUA{
    SU{"us",    micro,                    flags := isUnit},
    SU{"ms",    milli,                    flags := isUnit},
    SU{"s",     one,                      flags := isUnit+defScale},
    SU{"min",   sixty,                    flags := isUnit},
    SU{"h",     sixty*sixty,              flags := isUnit},
    SU{"d",     daySecs,                  flags := isUnit},
    SU{"a",     yearSecs,                 flags := isUnit}
  });

  DB.AddUnit(db,frequency,flags:=independent,scales:=SUA{
    SU{"bpm",   one/sixty},
    SU{"Hz",    one,                      flags := isUnit+defScale},
    SU{"kHz",   kilo,                     flags := isUnit},
    SU{"MHz",   mega,                     flags := isUnit},
    SU{"GHz",   giga,                     flags := isUnit}
  });

  DB.AddUnit(db,length,scales:=SUA{
    SU{"nm",    nano,                     flags := isUnit},
    SU{"um",    micro,                    flags := isUnit},
    SU{"mm",    milli,                    flags := isUnit},
    SU{"cm",    centi,                    flags := isUnit},
    SU{"dm",    deci,                     flags := isUnit},
    SU{"m",     one,                      flags := isUnit+defScale},
    SU{"km",    kilo,                     flags := isUnit}
  });

  DB.AddUnit(db,area,scales:=SUA{
    SU{"ha",    hecto*hecto}
  });

  DB.AddUnit(db,volume,scales:=SUA{
    SU{"ml",    milli*milli},
    SU{"cl",    milli*centi},
    SU{"l",     milli}
  });

  DB.AddUnit(db,speed,scales:=SUA{
    SU{"mach",  332.0D0},
    SU{"c",     299792458.0D0}
  });

  DB.AddUnit(db,acceleration,scales:=SUA{
    SU{"G",     earthAcc}
  });

  DB.AddUnit(db,mass,scales:=SUA{
    SU{"ug",    nano,                     flags := isUnit},
    SU{"mg",    micro,                    flags := isUnit},
    SU{"g",     milli,                    flags := isUnit},
    SU{"kg",    one,                      flags := isUnit+defScale},
    SU{"dt",    hecto,                    flags := isUnit},
    SU{"t",     kilo,                     flags := isUnit},
    SU{"kt",    mega,                     flags := isUnit}
  });

  DB.AddUnit(db,force,scales:=SUA{
    SU{"N",     one,                      flags := isUnit+defScale},
    SU{"kp",    earthAcc},
    SU{"kN",    kilo,                     flags := isUnit}
  });

  DB.AddUnit(db,pressure,scales:=SUA{
    SU{"Pa",    one,                      flags := isUnit+defScale},
    SU{"mbar",  hecto},
    SU{"kPa",   kilo,                     flags := isUnit},
    SU{"bar",   hecto*kilo}
  });

  DB.AddUnit(db,energy,scales:=SUA{
    SU{"eV",    1.602D-19},
    SU{"J",     one,                      flags := isUnit+defScale},
    SU{"cal",   4.19D0},
    SU{"kJ",    kilo,                     flags := isUnit},
    SU{"kcal",  4190.0D0}
  });

  DB.AddUnit(db,power,scales:=SUA{
    SU{"mW",    milli,                    flags := isUnit},
    SU{"W",     one,                      flags := isUnit+defScale},
    SU{"HP",    736.0D0},
    SU{"kW",    kilo,                     flags := isUnit}
  });

  DB.AddUnit(db,charge,scales:=SUA{
    SU{"C",     one,                      flags := isUnit+defScale}
  });

  DB.AddUnit(db,current,scales:=SUA{
    SU{"uA",    micro,                    flags := isUnit},
    SU{"mA",    milli,                    flags := isUnit},
    SU{"A",     one,                      flags := isUnit+defScale}
  });

  DB.AddUnit(db,resistance,scales:=SUA{
    SU{"Ohm",   one,                      flags := isUnit+defScale},
    SU{"kOhm",  kilo,                     flags := isUnit},
    SU{"MOhm",  mega,                     flags := isUnit}
  });

  DB.AddUnit(db,capacity,scales:=SUA{
    SU{"pF",    pico,                     flags := isUnit},
    SU{"nF",    nano,                     flags := isUnit},
    SU{"uF",    micro,                    flags := isUnit+defScale}
  });

  DB.AddUnit(db,temperature,scales:=SUA{
    SU{"K",     one,                      flags := isUnit+defScale}
  });

  DB.AddUnit(db,information,scales:=SUA{
    SU{"bit",   one,                      flags := isUnit+defScale},
    SU{"B",     bytesize,                 flags := isUnit},
    SU{"kB",    kilo*bytesize},
    SU{"KB",    K2*bytesize,              flags := isUnit},
    SU{"MB",    K2*K2*bytesize,           flags := isUnit},
    SU{"GB",    K2*K2*K2*bytesize,        flags := isUnit}
  });

  DB.AddUnit(db,datarate,flags:=independent,scales:=SUA{
    SU{"baud",  one,                      flags := isUnit+defScale},
    SU{"kbaud", kilo},
    SU{"Kbaud", K2,                       flags := isUnit},
    SU{"Mbaud", K2*K2,                    flags := isUnit},
    SU{"Gbaud", K2*K2*K2,                 flags := isUnit}
  });

  RETURN db;
END CreateDatabase;

BEGIN
END SIUnit.
