GENERIC MODULE SIUnit(RT,UU,UUList,DB);

(*
  I have collected some more units
  that are not suited for output
  but may be used for input in future
*)

PROCEDURE CreateDatabase():DB.T=
VAR
  db:DB.T;

CONST
  percent      = FLOAT(0.01D0,RT.T);
  fourth       = FLOAT(0.25D0,RT.T);
  half         = FLOAT(0.50D0,RT.T);
  threefourth  = FLOAT(0.75D0,RT.T);

  sixty        = FLOAT(60.0D0,RT.T);
  daySecs      = sixty*sixty*FLOAT(24.0D0,RT.T);  (* 86400.0D0 *)
  yearSecs     = daySecs*FLOAT(365.2422D0,RT.T);
  earthAcc     = FLOAT(9.80665D0,RT.T);
  K2           = FLOAT(1024.0D0,RT.T);
  deg180       = FLOAT(180.0D0,RT.T);
  grad200      = FLOAT(200.0D0,RT.T);
  radPerDeg    = RT.Pi/deg180;
  radPerGrad   = RT.Pi/grad200;
  bytesize     = FLOAT(8.0D0,RT.T);

  mach         = FLOAT(332.0D0,RT.T);
  speedoflight = FLOAT(299792458.0D0,RT.T);
  electronVolt = FLOAT(1.602D-19,RT.T);
  calorien     = FLOAT(4.19D0,RT.T);
  horsePower   = FLOAT(736.0D0,RT.T);

  pico  = FLOAT(1.0D-12,RT.T);
  nano  = FLOAT(1.0D-9,RT.T);
  micro = FLOAT(1.0D-6,RT.T);
  milli = FLOAT(1.0D-3,RT.T);
  centi = FLOAT(1.0D-2,RT.T);
  deci  = FLOAT(1.0D-1,RT.T);
  one   = FLOAT(1.0D0,RT.T);
(* deca  = FLOAT(1.0D1,RT.T); *)
  hecto = FLOAT(1.0D2,RT.T);
  kilo  = FLOAT(1.0D3,RT.T);
  mega  = FLOAT(1.0D6,RT.T);
  giga  = FLOAT(1.0D9,RT.T);

TYPE
  SUA     = DB.ScaledUnitInitArray;
  SU      = DB.ScaledUnitInit;

CONST
  IsUnit      = DB.ScaledUnitFlagSet{DB.ScaledUnitFlags.IsUnit};
  DefScale    = DB.ScaledUnitFlagSet{DB.ScaledUnitFlags.Default};
  Independent = UU.FlagSet{UU.Flags.Independent};

BEGIN
  DB.AddUnit(db,noUnit,scales:=SUA{
    SU{"pi",    RT.Pi},
    SU{"e",     RT.E},
(*    SU{"i",     0.,1.}, *)
    SU{"%",     percent},

    SU{"¼",     fourth},
    SU{"½",     half},
    SU{"¾",     threefourth}
  });

  DB.AddUnit(db,angle,scales:=SUA{
    SU{"''",    radPerDeg/(sixty*sixty),  flags := IsUnit},
    SU{"'",     radPerDeg/(sixty),        flags := IsUnit},
    SU{"grad",  radPerGrad},
    SU{"°",     radPerDeg,                flags := IsUnit+DefScale},
    SU{"rad",   one}
  });

  DB.AddUnit(db,frequency,flags:=Independent,scales:=SUA{
    SU{"bpm",   one/sixty},
    SU{"Hz",    one,                      flags := IsUnit+DefScale},
    SU{"kHz",   kilo,                     flags := IsUnit},
    SU{"MHz",   mega,                     flags := IsUnit},
    SU{"GHz",   giga,                     flags := IsUnit}
  });

  DB.AddUnit(db,time,scales:=SUA{
    SU{"us",    micro,                    flags := IsUnit},
    SU{"ms",    milli,                    flags := IsUnit},
    SU{"s",     one,                      flags := IsUnit+DefScale},
    SU{"min",   sixty,                    flags := IsUnit},
    SU{"h",     sixty*sixty,              flags := IsUnit},
    SU{"d",     daySecs,                  flags := IsUnit},
    SU{"a",     yearSecs,                 flags := IsUnit}
  });

  DB.AddUnit(db,length,scales:=SUA{
    SU{"nm",    nano,                     flags := IsUnit},
    SU{"um",    micro,                    flags := IsUnit},
    SU{"mm",    milli,                    flags := IsUnit},
    SU{"cm",    centi,                    flags := IsUnit},
    SU{"dm",    deci,                     flags := IsUnit},
    SU{"m",     one,                      flags := IsUnit+DefScale},
    SU{"km",    kilo,                     flags := IsUnit}
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
    SU{"mach",  mach},
    SU{"c",     speedoflight}
  });

  DB.AddUnit(db,acceleration,scales:=SUA{
    SU{"G",     earthAcc}
  });

  DB.AddUnit(db,mass,scales:=SUA{
    SU{"ug",    nano,                     flags := IsUnit},
    SU{"mg",    micro,                    flags := IsUnit},
    SU{"g",     milli,                    flags := IsUnit},
    SU{"kg",    one,                      flags := IsUnit+DefScale},
    SU{"dt",    hecto,                    flags := IsUnit},
    SU{"t",     kilo,                     flags := IsUnit},
    SU{"kt",    mega,                     flags := IsUnit}
  });

  DB.AddUnit(db,force,scales:=SUA{
    SU{"N",     one,                      flags := IsUnit+DefScale},
    SU{"kp",    earthAcc},
    SU{"kN",    kilo,                     flags := IsUnit}
  });

  DB.AddUnit(db,pressure,scales:=SUA{
    SU{"Pa",    one,                      flags := IsUnit+DefScale},
    SU{"mbar",  hecto},
    SU{"kPa",   kilo,                     flags := IsUnit},
    SU{"bar",   hecto*kilo}
  });

  DB.AddUnit(db,energy,scales:=SUA{
    SU{"eV",    electronVolt},
    SU{"J",     one,                      flags := IsUnit+DefScale},
    SU{"cal",   calorien},
    SU{"kJ",    kilo,                     flags := IsUnit},
    SU{"kcal",  kilo*calorien}
  });

  DB.AddUnit(db,power,scales:=SUA{
    SU{"mW",    milli,                    flags := IsUnit},
    SU{"W",     one,                      flags := IsUnit+DefScale},
    SU{"HP",    horsePower},
    SU{"kW",    kilo,                     flags := IsUnit}
  });

  DB.AddUnit(db,charge,scales:=SUA{
    SU{"C",     one,                      flags := IsUnit+DefScale}
  });

  DB.AddUnit(db,current,scales:=SUA{
    SU{"uA",    micro,                    flags := IsUnit},
    SU{"mA",    milli,                    flags := IsUnit},
    SU{"A",     one,                      flags := IsUnit+DefScale}
  });

  DB.AddUnit(db,voltage,scales:=SUA{
    SU{"mV",    milli,                    flags := IsUnit},
    SU{"V",     one,                      flags := IsUnit+DefScale},
    SU{"kV",    kilo,                     flags := IsUnit},
    SU{"MV",    mega,                     flags := IsUnit},
    SU{"GV",    giga,                     flags := IsUnit}
  });

  DB.AddUnit(db,resistance,scales:=SUA{
    SU{"Ohm",   one,                      flags := IsUnit+DefScale},
    SU{"kOhm",  kilo,                     flags := IsUnit},
    SU{"MOhm",  mega,                     flags := IsUnit}
  });

  DB.AddUnit(db,capacity,scales:=SUA{
    SU{"pF",    pico,                     flags := IsUnit},
    SU{"nF",    nano,                     flags := IsUnit},
    SU{"uF",    micro,                    flags := IsUnit+DefScale}
  });

  DB.AddUnit(db,temperature,scales:=SUA{
    SU{"K",     one,                      flags := IsUnit+DefScale}
  });

  DB.AddUnit(db,information,scales:=SUA{
    SU{"bit",   one,                      flags := IsUnit+DefScale},
    SU{"B",     bytesize,                 flags := IsUnit},
    SU{"kB",    kilo*bytesize},
    SU{"KB",    K2*bytesize,              flags := IsUnit},
    SU{"MB",    K2*K2*bytesize,           flags := IsUnit},
    SU{"GB",    K2*K2*K2*bytesize,        flags := IsUnit}
  });

  DB.AddUnit(db,datarate,flags:=Independent,scales:=SUA{
    SU{"baud",  one,                      flags := IsUnit+DefScale},
    SU{"kbaud", kilo},
    SU{"Kbaud", K2,                       flags := IsUnit},
    SU{"Mbaud", K2*K2,                    flags := IsUnit},
    SU{"Gbaud", K2*K2*K2,                 flags := IsUnit}
  });

  (* put the list in the right order *)
  db.first := UUList.ReverseD(db.first);
  RETURN db;
END CreateDatabase;

BEGIN
END SIUnit.
