GENERIC INTERFACE PhysicalValueFmtLex(CF, PV, DB);
(*Copyright (c) 1996, m3na project*)

(*==========================*)
TYPE T = PV.T;

TYPE
  FmtStyle = RECORD
               unitDataBase: DB.T;
               elemStyle            := CF.FmtStyle{};
             END;

PROCEDURE Fmt (READONLY x: T; READONLY style: FmtStyle): TEXT;

(*==========================*)
END PhysicalValueFmtLex.
