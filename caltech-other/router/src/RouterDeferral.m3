(* $Id$ *)

MODULE RouterDeferral;
FROM EndPointStatus IMPORT Dir;
IMPORT EndPointStatus;
IMPORT RouterDeferralClass;

REVEAL
  T = RouterDeferralClass.Private BRANDED Brand OBJECT END;

PROCEDURE Equal(a, b : T) : BOOLEAN =
  BEGIN
    (* ho hum... is this labels thing really right? *)
    RETURN a.labels = b.labels AND a.exitDirs = b.exitDirs
  END Equal;

PROCEDURE FormatExitDirs(d : SET OF Dir) : TEXT =
  VAR 
    res := "";
  BEGIN
    FOR i := FIRST(Dir) TO LAST(Dir) DO
      IF i IN d THEN
        res := res & EndPointStatus.DirName[i]
      END
    END;
    RETURN res
  END FormatExitDirs;

BEGIN END RouterDeferral.
