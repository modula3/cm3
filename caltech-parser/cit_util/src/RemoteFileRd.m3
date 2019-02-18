(* $Id$ *)

MODULE RemoteFileRd;
IMPORT Pathname, Rd, OSError, ProcUtils, Text, FileRd, TextRd;

PROCEDURE Open(p : Pathname.T) : Rd.T RAISES { OSError.E } =
  BEGIN
    WITH idx = Text.FindChar(p, ':') DO
      IF idx = -1 THEN 
        RETURN FileRd.Open(p) 
      ELSE
        VAR rd : Rd.T; BEGIN
          WITH remSpec = Text.Sub(p, 0, idx),
               remFile = Text.Sub(p, idx + 1),
               cmd = "ssh " & remSpec & " cat '" &  remFile & "'",
               completion =  ProcUtils.RunText(cmd,
                                            stdin := ProcUtils.ReadHere(NEW(TextRd.T).init("")),
                                            stderr := ProcUtils.Stderr(),
                                            stdout := ProcUtils.GimmeRd(rd)) DO
            (* really should spawn a reaper here? *) 
            RETURN rd
          END
        END
      END
    END
  END Open;

BEGIN END RemoteFileRd.
