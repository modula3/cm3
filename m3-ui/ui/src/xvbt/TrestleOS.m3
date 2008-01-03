(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman, Mark Manasse and Greg Nelson           *)
(* Last modified on Fri Oct 29 16:19:01 PDT 1993 by sfreeman   *)
(*      modified on Tue May 18 10:10:34 PDT 1993 by msm        *)
(*      modified on Fri Apr 16 09:48:00 PDT 1993 by steveg     *)

UNSAFE MODULE TrestleOS;

IMPORT XClient, Uutmp, Uugid, Upwd, M3toC, Env;

VAR mu := NEW(MUTEX);
    inited := FALSE;

PROCEDURE Init () =
  BEGIN
    LOCK mu DO IF NOT inited THEN XClient.Init(); inited := TRUE END END
  END Init;

PROCEDURE UserName (): TEXT =
  VAR res: TEXT;
  BEGIN
    res := Env.Get("USER");
    IF res = NIL THEN
      VAR logname := Uutmp.getlogin();
      BEGIN
        IF logname # NIL THEN
          res := M3toC.CopyStoT(logname)
        ELSE
          VAR pwent := Upwd.getpwuid(Uugid.geteuid());
          BEGIN
            IF pwent # NIL THEN
              res := M3toC.CopyStoT(pwent.pw_name)
            ELSE
              res := "Unknown user"
            END
          END
        END
      END
    END;
    RETURN res
  END UserName;

BEGIN
END TrestleOS.
