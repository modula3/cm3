(* Copyright (C) 1992, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* by Steve Glassman, Mark Manasse and Greg Nelson           *)
(* Last modified on Wed Mar 17 10:24:27 PST 1993 by mjordan  *)
(*      modified on Wed Dec  2 08:45:36 PST 1992 by mcjones  *)
(*      modified on Mon Feb 24 13:56:57 PST 1992 by muller   *)
(*      modified on Wed Sep 11 15:22:39 PDT 1991 by msm      *)
<*PRAGMA LL*>

UNSAFE MODULE Env;

IMPORT Cstdlib, M3toC, RTArgs, Text;

PROCEDURE Get(nm: TEXT): TEXT =
  VAR cRes := Cstdlib.getenv(M3toC.TtoS(nm));
  BEGIN
    IF cRes = NIL THEN
      RETURN NIL
    ELSE
      RETURN M3toC.CopyStoT(cRes)
    END
  END Get;

EXCEPTION FatalError; <* FATAL FatalError *>

PROCEDURE GetNth(n: CARDINAL; VAR (*OUT*) nm, val: TEXT) =
  VAR
    env: TEXT;
    i: CARDINAL;
  BEGIN
    IF n >= Count THEN RAISE FatalError END;
    env := RTArgs.GetEnv(n);
    i := 0;
    WHILE Text.GetChar(env, i) # '=' DO INC(i) END;
    nm := Text.Sub(env, 0, i);
    val := Text.Sub(env, i + 1, Text.Length(env)-(i+1));
  END GetNth;

BEGIN
  Count := RTArgs.EnvC();
END Env.










