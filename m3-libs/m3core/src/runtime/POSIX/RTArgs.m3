(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Last modified on Fri Nov 18 16:00:54 PST 1994 by kalsow     *)
(*      modified on Fri May  7 22:31:40 PDT 1993 by mjordan    *)

(* On Posix systems, "envp" points at a NIL-terminated list of
   string pointers *)

UNSAFE MODULE RTArgs;

IMPORT RTLinker, Ctypes, M3toC;

VAR env_c : CARDINAL := 0;

PROCEDURE ArgC (): CARDINAL =
  BEGIN
    RETURN RTLinker.info.argc;
  END ArgC;

PROCEDURE GetArg (n: CARDINAL): TEXT =
  VAR p: Ctypes.char_star_star := RTLinker.info.argv + n * ADRSIZE (ADDRESS);
      a: ARRAY [0..1] OF INTEGER;
  BEGIN
    IF (n >= RTLinker.info.argc) THEN
      n := 2;  n := a[n];  (* force a subscript fault *)
    END;
    RETURN M3toC.StoT (p^);
  END GetArg;

PROCEDURE EnvC (): CARDINAL =
  VAR
    cnt  : CARDINAL := 0;
    envp : Ctypes.char_star_star := RTLinker.info.envp;
  BEGIN
    IF (env_c = 0) THEN
      WHILE envp^ # NIL DO
        INC (envp, ADRSIZE (ADDRESS));
        INC (cnt);
      END;
      env_c := cnt;
    END;
    RETURN env_c;
  END EnvC;

PROCEDURE GetEnv (n: CARDINAL): TEXT =
  VAR p: Ctypes.char_star_star := RTLinker.info.envp + n * ADRSIZE (ADDRESS);
      a: ARRAY [0..1] OF INTEGER;
  BEGIN
    IF (n >= EnvC ()) THEN
      n := 2;  n := a[n];  (* force a subscript fault *)
    END;
    RETURN M3toC.StoT (p^);
  END GetEnv;

BEGIN
END RTArgs.
