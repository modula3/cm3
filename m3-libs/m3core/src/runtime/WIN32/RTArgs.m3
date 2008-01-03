(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Last modified on Fri Jan 27 12:59:10 PST 1995 by kalsow     *)
(*      modified on Fri May  7 22:57:08 PDT 1993 by mjordan    *)

(* In Windows/NT, "envp" points at a null-terminated block of
   null-terminated strings.
   This seems to have changed recently, as the original algorithms
   of this module break for the environment calculations, while the
   standard POSIX ones work. As I don't have any reasonable criterium
   when to use the old and standard POSIX algorithms, now the environment
   variable USE_FLAT_ENV is checked to decide this.
*)

UNSAFE MODULE RTArgs;

IMPORT RTLinker, Ctypes, Cstdlib, M3toC, WinBase;

CONST
  NUL    = VAL (0, Ctypes.char);
  QUOTE  = VAL (ORD ('"'), Ctypes.char);
  BSLASH = VAL (ORD ('\134'), Ctypes.char);

VAR env_c : CARDINAL := 0;

PROCEDURE ArgC (): CARDINAL =
  BEGIN
    IF (RTLinker.argc < 0) THEN ParseArgs (); END;
    RETURN RTLinker.argc;
  END ArgC;

PROCEDURE ParseArgs () =
  (* This hack is necessary because a windows GUI program is passed
     its command line unparsed.  This code is executed before the
     runtime type system is initialized. *)
  VAR
    cp       : Ctypes.char_star := RTLinker.argv;
    n_args   : INTEGER;
    n_chars  : INTEGER;
    next     : Ctypes.char_star_star;
    argv     : Ctypes.char_star_star;
    args     : Ctypes.char_star;
    quoted   : BOOLEAN;
    fn_len   : INTEGER;
    filename : ARRAY [0..255] OF Ctypes.char;
    is_blank : ARRAY Ctypes.char OF BOOLEAN;
  BEGIN
    IF (cp = NIL) THEN
      RTLinker.argc := 0;
      RETURN;
    END;

    FOR c := FIRST (is_blank) TO LAST (is_blank) DO is_blank[c] := FALSE; END;
    is_blank [ORD (' ')]  := TRUE;
    is_blank [ORD ('\r')] := TRUE;
    is_blank [ORD ('\n')] := TRUE;
    is_blank [ORD ('\t')] := TRUE;

    (* get a bound on number of arguments *)
    n_args := 0;
    WHILE (cp^ # NUL) DO
      (* skip blanks *)
      WHILE is_blank[cp^] DO INC (cp, ADRSIZE (cp^)); END;
      IF (cp^ = NUL) THEN EXIT; END;
      INC (n_args);
      WHILE (NOT is_blank[cp^]) AND (cp^ # NUL) DO INC (cp, ADRSIZE (cp^)); END;
    END;
    n_chars := cp - RTLinker.argv;

    (* try getting the file name *)
    fn_len := MAX (0, WinBase.GetModuleFileName (NIL, ADR (filename[0]),
                                         BYTESIZE(filename)));
    INC (n_chars, fn_len + 1);

    (* allocate the new argv arrays *)
    argv := Cstdlib.malloc ((n_args+2) * BYTESIZE (ADDRESS));
    args := Cstdlib.malloc ((n_chars+1) * BYTESIZE (CHAR));
    
    (* add the file name to the arg vectors *)
    n_args := 1;  next := argv;
    next^ := args;
    INC (next, ADRSIZE (next^));
    FOR i := 0 TO fn_len-1 DO
      args^ := filename[i];
      INC (args, ADRSIZE (args^));
    END;
    args^ := NUL;  INC (args, ADRSIZE (args^));

    (* parse the command line *)
    cp := RTLinker.argv;
    WHILE (cp^ # NUL) DO

      (* skip blanks *)
      WHILE is_blank[cp^] DO INC (cp, ADRSIZE (cp^)); END;

      IF (cp^ = NUL) THEN EXIT; END;

      (* add an arg *)
      next^ := args;
      INC (next, ADRSIZE (next^));
      INC (n_args);

      (* copy an arg *)
      quoted := FALSE;
      WHILE (cp^ # NUL) DO
        IF (is_blank[cp^]) THEN
          IF (NOT quoted) THEN EXIT; END;
          args^ := cp^;
          INC (args, ADRSIZE (args^));
        ELSIF (cp^ = QUOTE) THEN
          quoted := NOT quoted;
        ELSIF (cp^ = BSLASH) THEN (* escape *)
          INC (cp, ADRSIZE (cp^));
          IF (cp^ # QUOTE) THEN
            args^ := BSLASH;
            INC (args, ADRSIZE (args^));
          END;
          IF (cp^ = NUL) THEN EXIT; END;
          args^ := cp^;
          INC (args, ADRSIZE (args^));
        ELSE
          args^ := cp^;
          INC (args, ADRSIZE (args^));
        END;
        INC (cp, ADRSIZE (cp^));
      END;
      args^ := NUL;  INC (args, ADRSIZE (args^));
      
    END;
    next^ := NIL;

    RTLinker.argv := argv;
    RTLinker.argc := n_args;
  END ParseArgs;

PROCEDURE GetArg (n: CARDINAL): TEXT =
  VAR p: Ctypes.char_star_star := RTLinker.argv + n * ADRSIZE (ADDRESS);
      a: ARRAY [0..1] OF INTEGER;
  BEGIN
    IF (n >= RTLinker.argc) THEN
      n := 2;  n := a[n];  (* force a subscript fault *)
    END;
    RETURN M3toC.StoT (p^);
  END GetArg;

PROCEDURE EnvC (): CARDINAL =
  BEGIN
    IF useFlatEnv THEN
      RETURN EnvC_Flat ();
    ELSE
      RETURN EnvC_Posix ();
    END;
  END EnvC;

PROCEDURE EnvC_Flat (): CARDINAL =
  VAR
    cnt  : CARDINAL := 0; 
    envp : Ctypes.char_star := RTLinker.envp;
  BEGIN
    IF (env_c = 0) AND (envp # NIL) THEN
      WHILE envp^ # NUL DO
        (* skip over string *)
        WHILE envp^ # NUL DO INC (envp, ADRSIZE (CHAR)) END;
        INC (envp, ADRSIZE (CHAR));
        INC (cnt);
      END;
      env_c := cnt;
    END;
    RETURN env_c;
  END EnvC_Flat;

PROCEDURE EnvC_Posix (): CARDINAL =
  VAR
    cnt  : CARDINAL := 0;
    envp : Ctypes.char_star_star := RTLinker.envp;
  BEGIN
    IF (env_c = 0) THEN
      WHILE envp^ # NIL DO
        INC (envp, ADRSIZE (ADDRESS));
        INC (cnt);
      END;
      env_c := cnt;
    END;
    RETURN env_c;
  END EnvC_Posix;

PROCEDURE GetEnv (n: CARDINAL): TEXT =
  BEGIN
    IF useFlatEnv THEN
      RETURN GetEnv_Flat (n);
    ELSE
      RETURN GetEnv_Posix (n);
    END;
  END GetEnv;

PROCEDURE GetEnv_Flat (n: CARDINAL): TEXT =
  VAR envp : Ctypes.char_star := RTLinker.envp;
      a: ARRAY [0..1] OF INTEGER;
  BEGIN
    IF (n >= EnvC ()) THEN
      n := 2;  n := a[n];  (* force a subscript fault *)
    END;
    FOR i := 0 TO n-1 DO
      WHILE envp^ # NUL DO INC (envp, ADRSIZE (CHAR)) END;
      INC (envp, ADRSIZE (CHAR));
    END;
    RETURN M3toC.StoT (envp);
  END GetEnv_Flat;

PROCEDURE GetEnv_Posix (n: CARDINAL): TEXT =
  VAR p: Ctypes.char_star_star := RTLinker.envp + n * ADRSIZE (ADDRESS);
      a: ARRAY [0..1] OF INTEGER;
  BEGIN
    IF (n >= EnvC ()) THEN
      n := 2;  n := a[n];  (* force a subscript fault *)
    END;
    RETURN M3toC.StoT (p^);
  END GetEnv_Posix;

VAR
  useFlatEnv := FALSE;
  ufe_str := M3toC.SharedTtoS("USE_FLAT_ENV");
  ufe := Cstdlib.getenv(ufe_str);
BEGIN
  useFlatEnv := ufe # NIL AND 
    (ufe^ = ORD('y') OR ufe^ = ORD('Y') OR
     ufe^ = ORD('t') OR ufe^ = ORD('T') OR ufe^ = ORD('1'));
END RTArgs.

