(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Last modified on Fri Jan 27 12:59:10 PST 1995 by kalsow     *)
(*      modified on Fri May  7 22:57:08 PDT 1993 by mjordan    *)

(* In Windows/NT, "envp" points at a null-terminated block of
   null-terminated strings *)

UNSAFE MODULE RTArgs;

IMPORT RTLinker, Ctypes, Cstdlib, M3toC, WinBase;

CONST
  NUL    = VAL (0, Ctypes.char);
  QUOTE  = VAL (ORD ('"'), Ctypes.char);
  BSLASH = VAL (ORD ('\134'), Ctypes.char);

VAR env_c : CARDINAL := 0;

PROCEDURE ArgC (): CARDINAL =
  BEGIN
    IF (RTLinker.info.argc < 0) THEN ParseArgs (); END;
    RETURN RTLinker.info.argc;
  END ArgC;

PROCEDURE ParseArgs () =
  (* This hack is necessary because a windows GUI program is passed
     its command line unparsed.  This code is executed before the
     runtime type system is initialized. *)
  VAR
    cp       : Ctypes.char_star := RTLinker.info.argv;
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
      RTLinker.info.argc := 0;
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
    n_chars := cp - RTLinker.info.argv;

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
    cp := RTLinker.info.argv;
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

    RTLinker.info.argv := argv;
    RTLinker.info.argc := n_args;
  END ParseArgs;

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
    envp : Ctypes.char_star := RTLinker.info.envp;
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
  END EnvC;

PROCEDURE GetEnv (n: CARDINAL): TEXT =
  VAR envp : Ctypes.char_star := RTLinker.info.envp;
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
  END GetEnv;

BEGIN
END RTArgs.

