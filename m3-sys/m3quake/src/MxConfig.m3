(* Copyright 1996-2000 Critical Mass, Inc. All rights reserved.    *)
(* See file COPYRIGHT-CMASS for details. *)

MODULE MxConfig;

IMPORT Env, Params, Pathname, M3File, M3ID, Quake, Text, Thread;

VAR
  mu     : MUTEX         := NEW (MUTEX);
  found  : BOOLEAN       := FALSE;
  config : TEXT          := NIL;
  mach   : Quake.Machine := NIL;

PROCEDURE FindFile (): TEXT =
  BEGIN
    IF NOT found THEN
      LOCK mu DO FindConfig (); END;
    END;
    RETURN config;
  END FindFile;

PROCEDURE Get (param: TEXT): TEXT =
  BEGIN
    LOCK mu DO
      EvalConfig ();
      TRY
        RETURN Quake.LookUp (mach, param);
      EXCEPT Quake.Error =>
        RETURN NIL;
      END;
    END;
  END Get;

(*----------------------------------------------------------- internal ---*)

PROCEDURE FindConfig () =
  (* LL = mu *)
  VAR txt: TEXT;  s0, s1: INTEGER;  sep: CHAR;
  BEGIN
    IF (found) THEN RETURN END;

    (* try the current directory *)
    IF TryConfig (Filename) THEN RETURN END;

    (* try the immediate source directory *)
    IF TryConfig ("src", Filename) THEN RETURN END;

    (* try the sibling source directory *)
    IF TryConfig ("..", "src", Filename) THEN RETURN END;

    (* try the M3CONFIG environment variable *)
    txt := Env.Get ("M3CONFIG");
    IF (txt # NIL) THEN
      IF TryConfig (txt) THEN RETURN END;
      IF TryConfig (txt, Filename) THEN RETURN END;
    END;

    (* try the directory containing the current executable *)
    txt := Pathname.Prefix (Params.Get (0));
    IF Text.Length (txt) > 0 THEN
      IF TryConfig (txt, Filename) THEN RETURN END;
    END;

    (* what's the convention for search paths?   Unix or Win32 *)
    CONST XX = ARRAY BOOLEAN OF CHAR { ';', ':' };
    BEGIN
      sep := XX [Text.Equal (Pathname.Join ("a", "b", NIL), "a/b")];
    END;

    (* try the directories named by the PATH environment variable. *)
    txt := Env.Get ("PATH");
    IF (txt # NIL) THEN
      s0 := 0;
      WHILE (s0 < Text.Length (txt)) DO
        s1 := Text.FindChar (txt, sep, s0);
        IF (s1 < 0) THEN s1 := Text.Length (txt); END;
        IF (s0 < s1) THEN
          IF TryConfig (Text.Sub (txt, s0, s1-s0), Filename) THEN RETURN END;
        END;
        s0 := s1 + 1;
      END;
    END;

    (* oh well, make sure we don't try this silly exercise again... *)
    config := NIL;
    found := TRUE;
  END FindConfig;

PROCEDURE TryConfig (a, b, c: TEXT := NIL): BOOLEAN =
  BEGIN
    config := a;
    IF (b # NIL) THEN config := Pathname.Join (config, b, NIL); END;
    IF (c # NIL) THEN config := Pathname.Join (config, c, NIL); END;
    found := M3File.IsReadable (config);
    RETURN found;
  END TryConfig;

PROCEDURE EvalConfig () =
  (* LL = mu *)
  BEGIN
    IF (mach # NIL) THEN RETURN END;
    FindConfig ();
    mach := Quake.NewMachine (Quake.NewIDMap (Str2ID, Txt2ID, ID2Txt));
    TRY
      IF (config # NIL) THEN Quake.Run (mach, config); END;
    EXCEPT Quake.Error, Thread.Alerted =>
      (* ouch *)
    END;
  END EvalConfig;

PROCEDURE Str2ID (READONLY x: ARRAY OF CHAR): Quake.ID =
  BEGIN
    RETURN M3ID.FromStr (x);
  END Str2ID;

PROCEDURE Txt2ID (t: TEXT): Quake.ID =
  BEGIN
    RETURN M3ID.Add (t);
  END Txt2ID;

PROCEDURE ID2Txt (i: Quake.ID): TEXT =
  BEGIN
    RETURN M3ID.ToText (i);
  END ID2Txt;

BEGIN
END MxConfig.

