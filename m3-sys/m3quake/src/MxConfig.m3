(* Copyright 1996-2000 Critical Mass, Inc. All rights reserved.    *)
(* See file COPYRIGHT-CMASS for details. *)

MODULE MxConfig;

IMPORT Env, Params, Pathname, M3File, M3ID, Quake, RTIO, Text, Thread;
IMPORT QMachine;
IMPORT MxConfigC;

VAR
  mu     : MUTEX         := NEW (MUTEX);
  found  : BOOLEAN       := FALSE;
  config : TEXT          := NIL;
  mach   : Quake.Machine := NIL;
  trace  : BOOLEAN       := FALSE;

PROCEDURE FindFile (): TEXT =
  BEGIN
    IF NOT found THEN
      LOCK mu DO FindConfig (); END;
    END;
    RETURN config;
  END FindFile;

PROCEDURE Get (param: TEXT): TEXT =
(* Avoid this function.
 * It uses a global Quake.Machine and does not honor command line defines.
 *)
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

PROCEDURE EnableQuakeTrace() =
  BEGIN
    trace := TRUE;
  END EnableQuakeTrace;

(*----------------------------------------------------------- internal ---*)

PROCEDURE FindConfig () =
  (* LL = mu *)
  VAR txt: TEXT;
  BEGIN
    IF (found) THEN RETURN END;

    (* try the current directory *)
    IF TryConfig (".", Filename) THEN RETURN END;

    (* try the immediate source directory *)
    IF TryConfig ("src", Filename) THEN RETURN END;

    (* try the sibling source directory *)
    IF TryConfig ("..", "src", Filename) THEN RETURN END;

    (* try the M3CONFIG environment variable *)
    txt := QMachine.GetEnv (NIL, "M3CONFIG");
    IF (txt # NIL) THEN
      IF TryConfig (txt) THEN RETURN END;
      IF TryConfig (txt, Filename) THEN RETURN END;
    END;

    (* try the directory containing the current executable *)
    txt := Pathname.Prefix (Params.Get (0));
    IF Text.Length (txt) > 0 THEN
      IF TryConfig (txt, Filename) THEN RETURN END;
    END;

    (* try the directories named by the PATH environment variable. *)
    txt := Env.Get ("PATH");
    FindConfigInPath (txt);
    IF found THEN RETURN END;

    (* try the etc directories *)
    IF TryConfig("/usr/local/cm3/etc", Filename) THEN RETURN END;
    IF TryConfig("/usr/cm3/etc", Filename) THEN RETURN END; 
    IF TryConfig("/cm3/etc", Filename) THEN RETURN END; 
    IF TryConfig("/usr/contrib/etc", Filename) THEN RETURN END; 
    IF TryConfig("/usr/local/etc", Filename) THEN RETURN END; 
    IF TryConfig("/usr/etc", Filename) THEN RETURN END; 
    IF TryConfig("/opt/etc", Filename) THEN RETURN END; 
    IF TryConfig("/sw/etc", Filename) THEN RETURN END; 
    IF TryConfig("/etc", Filename) THEN RETURN END;

    (* oh well, make sure we don't try this silly exercise again... *)
    config := NIL;
    found := TRUE;
  END FindConfig;

PROCEDURE FindConfigInPath (txt: TEXT) =
  VAR
    s0, s1: INTEGER;
    sep: CHAR;
  BEGIN
    (* what's the convention for search paths?   Unix or Win32 *)
    CONST XX = ARRAY BOOLEAN OF CHAR { ';', ':' };
    BEGIN
      sep := XX [Text.Equal (Pathname.Join ("a", "b", NIL), "a/b")];
    END;

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
  END FindConfigInPath;

PROCEDURE TryConfig (a, b, c: TEXT := NIL): BOOLEAN =
  BEGIN
    config := a;
    IF (b # NIL) THEN config := Pathname.Join (config, b, NIL); END;
    IF (c # NIL) THEN config := Pathname.Join (config, c, NIL); END;
    found := M3File.IsReadable (config);
    RETURN found;
  END TryConfig;

PROCEDURE EvalConfig () =
(* Avoid this function.
 * It sets a global Quake.Machine and does not honor command line defines.
 *)
  (* LL = mu *)
  BEGIN
    IF (mach # NIL) THEN RETURN END;
    FindConfig ();
    mach := Quake.NewMachine (Quake.NewIDMap (Str2ID, Txt2ID, ID2Txt));
    mach.trace (trace);
    TRY
      IF (config # NIL) THEN Quake.Run (mach, config); END;
    EXCEPT 
      Quake.Error(e) => RTIO.PutText ("quake runtime error: " & e);
                        RTIO.Flush ();
    | Thread.Alerted => RTIO.PutText ("interrupted");
                        RTIO.Flush ();
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

(* porting: Other OS_TYPESs? *)
PROCEDURE HOST_OS_TYPE() : OS_TYPE =
VAR os_type := MxConfigC.os_type;
BEGIN
  <* ASSERT os_type = ORD(OS_TYPE.POSIX) OR os_type = ORD(OS_TYPE.WIN32) *>
  RETURN VAL(MxConfigC.os_type, OS_TYPE);
END HOST_OS_TYPE;

(* porting: Other word sizes? *)
(* TODO Generate const TEXT in C. *)
PROCEDURE HOST_OS_TYPE_TEXT() : TEXT =
VAR os_type := HOST_OS_TYPE();
BEGIN
  IF os_type = OS_TYPE.WIN32 THEN
    RETURN "WIN32";
  END;
  RETURN "POSIX";
END HOST_OS_TYPE_TEXT;

BEGIN
END MxConfig.
