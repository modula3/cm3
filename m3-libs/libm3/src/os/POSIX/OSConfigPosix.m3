(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

UNSAFE MODULE OSConfigPosix EXPORTS OSConfig;

IMPORT Env, OSError, Process, OSConfigPosixC;
IMPORT OSConfigPosix_DefaultOSName, OSConfigPosix_DefaultArch;

VAR
  host_name  : TEXT := NIL;
  host_arch  : TEXT := NIL;
  os_name    : TEXT := NIL;
  os_version : TEXT := NIL;
  user_name  : TEXT := NIL;
  user_home  : TEXT := NIL;

PROCEDURE HostName (): TEXT =
  BEGIN
    IF (host_name = NIL) THEN Init ();  END;
    RETURN host_name;
  END HostName;

PROCEDURE HostArchitecture (): TEXT =
  BEGIN
    IF (host_arch = NIL) THEN Init (); END;
    RETURN host_arch;
  END HostArchitecture;

PROCEDURE OSName (): TEXT =
  BEGIN
    IF (os_name = NIL) THEN Init (); END;
    RETURN os_name;
  END OSName;

PROCEDURE OSVersion (): TEXT =
  BEGIN
    IF (os_version = NIL) THEN Init (); END;
    RETURN os_version;
  END OSVersion;

PROCEDURE UserName (): TEXT =
  BEGIN
    IF user_name = NIL THEN InitUserName (); END;
    RETURN user_name;
  END UserName;

PROCEDURE UserHome (): TEXT =
  BEGIN
    IF user_home = NIL THEN InitUserHome (); END;
    RETURN user_home;
  END UserHome;

(*---------------------------------------------------------- internal ---*)

PROCEDURE Init () =
  BEGIN
    IF (host_name # NIL) AND (host_arch # NIL)
            AND (os_name # NIL) AND (os_version # NIL) THEN
        RETURN;
    END;
    IF OSConfigPosixC.Init (host_name, host_arch, os_name, os_version) < 0 THEN
      host_name  := "<unknown>";
      host_arch  := OSConfigPosix_DefaultArch.Value;
      os_name    := OSConfigPosix_DefaultOSName.Value;
      os_version := "";
    END;
  END Init;

PROCEDURE InitUserName () =
  BEGIN
    IF user_name # NIL THEN RETURN; END;

    user_name := Env.Get ("USER");
    IF user_name # NIL THEN RETURN; END;

    user_name := "<unknown user>";
  END InitUserName;

PROCEDURE InitUserHome () =
  BEGIN
    IF user_home # NIL THEN RETURN; END;

    user_home := Env.Get ("HOME");
    IF user_home # NIL THEN RETURN; END;

    TRY user_home := Process.GetWorkingDirectory ();
    EXCEPT OSError.E =>
    END;
    IF user_home # NIL THEN RETURN; END;

    user_home := "/tmp";
  END InitUserHome;

BEGIN
END OSConfigPosix.
