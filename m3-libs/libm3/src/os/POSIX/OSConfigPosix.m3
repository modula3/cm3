(* Copyright 1997, Critical Mass, Inc.  All rights reserved. *)

UNSAFE MODULE OSConfigPosix EXPORTS OSConfig;

IMPORT Compiler, Env, M3toC, OSError, Process, Uutsname;

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
  VAR uts: Uutsname.struct_utsname;
  BEGIN
    IF os_version # NIL THEN RETURN; END;
    IF Uutsname.uname (ADR (uts)) >= 0 THEN
      host_name  := M3toC.CopyStoT (ADR (uts.nodename[0]));
      host_arch  := M3toC.CopyStoT (ADR (uts.machine[0]));
      os_name    := M3toC.CopyStoT (ADR (uts.sysname[0]));
      os_version := M3toC.CopyStoT (ADR (uts.release[0]));
    ELSE
      host_name  := "<unknown>";
      host_arch  := DefaultArch [Compiler.ThisPlatform];
      os_name    := DefaultOSName [Compiler.ThisPlatform];
      os_version := "";
    END;
  END Init;

CONST
  DefaultOSName = ARRAY Compiler.Platform OF TEXT {
   (* AIX386    *)  "AIX",
   (* ALPHA_OSF *)  "Digital Unix",
   (* AP3000    *)  "Unix",
   (* ARM       *)  "Unix",
   (* DS3100    *)  "Ultrix",
   (* FreeBSD   *)  "FreeBSD",
   (* FreeBSD2  *)  "FreeBSD",
   (* HP300     *)  "HP/UX",
   (* HPPA      *)  "HP/UX",
   (* IBMR2     *)  "AIX",
   (* IBMRT     *)  "AIX",
   (* IRIX5     *)  "Irix",
   (* LINUX     *)  "Linux",
   (* LINUXELF  *)  "Linux",
   (* NEXT      *)  "NextOS",
   (* NT386     *)  "Win32",
   (* OKI       *)  "Unix",
   (* OS2       *)  "OS/2",
   (* SEQUENT   *)  "Unix",
   (* SOLgnu    *)  "Solaris",
   (* SOLsun    *)  "Solaris",
   (* SPARC     *)  "SunOS",
   (* SUN3      *)  "SunOS",
   (* SUN386    *)  "SunOS",
   (* UMAX      *)  "Unix",
   (* VAX       *)  "Ultrix",
   (* FreeBSD3  *)  "FreeBSD",
   (* FreeBSD4  *)  "FreeBSD"
  };

CONST
  DefaultArch = ARRAY Compiler.Platform OF TEXT {
   (* AIX386    *)  "i386",
   (* ALPHA_OSF *)  "alpha",
   (* AP3000    *)  "apollo",
   (* ARM       *)  "acorn risc",
   (* DS3100    *)  "mips",
   (* FreeBSD   *)  "i486",
   (* FreeBSD2  *)  "i486",
   (* HP300     *)  "hp300",
   (* HPPA      *)  "hppa",
   (* IBMR2     *)  "IBM romp",
   (* IBMRT     *)  "IBM RT",
   (* IRIX5     *)  "mips",
   (* LINUX     *)  "i486",
   (* LINUXELF  *)  "i486",
   (* NEXT      *)  "m68K",
   (* NT386     *)  "i486",
   (* OKI       *)  "m68K",
   (* OS2       *)  "i486",
   (* SEQUENT   *)  "m68K",
   (* SOLgnu    *)  "sparc",
   (* SOLsun    *)  "sparc",
   (* SPARC     *)  "sparc",
   (* SUN3      *)  "sparc3",
   (* SUN386    *)  "i386",
   (* UMAX      *)  "m68K",
   (* VAX       *)  "vax",
   (* FreeBSD3  *)  "i486",
   (* FreeBSD4  *)  "i486"
  };

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
