(* Copyright 1997, Critical Mass, Inc.  All rights reserved. *)

UNSAFE MODULE OSConfigWin32 EXPORTS OSConfig;

IMPORT Env, Fmt, M3toC, OSError, Process, Text, WinBase, WinNT, WinSock;

VAR
  host_name  : TEXT := NIL;
  host_arch  : TEXT := NIL;
  os_name    : TEXT := NIL;
  os_version : TEXT := NIL;
  user_name  : TEXT := NIL;
  user_home  : TEXT := NIL;

PROCEDURE HostName (): TEXT =
  BEGIN
    IF (host_name = NIL) THEN InitHostName (); END;
    RETURN host_name;
  END HostName;

PROCEDURE HostArchitecture (): TEXT =
  BEGIN
    IF (host_arch = NIL) THEN InitHostArch (); END;
    RETURN host_arch;
  END HostArchitecture;

PROCEDURE OSName (): TEXT =
  BEGIN
    IF (os_name = NIL) THEN InitOSName (); END;
    RETURN os_name;
  END OSName;

PROCEDURE OSVersion (): TEXT =
  BEGIN
    IF (os_version = NIL) THEN InitOSName (); END;
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

(*----------------------------------------------------------- internal ---*)

PROCEDURE InitHostName () =
  CONST WinSockVersion = 16_0101;       (* App version 1.1 *)
  VAR data: WinSock.WSAData;  name: ARRAY [0..255] OF CHAR;
  BEGIN
    IF host_name # NIL THEN RETURN; END;
    IF WinSock.WSAStartup(WinSockVersion, ADR(data)) = 0 THEN
      IF WinSock.gethostname (ADR (name[0]), BYTESIZE (name)) = 0
        THEN host_name := M3toC.CopyStoT (ADR (name[0]));
        ELSE host_name := "<unknown>";
      END;
      EVAL WinSock.WSACleanup();
    ELSE
      host_name := "<unknown>";
    END;
  END InitHostName;

PROCEDURE InitHostArch () =
  VAR sys: WinBase.SYSTEM_INFO;
  BEGIN
    IF host_arch # NIL THEN RETURN; END;
    WinBase.GetSystemInfo (ADR (sys));
    CASE sys.wProcessorArchitecture OF
    | WinNT.PROCESSOR_ARCHITECTURE_INTEL =>
        CASE sys.wProcessorLevel OF
        | 3 =>  host_arch := "Intel 386";
        | 4 =>  host_arch := "Intel 486";
        | 5 =>  host_arch := "Intel Pentium";
        ELSE    host_arch := "Intel x86";
        END;
    | WinNT.PROCESSOR_ARCHITECTURE_ALPHA =>
        host_arch := "Alpha " & Fmt.Int (sys.wProcessorLevel);
    | WinNT.PROCESSOR_ARCHITECTURE_MIPS =>
        host_arch := "Mips R4000";
    | WinNT.PROCESSOR_ARCHITECTURE_PPC =>
        CASE sys.wProcessorLevel OF
        | 1  =>  host_arch := "PowerPC 601";
        | 3  =>  host_arch := "PowerPC 603";
        | 4  =>  host_arch := "PowerPC 604";
        | 6  =>  host_arch := "PowerPC 603+";
        | 9  =>  host_arch := "PowerPC 604+";
        | 20 =>  host_arch := "PowerPC 620";
        ELSE     host_arch := "PowerPC";
        END;
    ELSE
        host_arch := "<unknown>";
    END;
  END InitHostArch;

PROCEDURE InitOSName () =
  VAR ver: WinBase.OSVERSIONINFO;
  BEGIN
    IF os_version # NIL THEN RETURN; END;
    ver.dwOSVersionInfoSize := BYTESIZE (ver);
    IF WinBase.GetVersionEx (ADR (ver)) # 0 THEN
      CASE ver.dwPlatformId OF
      | WinBase.VER_PLATFORM_WIN32s        => os_name := "Windows (with Win32s)";
      | WinBase.VER_PLATFORM_WIN32_WINDOWS => os_name := "Windows 95";
      | WinBase.VER_PLATFORM_WIN32_NT      => os_name := "Windows NT";
      ELSE                                    os_name := "Windows";
      END;
      os_version := Fmt.Int (ver.dwMajorVersion)
                    & "." & Fmt.Int (ver.dwMinorVersion);
    ELSE
      os_name    := "Windows";
      os_version := "";
    END;
  END InitOSName;

PROCEDURE InitUserName () =
  VAR buf: ARRAY [0..127] OF CHAR;  len: INTEGER;
  BEGIN
    IF user_name # NIL THEN RETURN; END;

    user_name := Env.Get ("USER");
    IF user_name # NIL THEN RETURN; END;

    len := NUMBER (buf);
    IF WinBase.GetUserName (ADR (buf[0]), ADR (len)) # 0 AND (len > 1) THEN
      user_name := Text.FromChars (SUBARRAY (buf, 0, len-1));
    END;
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

    user_home := "C:\\";
  END InitUserHome;

BEGIN
END OSConfigWin32.
