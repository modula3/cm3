(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

UNSAFE MODULE OpSys;

IMPORT Ctypes, M3toC, WinDef, WinSock;

PROCEDURE GetHostName (): TEXT RAISES {Error} =
  VAR
    vers  : WinDef.WORD;
    data  : WinSock.WSAData;
    status: Ctypes.int;
    name  : ARRAY [0 .. 255] OF Ctypes.char;
  BEGIN
    (* Initialize the WinSock DLL. *)
    vers := WinDef.MAKEWORD (1, 1);
    status := WinSock.WSAStartup (vers, ADR (data));
    IF status # 0 THEN
      RAISE Error;
    END;

    TRY
      (* Determine the host name. *)
      status := WinSock.gethostname (ADR (name), BYTESIZE(name));
      IF status # 0 THEN
        RAISE Error;
      END;
      RETURN M3toC.StoT (ADR (name));

    FINALLY
      (* Unregister the WinSock DLL -- required! *)
      status := WinSock.WSACleanup ();
      IF status # 0 THEN
        RAISE Error;
      END;
    END;
  END GetHostName;


BEGIN
END OpSys.
