(* Copyright (C) 1995, Digital Equipment Corporation.       *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(*                                                          *)
(* Created on Sat Jan 11 15:49:00 PST 1992 by gnelson       *)
(*                                                          *)
(* Last modified on Mon Apr 10 16:32:08 PDT 1995 by kalsow  *)
(*      modified on Fri Jan  7 13:31:07 PST 1994 by msm     *)
(*      modified on Fri Nov  5 13:42:30 PST 1993 by wobber  *)
(*      modified on Sun Jan 12 16:17:06 PST 1992 by meehan  *)

UNSAFE MODULE IP;

IMPORT IPError, M3toC, IPInternal, Ctypes, Process;
TYPE int = Ctypes.int;

(************
PROCEDURE GetHostByName(nm: TEXT; VAR (*out*) res: Address): BOOLEAN
    RAISES {Error} =
  BEGIN
      VAR
        s := M3toC.SharedTtoS(nm);
        h := WinSock.gethostbyname(s);
      BEGIN
        M3toC.FreeSharedS(nm, s);
        IF h = NIL THEN IPInternal.InterpretError(); RETURN FALSE; END;
        res := GetAddress(h);
      END;
    RETURN TRUE;
  END GetHostByName;
**************)

PROCEDURE GetHostByName(nm: TEXT; VAR (*out*) res: Address): BOOLEAN
    RAISES {Error} =
  (* Apparently WinSock "gethostbyname" does not resolve names
     that happen to be dotted IP addresses (e.g. "123.33.44.44").
     This function does. *)
  VAR s := M3toC.SharedTtoS(nm);
      h: ADDRESS := NIL; (* hostent, null or not *)
      err: int := 0;
  BEGIN
    err := IPInternal.GetHostByName(s, res, h);
    M3toC.FreeSharedS(nm, s);
    IF h = NIL THEN IPInternal.InterpretError(err); RETURN FALSE; END;
    RETURN TRUE;
  END GetHostByName;

(*************
PROCEDURE GetCanonicalByName(nm: TEXT): TEXT RAISES {Error} =
  BEGIN
      VAR
        s := M3toC.SharedTtoS(nm);
        h := WinSock.gethostbyname(s);
      BEGIN
        M3toC.FreeSharedS (nm, s);
        IF h # NIL THEN
          RETURN M3toC.CopyStoT(h.h_name);
        END;
        IPInternal.InterpretError();
      END;
    RETURN NIL;
  END GetCanonicalByName;
************)

PROCEDURE GetCanonicalByName(nm: TEXT): TEXT RAISES {Error} =
  (* Apparently WinSock "gethostbyname" does not resolve names
     that happen to be dotted IP addresses (e.g. "123.33.44.44").
     This function does. *)
  VAR s := M3toC.SharedTtoS(nm);
      h: ADDRESS := NIL; (* hostent, null or not *)
      text: TEXT := NIL;
      err: int := 0;
  BEGIN
      err := IPInternal.GetCanonicalByName(s, text, h);
      M3toC.FreeSharedS(nm, s);
      IF h = NIL THEN  IPInternal.InterpretError(err);  RETURN NIL;  END;
      RETURN text;
  END GetCanonicalByName;

PROCEDURE GetCanonicalByAddr(addr: Address): TEXT RAISES {Error} =
  VAR text: TEXT := NIL;
      err: int := 0;
      h: ADDRESS := NIL; (* hostent, null or not *)
  BEGIN
    err := IPInternal.GetCanonicalByAddr(addr, text, h);
    IF h # NIL THEN
      RETURN text;
    END;
    IPInternal.InterpretError(err);
    RETURN NIL;
  END GetCanonicalByAddr;

PROCEDURE GetHostAddr(): Address =
  VAR address := NullAddress4;
      h: ADDRESS := NIL; (* hostent, null or not *)
      err: int := 0;
  BEGIN
      err := IPInternal.GetHostAddr(address, h);
      IF err # 0 THEN
        IPError.Die();
      END;
      RETURN address;
  END GetHostAddr;

BEGIN
  IF IPInternal.Init() THEN
    Process.RegisterExitor(IPInternal.Exitor);
  END;
END IP.
