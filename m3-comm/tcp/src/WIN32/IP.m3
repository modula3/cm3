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

IMPORT IPError, M3toC, Process, WinSock;

(************
PROCEDURE GetHostByName(nm: TEXT; VAR (*out*) res: Address): BOOLEAN
    RAISES {Error} =
  BEGIN
      VAR
        s := M3toC.SharedTtoS(nm);
        h := WinSock.gethostbyname(s);
      BEGIN
        M3toC.FreeSharedS(nm, s);
        IF h = NIL THEN InterpretError(); RETURN FALSE; END;
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
  VAR
    s := M3toC.SharedTtoS(nm);
    a := WinSock.inet_addr(s);
    h : WinSock.struct_hostent_star;
  BEGIN
    IF a # WinSock.INADDR_NONE THEN
      (* the name is already a dotted IP address *)
      M3toC.FreeSharedS(nm, s);
      res := LOOPHOLE (a, Address);
    ELSE
      (* the name is not a dotted IP address *)
        h := WinSock.gethostbyname(s);
        M3toC.FreeSharedS(nm, s);
        IF h = NIL THEN InterpretError(); RETURN FALSE; END;
        res := GetAddress(h);
    END;
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
        InterpretError();
      END;
    RETURN NIL;
  END GetCanonicalByName;
************)

PROCEDURE GetCanonicalByName(nm: TEXT): TEXT RAISES {Error} =
  (* Apparently WinSock "gethostbyname" does not resolve names
     that happen to be dotted IP addresses (e.g. "123.33.44.44").
     This function does. *)
  VAR
    s := M3toC.SharedTtoS(nm);
    a := WinSock.inet_addr(s);
    h : WinSock.struct_hostent_star;
  BEGIN
      IF a = WinSock.INADDR_NONE THEN
        (* the name is not a dotted IP address *)
        h := WinSock.gethostbyname(s);
      ELSE
        (* the name is a dotted IP address *)
        h := WinSock.gethostbyaddr(ADR(a), BYTESIZE(a), WinSock.PF_INET);
      END;
      M3toC.FreeSharedS(nm, s);
      IF h = NIL THEN  InterpretError();  RETURN NIL;  END;
      RETURN M3toC.CopyStoT(h.h_name);
  END GetCanonicalByName;

PROCEDURE GetCanonicalByAddr(addr: Address): TEXT RAISES {Error} =
  VAR ua: WinSock.struct_in_addr;
  BEGIN
    ua.s_addr := LOOPHOLE(addr, WinSock.u_long);
      VAR h := WinSock.gethostbyaddr(
                   ADR(ua), BYTESIZE(ua), WinSock.AF_INET);
      BEGIN
        IF h # NIL THEN
          RETURN M3toC.CopyStoT(h.h_name);
        END;
      END;
      InterpretError();
    RETURN NIL;
  END GetCanonicalByAddr;

PROCEDURE GetAddress (ent: WinSock.struct_hostent_star): Address =
  VAR ua: WinSock.struct_in_addr;
  BEGIN
    <* ASSERT ent.h_length <= BYTESIZE(Address) *>
    ua := LOOPHOLE(ent.h_addr_list,
                    UNTRACED REF UNTRACED REF WinSock.struct_in_addr)^^;
    RETURN LOOPHOLE(ua.s_addr, Address);
  END GetAddress;

PROCEDURE GetHostAddr(): Address =
  VAR hname: ARRAY [0..255] OF CHAR;
      lochost := ARRAY [0..9] OF CHAR {'1', '2', '7', '.', '0', '.', '0',
                '.', '1', '\000'};
      ent: WinSock.struct_hostent_star;
  BEGIN
      IF WinSock.gethostname(ADR(hname[0]), BYTESIZE(hname)) # 0 THEN
        IPError.Die();
      END;
      ent := WinSock.gethostbyname(ADR(hname[0]));
      IF ent = NIL THEN
        ent := WinSock.gethostbyname(ADR(lochost[0])); 
      END;
      RETURN GetAddress(ent);
  END GetHostAddr;

PROCEDURE InterpretError() RAISES {Error} =
  VAR err := WinSock.WSAGetLastError();
  BEGIN
    CASE err OF
    | WinSock.TRY_AGAIN, WinSock.NO_RECOVERY, WinSock.NO_ADDRESS =>
        IPError.Raise(LookupFailure);
    ELSE
    END;
  END InterpretError;

CONST WinSockVersion = 16_0101;       (* App version 1.1 *)

PROCEDURE Init() =
  VAR data: WinSock.WSAData;
  BEGIN
    IF WinSock.WSAStartup(WinSockVersion, ADR(data)) # 0 THEN
      IPError.Die();
    END;
    Process.RegisterExitor(Exitor);
  END Init;

PROCEDURE Exitor() =
  BEGIN
    EVAL WinSock.WSACleanup();
  END Exitor;

BEGIN
  Init();
END IP.
