(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Created on Sat Jan 11 15:49:00 PST 1992 by gnelson *)
(* Last modified onFri Jan  7 13:31:07 PST 1994by msm    *)
(*      modified on Fri Nov  5 13:42:30 PST 1993 by wobber *)
(*      modified on Sun Jan 12 16:17:06 PST 1992 by meehan *)

UNSAFE MODULE IP;

IMPORT Herrno, IPError, M3toC,
       Unetdb, Usocket, Unix, Uin, Utypes, Ctypes;

VAR mu := NEW(MUTEX);

PROCEDURE GetHostByName(nm: TEXT; VAR (*out*) res: Address): BOOLEAN
    RAISES {Error} =
  VAR hostent: Unetdb.struct_hostent;
      h: Unetdb.struct_hostent_star;
      s := M3toC.SharedTtoS(nm);
  BEGIN
    LOCK mu DO
        h := Unetdb.gethostbyname(s, ADR(hostent));
        res := GetAddress(h);
    END;
    M3toC.FreeSharedS(nm, s);
    IF h = NIL THEN InterpretError(); RETURN FALSE; END;
    RETURN TRUE;
  END GetHostByName;

PROCEDURE GetCanonicalByName(nm: TEXT): TEXT RAISES {Error} =
  VAR hostent: Unetdb.struct_hostent;
      h: Unetdb.struct_hostent_star;
      s := M3toC.SharedTtoS(nm);
  BEGIN
    LOCK mu DO
      h := Unetdb.gethostbyname(s, ADR(hostent));
      M3toC.FreeSharedS(nm, s);
      IF h # NIL THEN
        RETURN M3toC.CopyStoT(h.h_name);
      END;
    END;
    InterpretError();
    RETURN NIL;
  END GetCanonicalByName;

PROCEDURE GetCanonicalByAddr(addr: Address): TEXT RAISES {Error} =
  VAR ua := Uin.struct_in_addr{0};
      hostent: Unetdb.struct_hostent;
      h: Unetdb.struct_hostent_star;
  BEGIN
    ua.s_addr := LOOPHOLE(addr, Utypes.u_int);
    LOCK mu DO
      h := Unetdb.gethostbyaddr(ADR(ua), BYTESIZE(ua), Usocket.AF_INET, ADR(hostent));
      IF h # NIL THEN
        RETURN M3toC.CopyStoT(h.h_name);
      END;
    END;
    InterpretError();
    RETURN NIL;
  END GetCanonicalByAddr;

PROCEDURE GetAddress (ent: Unetdb.struct_hostent_star): Address =
  VAR ua: Uin.struct_in_addr;
  BEGIN
    <* ASSERT ent.h_length <= BYTESIZE(Address) *>
    ua := LOOPHOLE(ent.h_addr_list,
                    UNTRACED REF UNTRACED REF Uin.struct_in_addr)^^;
    RETURN LOOPHOLE(ua.s_addr, Address);
  END GetAddress;

PROCEDURE GetHostAddr(): Address =
  VAR hname: ARRAY [0..255] OF CHAR;
      hostent: Unetdb.struct_hostent;
      lochost := ARRAY [0..9] OF CHAR {'1', '2', '7', '.', '0', '.', '0',
                '.', '1', '\000'};
      h: Unetdb.struct_hostent_star;
  BEGIN
    LOCK mu DO
      IF Unix.gethostname(ADR(hname[0]), BYTESIZE(hname)) = 0 THEN
        h := Unetdb.gethostbyname(ADR(hname[0]), ADR(hostent));
        IF h = NIL THEN
          h := Unetdb.gethostbyname(ADR(lochost[0]), ADR(hostent));
        END;
        IF h # NIL THEN
          RETURN GetAddress(h);
        END;
      END;
    END;
    IPError.Die ();
    RETURN NullAddress4;
  END GetHostAddr;

CONST
  MaxHost = 1025;
  MaxService = 32;

TYPE
  RefAddrInfo = UNTRACED REF Unetdb.addrinfo_t;
  RefSockaddr4 = UNTRACED REF Uin.struct_sockaddr_in;
  RefSockaddr16 = UNTRACED REF Uin.struct_sockaddr_in6;

(* Return the first usable address - there could be many - and more likely an IP4 address. *)
PROCEDURE GetAddrInfo(READONLY name,service : TEXT) : EP RAISES {Error} =
  VAR
    node,port : Ctypes.char_star;
    res : RefAddrInfo;
    sa4 : RefSockaddr4;
    sa16 : RefSockaddr16;
    a4 : Address4;
    a16 : Address16;
    ep4 : Endpoint4;
    ep16 : Endpoint16;
    err : Ctypes.int;
  BEGIN
    IF name = NIL AND service = NIL THEN
      IPError.Raise (LookupFailure);
    END;
    IF name = NIL THEN node := NIL; ELSE node := M3toC.CopyTtoS(name); END;
    IF service = NIL THEN port := NIL; ELSE port := M3toC.CopyTtoS(service); END;

    err := Unetdb.getaddrinfo(node,port,NIL,res);
    IF err # 0 THEN
      IPError.Raise (LookupFailure, err);
    ELSE
      WHILE res # NIL DO
        IF res.ai_family = Usocket.AF_INET THEN
          sa4 := LOOPHOLE(res.ai_addr,RefSockaddr4);
          a4 := LOOPHOLE(sa4.sin_addr,Address4);
          ep4 := NEW(Endpoint4);
          ep4.adr := a4;
          ep4.port := sa4.sin_port;
          Unetdb.freeaddrinfo(res);
          RETURN ep4;
        ELSIF res.ai_family = Usocket.AF_INET6 THEN
          sa16 := LOOPHOLE(res.ai_addr, RefSockaddr16);
          a16 := LOOPHOLE(sa16.sin6_addr, Address16);
          ep16 := NEW(Endpoint16);
          ep16.adr := a16;
          ep16.port := sa16.sin6_port;
          Unetdb.freeaddrinfo(res);
          RETURN ep16;
        END;
        res := res.ai_next;
    END;
  END;
  RETURN NIL;
END GetAddrInfo;

PROCEDURE GetNameInfo(ep : EP; VAR (*out*) host,service : TEXT) RAISES {Error} =
  VAR
    ai_addr : UNTRACED REF Uin.struct_sockaddr;
    ai_addrlen : Ctypes.int;
    refSA4 : RefSockaddr4;
    refSA16 : RefSockaddr16;
    hostBuf,serviceBuf : REF ARRAY OF CHAR;
    h,s : ADDRESS;
    sLen : Ctypes.int;
    err : Ctypes.int;
  BEGIN
    hostBuf := NEW(REF ARRAY OF CHAR,MaxHost);
    h := ADR(hostBuf[0]);
    IF ep.port # 0 THEN
      serviceBuf := NEW(REF ARRAY OF CHAR,MaxService);
      s := ADR(serviceBuf[0]);
      sLen := MaxService;
    ELSE
      service := NIL; s := NIL; sLen := 0;
    END;

    TYPECASE ep OF
    | Endpoint4(xx) =>
       refSA4 := NEW(RefSockaddr4);
       refSA4.sin_family := Usocket.AF_INET;
       refSA4.sin_port := ep.port;
       refSA4.sin_addr :=  LOOPHOLE(xx.adr, Uin.struct_in_addr);
       ai_addr := LOOPHOLE(refSA4, UNTRACED REF Uin.struct_sockaddr);
       ai_addrlen := BYTESIZE(Uin.struct_sockaddr_in);
    | Endpoint16(xx) =>
       refSA16 := NEW(RefSockaddr16);
       refSA16.sin6_family := Usocket.AF_INET6;
       refSA16.sin6_port := ep.port;
       refSA16.sin6_addr :=  LOOPHOLE(xx.adr, Uin.struct_in6_addr);
       ai_addr := LOOPHOLE(refSA16,UNTRACED REF Uin.struct_sockaddr);
       ai_addrlen := BYTESIZE(Uin.struct_sockaddr_in6);
    ELSE
        IPError.Raise (LookupFailure);
    END;

    err := Unetdb.getnameinfo(ai_addr, ai_addrlen, h, MaxHost , s, sLen, 0);
    IF err = 0 THEN
      host := M3toC.CopyStoT(h);
      IF s # NIL THEN
        service := M3toC.CopyStoT(s);
      END;
    ELSE
      IPError.Raise (LookupFailure, err);
    END;
  END GetNameInfo;

PROCEDURE InterpretError() RAISES {Error} =
  VAR err := Herrno.Get_h_errno();
  BEGIN
    IF (err = Unetdb.TRY_AGAIN) OR (err = Unetdb.NO_RECOVERY) OR (err = Unetdb.NO_ADDRESS) THEN
        IPError.Raise (LookupFailure);
    END;
  END InterpretError;

BEGIN
END IP.
