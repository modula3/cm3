UNSAFE MODULE IPCommon EXPORTS IP;
IMPORT Ctypes, IPError, M3toC, IPInternal, Usocket;

TYPE char_star = Ctypes.char_star;
     int = Ctypes.int;

(* Return the first usable address - there could be many - and more likely an IP4 address. *)
PROCEDURE GetAddrInfo(READONLY name,service : TEXT) : EP RAISES {Error} =
  VAR
    node: char_star := NIL;
    port : char_star := NIL;
    err : int := 0;
    ep: EP := NIL;
  BEGIN
    IF name = NIL AND service = NIL THEN
      IPError.Raise (LookupFailure);
    END;

    IF name # NIL THEN node := M3toC.SharedTtoS(name); END;
    IF service # NIL THEN port := M3toC.SharedTtoS(service); END;

    err := IPInternal.GetAddrInfo(ep, node, port);

    M3toC.FreeSharedS(name, node);
    M3toC.FreeSharedS(service, port);

    IF err # 0 THEN
      IPError.Raise (LookupFailure, err);
    END;

    RETURN ep;
END GetAddrInfo;

PROCEDURE GetNameInfo(ep : EP; VAR (*out*) host,service : TEXT) RAISES {Error} =
  VAR
    addr: ADDRESS := NIL;
    family := Usocket.AF_INET;
    err: int := 0;
  BEGIN
    TYPECASE ep OF
    | Endpoint4(ep4) =>
       family := Usocket.AF_INET;
       addr :=  ADR(ep4.adr);
    | Endpoint16(ep6) =>
       family := IPInternal.AF_INET6;
       addr :=  ADR(ep6.adr);
    ELSE
        IPError.Raise (LookupFailure);
    END;

    err := IPInternal.GetNameInfo(family, ep.port, addr, host, service);

    IF err # 0 THEN
      IPError.Raise (LookupFailure, err);
    END;
  END GetNameInfo;

BEGIN
END IPCommon.
