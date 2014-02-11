(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Created on Sat Jan 11 15:49:00 PST 1992 by gnelson *)
(* Last modified onFri Jan  7 13:31:07 PST 1994by msm    *)
(*      modified on Fri Nov  5 13:42:30 PST 1993 by wobber *)
(*      modified on Sun Jan 12 16:17:06 PST 1992 by meehan *)

UNSAFE MODULE IP;

IMPORT Herrno, IPError, M3toC,
       Unetdb, Usocket, Unix, Uin, Utypes;

VAR mu := NEW(MUTEX);

PROCEDURE GetHostByName(nm: TEXT; VAR (*out*) res: Address): BOOLEAN
    RAISES {Error} =
VAR hostent: Unetdb.struct_hostent;
  BEGIN
    LOCK mu DO
      VAR
        s := M3toC.SharedTtoS(nm);
        h := Unetdb.gethostbyname(s, ADR(hostent));
      BEGIN
        M3toC.FreeSharedS(nm, s);
        IF h = NIL THEN InterpretError(); RETURN FALSE; END;
        res := GetAddress(h);
      END;
    END;
    RETURN TRUE;
  END GetHostByName;

PROCEDURE GetCanonicalByName(nm: TEXT): TEXT RAISES {Error} =
VAR hostent: Unetdb.struct_hostent;
  BEGIN
    LOCK mu DO
      VAR
        s := M3toC.SharedTtoS(nm);
        h := Unetdb.gethostbyname(s, ADR(hostent));
      BEGIN
        M3toC.FreeSharedS(nm, s);
        IF h # NIL THEN
          RETURN M3toC.CopyStoT(h.h_name);
        END;
        InterpretError();
      END;
    END;
    RETURN NIL;
  END GetCanonicalByName;

PROCEDURE GetCanonicalByAddr(addr: Address): TEXT RAISES {Error} =
  VAR ua: Uin.struct_in_addr;
      hostent: Unetdb.struct_hostent;
  BEGIN
    ua.s_addr := LOOPHOLE(addr, Utypes.u_int);
    LOCK mu DO
      VAR h := Unetdb.gethostbyaddr(
                   ADR(ua), BYTESIZE(ua), Usocket.AF_INET, ADR(hostent));
      BEGIN
        IF h # NIL THEN
          RETURN M3toC.CopyStoT(h.h_name);
        END;
        InterpretError();
      END;
    END;
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
  BEGIN
    LOCK mu DO
      IF Unix.gethostname(ADR(hname[0]), BYTESIZE(hname)) # 0 THEN
        IPError.Die ();
      END;
      VAR h := Unetdb.gethostbyname(ADR(hname[0]), ADR(hostent));
      BEGIN
        IF h = NIL THEN
          h := Unetdb.gethostbyname(ADR(lochost[0]), ADR(hostent)); 
          IF h = NIL THEN IPError.Die(); END;
        END;
        RETURN GetAddress(h);
      END;
    END;
  END GetHostAddr;

PROCEDURE InterpretError() RAISES {Error} =
  VAR err := Herrno.Get_h_errno();
  BEGIN
    IF (err = Unetdb.TRY_AGAIN) OR (err = Unetdb.NO_RECOVERY) OR (err = Unetdb.NO_ADDRESS) THEN
        IPError.Raise (LookupFailure);
    END;
  END InterpretError;

BEGIN
END IP.
