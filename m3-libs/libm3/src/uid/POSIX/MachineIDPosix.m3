(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Last modified on Fri Feb 10 11:56:08 PST 1995 by kalsow     *)
(*      modified on Thu Jul 15 16:23:08 PDT 1993 by swart      *)

UNSAFE MODULE MachineIDPosix EXPORTS MachineID;

IMPORT Unix, Usocket, Unetdb, Ctypes;

FROM Word IMPORT Shift, And, Or;

TYPE
  Byte = BITS 8 FOR [0..255];

(* The following information is commented out in the Unix interface.  This
   is probably system dependent. *)

TYPE
  struct_ifreq = RECORD
    ifr_name  : ARRAY [0 .. 15] OF CHAR;  (* if name, e.g.  "en0" *)
    ifru_addr : Usocket.struct_sockaddr;
  END;

  struct_ifconf = RECORD
    ifc_len  : INTEGER;  (* size of associated buffer *)
    ifcu_req : UNTRACED REF struct_ifreq;
  END;

  struct_ifdevea = RECORD
    ifr_name   : ARRAY [0 .. 15] OF CHAR;  (* if name, e.g.  "en0" *)
    default_pa : ARRAY [0 .. 5] OF Byte;   (* default hardware address *)
    current_pa : ARRAY [0 .. 5] OF Byte;   (* current physical address *)
  END;

CONST
  IFD = Shift(And(BYTESIZE(struct_ifdevea), Unix.IOCPARM_MASK), 16);
  IFC = Shift(And(BYTESIZE(struct_ifconf), Unix.IOCPARM_MASK), 16);

  SIOCGIFCONF   = Or(Unix.SIOCGIFCONF, IFC);   (* Get ifnet ls.*)
  SIOCRPHYSADDR = Or(Unix.SIOCRPHYSADDR, IFD); (* Read phy.  ad.*)

EXCEPTION Failure;

PROCEDURE Get (): T =
  <*FATAL Failure*>
  VAR id: T;
  BEGIN
    IF CanGet (id)
      THEN RETURN id;
      ELSE RAISE Failure;
    END;
  END Get;

PROCEDURE CanGet (VAR(*OUT*) id: T): BOOLEAN =
  VAR
    hostname: ARRAY [0 .. 33] OF Ctypes.char;
    hostent : Unetdb.struct_hostent_star;
    list    : struct_ifconf;
    buf     : ARRAY [0 .. 10] OF struct_ifreq;
    req     : struct_ifdevea;
    s       : INTEGER;
  BEGIN
    (* try to find an ethernet hardware address *)

    s := Usocket.socket(Usocket.AF_UNIX, Usocket.SOCK_STREAM,
                        Usocket.PF_UNSPEC);
    IF (s >= 0) THEN
      list.ifc_len := BYTESIZE(buf);
      list.ifcu_req := ADR(buf[0]);
      IF Unix.ioctl(s, SIOCGIFCONF, ADR(list)) # -1 THEN
        FOR i := 0 TO list.ifc_len DIV BYTESIZE(struct_ifreq) - 1 DO
          req.ifr_name := buf[i].ifr_name;
          IF (Unix.ioctl(s, SIOCRPHYSADDR, ADR(req)) # -1)
            AND (req.default_pa[0] # 0 OR req.default_pa[1] # 0) THEN
            id.r := req.default_pa;
            RETURN TRUE;
          END;
        END;
      END;
    END;


    (* try using the machine's internet address *)

    IF Unix.gethostname(ADR(hostname[0]), 32) = 0 THEN
      hostent := Unetdb.gethostbyname(ADR(hostname[0]));
      IF (hostent # NIL) AND (hostent.h_length = 4) THEN
        TYPE Ptr = UNTRACED REF ARRAY [0 .. 3] OF Byte;
        VAR p : Ptr := LOOPHOLE (hostent.h_addr_list^, Ptr); BEGIN
          id.r[0] := 0;
          id.r[1] := 0;
          id.r[2] := p[0];
          id.r[3] := p[1];
          id.r[4] := p[2];
          id.r[5] := p[3];
        END;
        RETURN TRUE;
      END;
    END;

    (* failed *)
    id.r[0] := 0;
    id.r[1] := 0;
    id.r[2] := 0;
    id.r[3] := 0;
    id.r[4] := 0;
    id.r[5] := 0;
    RETURN FALSE;
  END CanGet;

BEGIN
END MachineIDPosix.
