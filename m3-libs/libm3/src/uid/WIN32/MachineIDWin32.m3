(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Last modified on Fri Feb 10 11:43:49 PST 1995 by kalsow     *)
(*      modified on Thu Jul 15 16:41:23 PDT 1993 by swart      *)
(*      modified on Thu May  6 13:27:58 PDT 1993 by mjordan    *)

UNSAFE MODULE MachineIDWin32 EXPORTS MachineID;

IMPORT NB30;

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
    ncb          : NB30.NCB;
    lanaEnum     : NB30.LANA_ENUM;
    adaptorStatus: NB30.ADAPTER_STATUS;
  BEGIN
    ncb.ncb_command := NB30.NCBENUM;
    ncb.ncb_buffer := ADR(lanaEnum);
    ncb.ncb_length := BYTESIZE(lanaEnum);
    EVAL NB30.Netbios(ADR(ncb));
    IF (ncb.ncb_retcode = 0) AND (lanaEnum.length >= 1) THEN
      ncb.ncb_command := NB30.NCBRESET;
      ncb.ncb_lana_num := lanaEnum.lana[0];
      ncb.ncb_lsn := 0;
      ncb.ncb_num := 0;
      ncb.ncb_buffer := NIL;
      ncb.ncb_length := 0;
      EVAL NB30.Netbios(ADR(ncb));
      IF (ncb.ncb_retcode = 0) THEN
        ncb.ncb_command := NB30.NCBASTAT;
        ncb.ncb_callname[0] := ORD('*');
        ncb.ncb_callname[1] := ORD('\000');
        ncb.ncb_buffer := ADR(adaptorStatus);
        ncb.ncb_length := BYTESIZE(adaptorStatus);
        EVAL NB30.Netbios(ADR(ncb));
        IF (ncb.ncb_retcode = 0) THEN
          id.r := LOOPHOLE(adaptorStatus.adapter_address,
                       ARRAY [0 .. 5] OF BITS 8 FOR [0 .. 255]);
          RETURN TRUE;
        END;
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
END MachineIDWin32.

(*
PROCEDURE Get (): T =
  VAR
    uid: WinRPC.UUID;
    id : T;
  VAR id: T;
  BEGIN
    WinRPC.UuidCreate(ADR(uid));
    id.r := SUBARRAY(uid.Data4, 2, 6);
    RETURN id;
  END GetMachineID;
*)
