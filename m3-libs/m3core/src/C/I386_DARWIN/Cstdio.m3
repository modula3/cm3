(* Copyright according to COPYRIGHT-CMASS. *)
(* FIXME: copied from FreeBSD3 target. Probably needs to be changed. *)

UNSAFE MODULE Cstdio;

BEGIN
   iF[0] := ADR(sF[0]);
   iF[1] := ADR(sF[1]);
   iF[2] := ADR(sF[2]);
   FOR i := NSTDBUF TO NIOBRW-1 DO
      iF[i] := NIL;
   END;
END Cstdio.
