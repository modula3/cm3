(* Copyright (C) 1992, Digital Equipment Corporation          *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(* Last modified on Mon Oct 17 09:17:55 PDT 1994 by kalsow    *)
(*      Olaf Wagner 16.09.1994                                *)

UNSAFE MODULE Cstdio;

BEGIN
   iF[0] := ADR(sF[0]);
   iF[1] := ADR(sF[1]);
   iF[2] := ADR(sF[2]);
   FOR i := NSTDBUF TO NIOBRW-1 DO
      iF[i] := NIL;
   END;
END Cstdio.
