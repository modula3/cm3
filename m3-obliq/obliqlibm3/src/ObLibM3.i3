(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

INTERFACE ObLibM3;
IMPORT SynLocation, ObValue, Rd, Wr, Process;

  PROCEDURE PackageSetup();
  (* To be called at least once before any other use of the obliqlibm3 package. *)

(* ============ "rd" package ============ *)

  TYPE
    ValRd =
      ObValue.ValAnything BRANDED OBJECT
        rd: Rd.T;
      OVERRIDES Is := IsRd; Copy := CopyRd;
      END;

  PROCEDURE IsRd(self: ValRd; other: ObValue.ValAnything): BOOLEAN;
  PROCEDURE CopyRd(self: ObValue.ValAnything; tbl: ObValue.Tbl;
    loc: SynLocation.T): ObValue.ValAnything RAISES {ObValue.Error};
    (* Shares the reader *)

(* ============ "wr" package ============ *)

  TYPE
    ValWr =
      ObValue.ValAnything BRANDED OBJECT
        wr: Wr.T;
      OVERRIDES Is := IsWr; Copy := CopyWr;
      END;

  PROCEDURE IsWr(self: ValWr; other: ObValue.ValAnything): BOOLEAN;
  PROCEDURE CopyWr(self: ObValue.ValAnything; tbl: ObValue.Tbl;
    loc: SynLocation.T): ObValue.ValAnything RAISES {ObValue.Error};
    (* Shares the writer *)

(* ============ "process" package ============ *)

  TYPE
    ValProc =
      ObValue.ValAnything BRANDED OBJECT
        proc: Process.T;
        in: ValWr;
        out: ValRd;
        err: ValRd;
      OVERRIDES Is := IsProc; Copy := CopyProc;
      END;

  PROCEDURE IsProc(self: ValProc; other: ObValue.ValAnything): BOOLEAN;
  PROCEDURE CopyProc(self: ObValue.ValAnything; tbl: ObValue.Tbl;
    loc: SynLocation.T): ObValue.ValAnything RAISES {ObValue.Error};
    (* Raises Error *)

END ObLibM3.
