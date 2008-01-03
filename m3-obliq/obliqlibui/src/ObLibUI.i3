(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

INTERFACE ObLibUI;
IMPORT SynLocation, ObValue, Color, VBT;

  PROCEDURE PackageSetup();
  (* To be called at least once before any other use of the obliqlibui package. *)

  TYPE 
    ValColor =
      ObValue.ValAnything BRANDED OBJECT
        color: Color.T;
      OVERRIDES Is := IsColor; Copy := CopyColor;
      END;

  PROCEDURE IsColor(self: ValColor; other: ObValue.ValAnything): BOOLEAN;
  PROCEDURE CopyColor(self: ObValue.ValAnything; tbl: ObValue.Tbl;
    loc: SynLocation.T): ObValue.ValAnything RAISES {ObValue.Error};

  TYPE
    ValVBT = ObValue.ValAnything BRANDED OBJECT
      vbt: VBT.T;
    OVERRIDES Is := IsVBT; Copy := CopyVBT;
    END;
  PROCEDURE IsVBT(self: ValVBT; other: ObValue.ValAnything): BOOLEAN;
  PROCEDURE CopyVBT(self: ObValue.ValAnything; tbl: ObValue.Tbl;
    loc: SynLocation.T): ObValue.ValAnything RAISES {ObValue.Error};
    (* Raises Error *)

  TYPE 
    ValForm = ValVBT BRANDED OBJECT END;


END ObLibUI.


