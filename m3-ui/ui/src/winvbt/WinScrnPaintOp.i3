(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Mon Jun 26 23:31:44 PDT 1995 by najork                   *)
(*       Created on Tue Jan 17 16:06:56 PST 1995 by najork                   *)


INTERFACE WinScrnPaintOp;

IMPORT Ctypes, ScrnPaintOp, WinDef, WinScreenType;

TYPE
  Mode = {Copy, Opaq, Tran, Swap};

  (* If Modula-3 had variant records, this would be one. *)
  Op = RECORD
    mode: Mode;
    col : ScrnPaintOp.Pixel;
  END;
    
  OpRecord = RECORD
    bop : Op;
    fop : Op;
  (* The raster operations can be derived from "bop" and "fop". 
     They are included into the record simply for caching purposes. *)
    rop2 : Ctypes.int;         (* binary raster operation *)
    brop3: WinDef.DWORD;       (* foreground ternary raster operation *)
    frop3: WinDef.DWORD;       (* background ternary raster operation *)
  END;

PROCEDURE NewOracle(st: WinScreenType.T): ScrnPaintOp.Oracle;

END WinScrnPaintOp.
