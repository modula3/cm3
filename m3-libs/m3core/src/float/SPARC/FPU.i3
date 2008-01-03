(* Copyright (C) 1992, Xerox                                                 *)
(* All rights reserved.                                                      *)

(* Last modified on Tue Sep 21 15:40:33 PDT 1993 by kalsow                   *)
(*      modified on Fri May  7 14:51:18 PDT 1993 by muller                   *)
(*      modified on Wed Sep 25 00:33:01 1991 by goldberg@xerox.parc.com      *)

INTERFACE FPU;

IMPORT Ctypes, FloatMode;

<* EXTERNAL scalbn *> PROCEDURE scalb(x: LONGREAL; n: INTEGER): LONGREAL;
<* EXTERNAL *> PROCEDURE ilogb(x: LONGREAL): INTEGER;
<* EXTERNAL *> PROCEDURE logb(x: LONGREAL): LONGREAL;
<* EXTERNAL *> PROCEDURE nextafter(x, y: LONGREAL): LONGREAL;
<* EXTERNAL *> PROCEDURE copysign(x, y: LONGREAL): LONGREAL;
<* EXTERNAL *> PROCEDURE finite(x: LONGREAL): BOOLEAN;
<* EXTERNAL *> PROCEDURE isnan(x: LONGREAL): BOOLEAN;
<* EXTERNAL *> PROCEDURE fp_class(x: LONGREAL): INTEGER;
<* EXTERNAL *> PROCEDURE signbit(x: LONGREAL): INTEGER;
<* EXTERNAL *> PROCEDURE sqrt(x: LONGREAL): LONGREAL;
<* EXTERNAL *> PROCEDURE ieee_flags(action, mode, in: Ctypes.char_star;
				    VAR out: Ctypes.char_star): INTEGER;

TYPE
  SigFPEHandler = PROCEDURE(sig, code: INTEGER; scp, addr: ADDRESS)
		      RAISES {FloatMode.Trap};

<* EXTERNAL *> PROCEDURE ieee_handler(action, exception: Ctypes.char_star;
				      hdl: SigFPEHandler): INTEGER;

<* EXTERNAL *> PROCEDURE sigfpe(code: INTEGER; hdl: SigFPEHandler): INTEGER;

END FPU.
