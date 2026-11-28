(* See file COPYRIGHT-CMASS for details. *)

(* Checked signed integer arithmetic, for "-OverflowChk".

   Integer overflow is not a checked runtime error in Modula-3: the
   required interface FloatMode lets an implementation Ignore it, set a
   flag, or Trap.  CM3 ignores it, and the result wraps around.  With
   "-OverflowChk" (Host.doOverflowChk), INTEGER and LONGINT arithmetic
   instead aborts with RuntimeError.IntegerOverflow when the exact result
   is not representable.

   Each procedure does what the corresponding CG operation does, then
   checks.  The checks use only ordinary arithmetic and comparisons, and
   no jumps, so they are safe in the middle of an expression; and they
   rely on the operation wrapping around, as it does in every backend.
   Word arithmetic, address arithmetic and FOR loop stepping are not
   checked.

   To turn the checks on for one package, put M3_OPTIONS += "-OverflowChk"
   in its m3makefile; for everything a cm3 builds, pass
   -DM3_OVERFLOW_CHECKS or set M3_OVERFLOW_CHECKS in cm3.cfg (see
   cm3cfg.common).  In checked mode an overflowing constant expression is
   an error, not a warning. *)

INTERFACE OverflowChk;

IMPORT CG;

PROCEDURE Signed (t: CG.Type): BOOLEAN;
(* TRUE if t is the stack type of INTEGER or LONGINT, i.e. arithmetic on
   t is checked when Host.doOverflowChk. *)

PROCEDURE Add      (t: CG.IType);  (* s1.t := s1.t + s0.t ; pop *)
PROCEDURE Subtract (t: CG.IType);  (* s1.t := s1.t - s0.t ; pop *)
PROCEDURE Multiply (t: CG.IType);  (* s1.t := s1.t * s0.t ; pop *)
PROCEDURE Negate   (t: CG.IType);  (* s0.t := - s0.t *)
PROCEDURE Abs      (t: CG.IType);  (* s0.t := ABS (s0.t) *)
PROCEDURE Div      (t: CG.IType;  a, b: CG.Sign);
                                   (* s1.t := s1.t DIV s0.t ; pop *)
(* Each aborts with RuntimeError.IntegerOverflow if the exact result is
   outside t's range.  (MOD needs no check: x MOD y is always between 0
   and y.) *)

END OverflowChk.
