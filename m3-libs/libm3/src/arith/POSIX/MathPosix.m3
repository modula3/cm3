MODULE MathPosix EXPORTS Math;

(*
  Glue to fix broken prototypes in CM3-d5.11.9

  Author : Mika Nystrom <mika.nystroem@intel.com>
  June, 2024

  Copyright (c) Intel Corporation, 2024.  All rights reserved.

  See COPYRIGHT-INTEL for terms.  

  SPDX-License-Identifier: BSD-3-Clause

 *)

IMPORT MathPosixC;
FROM Ctypes IMPORT int;

PROCEDURE frexp (x: LONGREAL;  VAR exp: int): LONGREAL =
  BEGIN
    exp := MathPosixC.frexp_exp_glue(x);
    RETURN MathPosixC.frexp_result_glue(x)
  END frexp;

PROCEDURE modf (x: LONGREAL; VAR i: LONGREAL): LONGREAL =
  BEGIN
    i := MathPosixC.modf_intpart_glue(x);
    RETURN MathPosixC.modf_result_glue(x);
  END modf;

PROCEDURE cabs (z: Complex): LONGREAL =
  BEGIN
    RETURN MathPosixC.cabs_glue(z.x, z.y)
  END cabs;

BEGIN END MathPosix.
