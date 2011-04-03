INTERFACE FloatModeC;

(* interface to C glue for C99 implementation of FloatMode *)
(* 
   Copyright (c) 2011 Generation Capital, Ltd.  All rights reserved.

   Permission to use, copy, modify, and distribute this software
   and its documentation for any purpose and without fee is hereby
   granted, provided that the above copyright notice appear in all
   copies.  Generation Capital, Ltd. makes no representations
   about the suitability of this software for any purpose. It is
   provided "as is" without express or implied warranty.

   Author : Mika Nystrom <mika@alum.mit.edu> 
   April 2011

*)

FROM Ctypes IMPORT int;

(* -------------------- constant-getters follow -------------------- *)

<*EXTERNAL FloatModeC__get_FE_TONEAREST*>
PROCEDURE get_FE_TONEAREST() : int;

<*EXTERNAL FloatModeC__get_FE_DOWNWARD*>
PROCEDURE get_FE_DOWNWARD() : int;

<*EXTERNAL FloatModeC__get_FE_UPWARD*>
PROCEDURE get_FE_UPWARD() : int;

<*EXTERNAL FloatModeC__get_FE_TOWARDZERO*>
PROCEDURE get_FE_TOWARDZERO() : int;

<*EXTERNAL FloatModeC__fegetround*>
PROCEDURE fegetround() : int;

<*EXTERNAL FloatModeC__fesetround*>
PROCEDURE fesetround(round : int);

(* -------------------- exception flags follow -------------------- *)

<*EXTERNAL FloatModeC__get_FE_DIVBYZERO*>
PROCEDURE get_FE_DIVBYZERO() : int;

<*EXTERNAL FloatModeC__get_FE_INEXACT*>
PROCEDURE get_FE_INEXACT() : int;

<*EXTERNAL FloatModeC__get_FE_INVALID*>
PROCEDURE get_FE_INVALID() : int;

<*EXTERNAL FloatModeC__get_FE_OVERFLOW*>
PROCEDURE get_FE_OVERFLOW() : int;

<*EXTERNAL FloatModeC__get_FE_UNDERFLOW*>
PROCEDURE get_FE_UNDERFLOW() : int;

<*EXTERNAL FloatModeC__feclearexcept*>
PROCEDURE feclearexcept(excepts : int);

<*EXTERNAL FloatModeC__feraiseexcept*>
PROCEDURE feraiseexcept(excepts : int);

<*EXTERNAL FloatModeC__fetestexcept*>
PROCEDURE fetestexcept(excepts : int) : int;

END FloatModeC.
