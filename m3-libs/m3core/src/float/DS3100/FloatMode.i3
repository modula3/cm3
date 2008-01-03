(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Last modified on Thu May 13 13:20:55 PDT 1993 by mcjones    *)
(*      modified on Thu Apr 29 15:50:37 PDT 1993 by muller     *)
(*      modified on Wed Sep 25 20:30:16 1991 by kalsow         *)

(* The interface "FloatMode" allows you to test the behavior of
   rounding and of numerical exceptions.  On some implementations it
   also allows you to change the behavior, on a per-thread basis. *)

INTERFACE FloatMode (* FOR DS3100 *);

CONST IEEE = TRUE;
(* "TRUE" for fully-compliant IEEE implementations. *)

EXCEPTION Failure;
(* Raised by attempts to set modes that are not supported by the
   implementation. *)

TYPE RoundingMode =
  {NearestElseEven, TowardMinusInfinity, TowardPlusInfinity,
   TowardZero, NearestElseAwayFromZero, IBM370, Other};
(* Rounding modes.  The first four are the IEEE modes. *)

CONST RoundDefault = RoundingMode.NearestElseEven;
(* Implementation-dependent: the default mode for rounding arithmetic
   operations, used by a newly forked thread.  This also specifies the
   behavior of the "ROUND" operation in half-way cases. *)

PROCEDURE SetRounding(md: RoundingMode) RAISES {Failure};
(* Change the rounding mode for the calling thread to "md", or raise
   the exception if this cannot be done.  This affects the implicit
   rounding in floating-point operations; it does not affect the
   "ROUND" operation.  Generally this can be done only on IEEE
   implementations and only if "md" is an IEEE mode. *)

PROCEDURE GetRounding(): RoundingMode;
(* Return the rounding mode for the calling thread. *)

TYPE Flag = {Invalid, Inexact, Overflow, Underflow,
  DivByZero, IntOverflow, IntDivByZero};

(* Associated with each thread is a set of boolean status flags
   recording whether the condition represented by the flag has
   occurred in the thread since the flag was last reset.  The meaning
   of the first five flags is defined precisely in the IEEE floating
   point standard; roughly they mean:

\begin{quote}   
   "Invalid" = invalid argument to an operation.  

   "Inexact" = an operation produced an inexact result.

   "Overflow" = a floating-point operation produced a result whose
   absolute value is too large to be represented.

   "Underflow" = a floating-point operation produced a result whose
   absolute value is too small to be represented.

   "DivByZero" = floating-point division by zero.

   The meaning of the last two flags is:

   "IntOverflow" = an integer operation produced a result whose
   absolute value is too large to be represented.

   "IntDivByZero" = integer "DIV" or "MOD" by zero.
\end{quote}
*)

CONST NoFlags = SET OF Flag {};

PROCEDURE GetFlags(): SET OF Flag;
(* Return the set of flags for the current thread. *)

PROCEDURE SetFlags(s: SET OF Flag)
  : SET OF Flag RAISES {Failure};
(* Set the flags for the current thread to "s", and return their
   previous values. *)

PROCEDURE ClearFlag(f: Flag);
(* Turn off the flag "f" for the current thread. *)  

EXCEPTION Trap(Flag);

TYPE Behavior = {Trap, SetFlag, Ignore};

(* The behavior of an operation that causes one of the flag conditions
   is either:

\begin{quote}  
   "Ignore" = return some result and do nothing.

   "SetFlag" = return some result and set the condition flag.  For
   IEEE implementations, the result of the operation is defined by the
   standard.
        
   "Trap" = possibly set the condition flag; in any case raise the
   "Trap" exception with the appropriate flag as the argument.
\end{quote}
*)

PROCEDURE SetBehavior(f: Flag; b: Behavior) RAISES {Failure};
(* Set the behavior of the current thread for the flag "f" to be "b",
   or raise "Failure" if this cannot be done. *)

PROCEDURE GetBehavior(f: Flag): Behavior;
(* Return the behavior of the current thread for the flag "f". *)


(*----------------------------------------------------------------- misc. ---*)

TYPE ThreadState = RECORD
    behavior: ARRAY Flag OF Behavior;
    sticky: ARRAY Flag OF BOOLEAN;
  END;
(* One copy per thread, saved by the thread implementation. *)

PROCEDURE InitThread(VAR s: ThreadState);
(* Initialize the current thread to the default floating-point state. *)

(* DECstation/Ultrix defaults:
     RoundingMode = Nearest;

     (X => default behavior, @ => allowed by SetBehavior,  . => not allowed)

     Flag        Ignore SetFlag Trap
     --------    -------------------------
     Invalid        @      X      @
     Inexact        @      X      @
     Overflow       @      X      @
     Underflow      @      X      @
     DivByZero      @      X      @
     IntOverflow    X      .      .
     IntDivByZero   .      .      X
*)

END FloatMode.
