(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Last modified on Wed Jan 26 14:37:21 PST 1994 by mcjones    *)

(* "Chf" provides direct access to the VMS-like Condition Handling
   Facility used by DECimage Character Recognition Services. See man
   Chf_intro(3), etc., and /usr/include/ChfDef.h. *)

UNSAFE INTERFACE Chf;

FROM Ctypes IMPORT int, int_star, unsigned_int_star, unsigned_long,
                   void_star;

TYPE
  SigArg = RECORD
    args: int;
    name: int;
    arg1: int
  END;
  SigArg_star = UNTRACED REF SigArg;
  MchArg = RECORD
    args: int;
    frame: void_star;
    depth: int;
    save_R0: int;
    save_r1: int
  END;
  MchArg_star = UNTRACED REF MchArg;

CONST
  ActionContinue = 1;
  ActionResignal = 2328;
  ActionUnwind = 2336;


<* EXTERNAL ChfEstablish *> PROCEDURE Establish(
    new_handler: PROCEDURE(sig_args: SigArg_star; mch_args: MchArg_star): int)
  : int;

<* EXTERNAL ChfMatchCondition *> PROCEDURE MatchCondition(
    argcnt: int;
    cond_val, cond_val_2: unsigned_int_star): int;

<* EXTERNAL ChfRevert *> PROCEDURE Revert(): int;

<* EXTERNAL ChfSigToRet *> PROCEDURE SigToRet(
    sig_args: SigArg_star; mch_args: MchArg_star)
  : unsigned_long;

<* EXTERNAL ChfUnwind *> PROCEDURE Unwind(depadr: int_star; newpc: void_star)
  : unsigned_long;

END Chf.





