(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Mon Jul 25 16:37:19 PDT 1994 by kalsow     *)

(* Many of the routines in the RTHooks interface are exported directly
   by other modules of the runtime:

|     CheckIsType, ScanTypecase  are in RTType
|     PushEFrame, PopEFrame      are in ThreadPosix/ThreadWin32
|     Allocate*, Dispose*        are in RTAllocator
|     Concat, MultiCat           are in TextCat
|     DebugText, DebugInt        are in RTDebug

*)

UNSAFE MODULE RTHooks;

IMPORT RT0, RTException, Word, RuntimeError;

<*UNUSED*> VAR copyright := ARRAY [0..36] OF TEXT {
  "              SRC Modula-3 Non-commercial License",
  "",
  "SRC Modula-3 is distributed by Digital Equipment Corporation ('DIGITAL'),",
  "a corporation of the Commonwealth of Massachusetts.  DIGITAL hereby grants",
  "to you a non-transferable, non-exclusive, royalty free worldwide license",
  "to use, copy, modify, prepare integrated and derivative works of and",
  "distribute SRC Modula-3 for non-commercial purposes, subject to your",
  "agreement to the following terms and conditions:",
  "",
  "  - The SRC Modula-3 Non-commercial License shall be included in the code",
  "    and must be retained in all copies of SRC Modula-3 (full or partial;",
  "    original, modified, derivative, or otherwise):",
  "",
  "  - You acquire no ownership right, title, or interest in SRC Modula-3",
  "    except as provided herein.",
  "",
  "  - You agree to make available to DIGITAL all improvements,",
  "    enhancements, extensions, and modifications to SRC Modula-3 which",
  "    are made by you or your sublicensees and distributed to others and",
  "    hereby grant to DIGITAL an irrevocable, fully paid, worldwide, and",
  "    non-exclusive license under your intellectual property rights,",
  "    including patent and copyright, to use and sublicense, without",
  "    limititation, these modifications.",
  "",
  "  - SRC Modula-3 is a research work which is provided 'as is',",
  "    and  DIGITAL disclaims all warranties",
  "    with regard to this software, including all implied warranties of",
  "    merchantability and fitness of purpose.  In no event shall DIGITAL be",
  "    liable for any special, direct, indirect, or consequential damages or",
  "    any damages whatsoever resulting from loss of use, data or profits,",
  "    whether in an action of contract, negligence or other tortious action,",
  "    arising out of or in connection with the use or performance of this",
  "    software. ",
  "",
  "",
  "              Copyright (C) 1990 Digital Equipment Corporation",
  "                       All Rights Reserved"
   };

(*------------------------------------------------------------ exceptions ---*)

PROCEDURE Raise (ex     : ADDRESS; (*RT0.ExceptionPtr*)
                 arg    : ADDRESS; (*RT0.ExceptionArg*)
                 module : ADDRESS; (*RT0.ModulePtr*)
                 line   : INTEGER) RAISES ANY =
  VAR a: RT0.RaiseActivation;
  BEGIN
    a.exception   := ex;
    a.arg         := arg;
    a.module      := module;
    a.line        := line;
    a.pc          := NIL;
    a.info0       := NIL;
    a.info1       := NIL;
    a.un_except   := NIL;
    a.un_arg      := NIL;
    RTException.Raise (a);
  END Raise;

PROCEDURE ResumeRaise (info: ADDRESS) RAISES ANY =
  BEGIN
    RTException.ResumeRaise (LOOPHOLE (info, UNTRACED REF RT0.RaiseActivation)^);
  END ResumeRaise;

(* peter temp implementation to get link to work *)
PROCEDURE LatchEHReg () : ADDRESS =
  BEGIN
    RETURN NIL;
  END LatchEHReg;

(*-------------------------------------------------------- runtime errors ---*)

CONST
  MIN_RTErr = ORD (FIRST (RuntimeError.T));
  MAX_RTErr = ORD (LAST (RuntimeError.T));

PROCEDURE ReportFault (module: ADDRESS(*RT0.ModulePtr*);  info: INTEGER)
  RAISES ANY =
   (* 1F: see M3CG.RuntimeError, RuntimeError.T *)
  VAR a: RT0.RaiseActivation;  code := Word.And (info, 16_1f);
  BEGIN
    a.exception   := RuntimeError.Self ();
    a.arg         := LOOPHOLE (code, RT0.ExceptionArg);
    a.module      := module;
    a.line        := Word.RightShift (info, 5); (* 5: due to 1F *)
    a.pc          := NIL;
    a.info0       := NIL;
    a.info1       := NIL;
    a.un_except   := NIL;
    a.un_arg      := NIL;
    IF (code < MIN_RTErr) OR (MAX_RTErr < code) THEN
      a.arg := LOOPHOLE (ORD (RuntimeError.T.Unknown), RT0.ExceptionArg);
      a.info0 := LOOPHOLE (code, ADDRESS);
    END;
    RTException.Raise (a);
  END ReportFault;

PROCEDURE AssertFailed (module: ADDRESS(*RT0.ModulePtr*);  line: INTEGER;
                        msg: TEXT) RAISES ANY =
  CONST Err = ORD (RuntimeError.T.AssertFailed);
  VAR a: RT0.RaiseActivation;
  BEGIN
    a.exception   := RuntimeError.Self ();
    a.arg         := LOOPHOLE (Err, RT0.ExceptionArg);
    a.module      := module;
    a.line        := line;
    a.pc          := NIL;
    a.info0       := LOOPHOLE (msg, ADDRESS);
    a.info1       := NIL;
    a.un_except   := NIL;
    a.un_arg      := NIL;
    RTException.Raise (a);
  END AssertFailed;

BEGIN
END RTHooks.
