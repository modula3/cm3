(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Mon Jul 25 16:37:19 PDT 1994 by kalsow     *)

(* Many of the routines in the RTHooks interface are exported directly
   by other modules of the runtime:
 
|     PushEFrame, PopEFrame   are in Thread
|     LockMutex, UnlockMutex  are in Thread
|     Allocate*, Dispose*     are in RTAllocator

*)

UNSAFE MODULE RTHooks;

IMPORT RT0, RTException, RTMisc, Text, Word;

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
  "  limititation, these modifications.",
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

(*----------------------------------------------------------------- RAISE ---*)

PROCEDURE Raise (exception: ADDRESS;  arg: ADDRESS) RAISES ANY =
  BEGIN
    RTException.Raise (exception, arg);
  END Raise;

PROCEDURE ResumeRaise (info: ADDRESS) RAISES ANY =
  TYPE Info = UNTRACED REF RECORD exception, arg: ADDRESS END;
  VAR p := LOOPHOLE (info, Info);
  BEGIN
    RTException.ResumeRaise (p.exception, p.arg);
  END ResumeRaise;

(*----------------------------------------------- builtin TEXT operations ---*)

PROCEDURE Concat (a, b: TEXT): TEXT =
  BEGIN
    RETURN Text.Cat (a, b);
  END Concat;

(*-------------------------------------------------------- runtime errors ---*)

CONST
  msgs = ARRAY [0..9] OF TEXT {
    (* 0 *) "ASSERT failed",
    (* 1 *) "Value out of range",
    (* 2 *) "Subscript out of range",
    (* 3 *) "Incompatible array shapes",
    (* 4 *) "Attempt to dereference NIL",
    (* 5 *) "NARROW failed",
    (* 6 *) "Function did not return a value",
    (* 7 *) "Unhandled value in CASE statement",
    (* 8 *) "Unhandled type in TYPECASE statement",
    (* 9 *) "Stack overflow"
  };

PROCEDURE ReportFault (module: ADDRESS(*RT0.ModulePtr*);  info: INTEGER)=
  VAR
    line : INTEGER       := Word.RightShift (info, 4);
    code : INTEGER       := Word.And (info, 16_f);
    mi   : RT0.ModulePtr := module;
    file : RT0.String    := NIL;
    msg  : TEXT          := "bad error code!";
  BEGIN
    IF (0 <= code) AND (code <= LAST (msgs)) THEN msg := msgs[code]; END;
    IF (mi # NIL) THEN file := mi.file; END;
    RTMisc.FatalErrorS (file, line, msg);
  END ReportFault;

BEGIN
END RTHooks.
