(*                                                                           *)
(*  DebugFmtPointer.m3                                                       *)
(*                                                                           *)
(*  Debugging output and aborting the program.                               *)
(*                                                                           *)
(*  Copyright (c) 2002 California Institute of Technology                    *)
(*  All rights reserved.                                                     *)
(*  Department of Computer Science                                           *)
(*  Pasadena, CA 91125.                                                      *)
(*                                                                           *)
(*  Author: Mika Nystrom <mika@cs.caltech.edu>                               *)
(*                                                                           *)
(*  Permission to use, copy, modify, and distribute this software            *)
(*  and its documentation for any purpose and without fee is hereby          *)
(*  granted, provided that the above copyright notice appear in all          *)
(*  copies. The California Institute of Technology makes no representations  *)
(*  about the suitability of this software for any purpose. It is            *)
(*  provided "as is" without express or implied warranty. Export of this     *)
(*  software outside of the United States of America may require an          *)
(*  export license.                                                          *)
(*                                                                           *)
(* $Id$ *)

UNSAFE MODULE DebugFmtPointer EXPORTS Debug;
IMPORT Word, Fmt;

PROCEDURE FmtAddress(p : ADDRESS; base : Fmt.Base) : TEXT =
  BEGIN
    RETURN Fmt.Unsigned(LOOPHOLE(p,Word.T), base)
  END FmtAddress;

PROCEDURE FmtPointer(p : REFANY; base : Fmt.Base) : TEXT =
  BEGIN
    RETURN Fmt.Unsigned(LOOPHOLE(p,Word.T), base)
  END FmtPointer;

BEGIN END DebugFmtPointer.
