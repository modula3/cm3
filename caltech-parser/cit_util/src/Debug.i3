(*                                                                           *)
(*  Debug.i3                                                                 *)
(*                                                                           *)
(*  Debugging output and aborting the program.                               *)
(*                                                                           *)
(*  Copyright (c) 2000 California Institute of Technology                    *)
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

INTERFACE Debug;

PROCEDURE Out(t : TEXT; minLevel : CARDINAL := 10);
PROCEDURE S(t: TEXT; minLevel : CARDINAL := 5);
PROCEDURE Warning(t : TEXT);
PROCEDURE Error(t : TEXT);
PROCEDURE UnNil(text : TEXT) : TEXT;

(* apart from these procedures, the debug level is also set from the
   env. variable DEBUGLEVEL *)

(* if nothing else, it defaults to zero *)

PROCEDURE RaiseLevel(newLevel : CARDINAL);
PROCEDURE LowerLevel(newLevel : CARDINAL);
PROCEDURE SetLevel(newLevel : CARDINAL);
PROCEDURE GetLevel() : CARDINAL;

END Debug.
