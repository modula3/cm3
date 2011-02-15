(*
   Copyright (c) 2011 Generation Capital, Ltd.
   All rights reserved.

   Permission to use, copy, modify, and distribute this software and
   its documentation for any purpose and without fee is hereby
   granted, provided that the above copyright notice appear in all
   copies.  Generation Capital, Ltd. makes no representations about
   the suitability of this software for any purpose. It is provided
   "as is" without express or implied warranty.
*)

INTERFACE QPromise;

FROM Quake IMPORT Error;
IMPORT Thread, Process;

(* 
   deferred execution in Quake 
   see "delayed evaluation" in Abelson and Sussman,
   Structure and Interpretation of Computer Programs,
   for the inspiration to this.
*)

TYPE
  T = OBJECT METHODS
    fulfil() : Process.ExitCode RAISES { Error, Thread.Alerted } 
    (* force the promise *)
  END;

  Empty <: T;
  (* empty is a promise that does nothing when forced *)

CONST Brand = "QPromise";

CONST Equal : PROCEDURE(a, b : T) : BOOLEAN = NIL;

END QPromise.
