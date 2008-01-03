(* Copyright (C) 1993, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* Last modified on Sun Sep 25 18:55:55 PDT 1994 by heydon   *)
(*      modified on Thu Nov 11 10:43:51 PST 1993 by kalsow   *)

INTERFACE Compl;

IMPORT BasicCtypes, Completion;

(* used to store info about expected replies from the X server after
   XShmPutImage's *)

CONST Brand = "Compl";

TYPE
  T = OBJECT
        serial    : BasicCtypes.unsigned_long_int;
        completion: Completion.T;
      END;

(* free list management *)
PROCEDURE Get (): T;
PROCEDURE Free (t: T);

END Compl.
