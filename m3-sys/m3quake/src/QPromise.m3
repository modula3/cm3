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

MODULE QPromise;
IMPORT Process;

REVEAL Empty = T BRANDED Brand OBJECT OVERRIDES fulfil := FulfilNothing END;

PROCEDURE FulfilNothing(<*UNUSED*>p : Empty) : Process.ExitCode = 
  BEGIN RETURN 0 END FulfilNothing;

BEGIN END QPromise.
