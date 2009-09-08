(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Last modified on Tue Feb 11 15:07:33 PST 1992 by muller                   *)

INTERFACE ThreadEvent;

IMPORT ThreadF;

TYPE
  Kind = {Changed, Running, Deleted};

  T = RECORD
        kind: Kind;
        id: ThreadF.Id := 0;
        state := ThreadF.State.dead; END;

END ThreadEvent.
