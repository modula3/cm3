(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Tue Jan 31 14:53:53 PST 1995 by kalsow                   *)
(*      modified on Wed May  4 09:20:37 PDT 1994 by najork                   *)
(*       Created on Sun May  1 17:17:31 PDT 1994 by najork                   *)


MODULE MFFmt;

PROCEDURE Ref (<*UNUSED*> r : REFANY) : TEXT =
  BEGIN
    RETURN "<a Ref>";
  END Ref;

BEGIN
END MFFmt.
