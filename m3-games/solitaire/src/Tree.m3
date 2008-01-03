(* Copyright (C) 1992, Xerox Corporation.                      *)
(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the files COPYRIGHT and COPYRIGHT.extras for details.   *)
(*                                                             *)
(* Last Modified On Tue Nov  1 10:11:02 PST 1994 By kalsow     *)
(* Created by David Goldberg, goldberg@parc.xerox.com, 1992    *)

MODULE Tree;

PROCEDURE Equal (<*UNUSED*>t, u: T): BOOLEAN =
  BEGIN
    <* ASSERT FALSE *>
  END Equal;

PROCEDURE Copy (<*UNUSED*>t: T): T =
  BEGIN
    <* ASSERT FALSE *>
  END Copy;

PROCEDURE Compare (<* UNUSED *>t, u: T): [-1 .. 1] =
  BEGIN
    <* ASSERT FALSE *>
  END Compare;

PROCEDURE Hash (<* UNUSED *>t: T; <* UNUSED *>lessThan: CARDINAL): CARDINAL =
  BEGIN
    <* ASSERT FALSE *>
  END Hash;

BEGIN
END Tree.
