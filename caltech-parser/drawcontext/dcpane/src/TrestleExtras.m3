(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: TrestleExtras.m3,v 1.1.1.1 2001-09-19 11:40:10 wagner Exp $ *)

MODULE TrestleExtras;
IMPORT VBT;
IMPORT Process;
IMPORT Trestle;
IMPORT Thread;

TYPE
  Death = Thread.Closure OBJECT
    v: VBT.T;
  OVERRIDES
    apply := Lurk;
  END;

PROCEDURE Lurk(self: Death): REFANY =
  BEGIN
    Trestle.AwaitDelete(self.v);
    Process.Exit(0);
    <* ASSERT FALSE *>
  END Lurk;

PROCEDURE LazyAwaitDelete(v: VBT.T) =
  BEGIN
    EVAL Thread.Fork(NEW(Death, v := v));
  END LazyAwaitDelete;

BEGIN
END TrestleExtras.
