(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Jan 26 14:57:03 PST 1993 by mhb                      *)
(*      modified on Tue Jun 16 20:39:55 PDT 1992 by muller                   *)
(*      modified on Mon Jun  8  1:58:39 PDT 1992 by meehan                   *)
(*      modified on Fri Mar 27 02:32:06 1992 by steveg                       *)

MODULE MultiFilter;

IMPORT Filter, MultiClass, VBT;

PROCEDURE Child (v: VBT.T): VBT.T =
  BEGIN
    WITH m = MultiClass.Resolve(v) DO
      IF m = NIL THEN RETURN Filter.Child(v) ELSE RETURN m.succ(NIL) END
    END
  END Child;

PROCEDURE Replace (v, ch: VBT.T): VBT.T =
  VAR old: VBT.T;
  BEGIN
    WITH m = MultiClass.Resolve(v) DO
      IF m = NIL THEN
        RETURN Filter.Replace(v, ch)
      ELSE
        IF ch # NIL THEN MultiClass.BeChild(v, ch); END;
        old := m.succ(NIL);
        m.replace(old, ch);
        IF old # NIL THEN MultiClass.UnChild(v, old) END;
        RETURN old;
      END
    END
  END Replace;

BEGIN
END MultiFilter.
