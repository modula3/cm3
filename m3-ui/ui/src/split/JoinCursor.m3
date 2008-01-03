(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Jan 31 09:42:15 PST 1995 by kalsow  *)
(*      modified on Thu Dec 10 17:44:46 PST 1992 by msm     *)
<*PRAGMA LL*>

MODULE JoinCursor;

IMPORT ScrnCursor, JoinScreen, Cursor, Palette, PlttFrnds,VBTRep;

PROCEDURE New(st: JoinScreen.T): Oracle =
  BEGIN
    RETURN NEW(Oracle, st := st)
  END New;

PROCEDURE Resolve (st: JoinScreen.T; cs: Cursor.T) =
  VAR i: INTEGER; t := st.succ(NIL, i);
  BEGIN
    WHILE t # NIL DO EVAL Palette.ResolveCursor(t, cs); t := st.succ(t, i) END
  END Resolve;

PROCEDURE Apply (           st: JoinScreen.T;
                 <*UNUSED*> cl: Palette.CursorClosure;
                            cs: Cursor.T               ): ScrnCursor.T =
  VAR res := st.cursors[cs.cs];
  BEGIN
    Resolve(st, cs);
    IF res # NIL AND res # PlttFrnds.noCursor THEN RETURN res END;
    RETURN NEW(T, id := 2 * cs.cs + 1, st := st);
  END Apply;

REVEAL
  Oracle = ScrnCursor.Oracle BRANDED OBJECT
             st: JoinScreen.T;
           (*
           OVERRIDES
             load    := Register;
             list    := List;
             lookup  := Lookup;
             builtIn := BuiltIn
           *)
           END;

TYPE
  T = ScrnCursor.T OBJECT
        st: JoinScreen.T;
      (*
      OVERRIDES
        localize := Localize;
        unload   := Unload
      *)
      END;

(*
TYPE
  Registered = T OBJECT
                 raw: ScrnCursor.Raw
               OVERRIDES
                 localize := RegisteredLocalize
               END;

  Lookedup = T OBJECT name: TEXT END;

  Builtin = T OBJECT cs: Cursor.Predefined END;
*)

(*
PROCEDURE Localize (cs: T): ScrnCursor.Raw
  RAISES {TrestleComm.Failure, ScrnCursor.Failure} =
  BEGIN
    IF cs.buddy = NIL THEN RAISE ScrnCursor.Failure END;
    RETURN cs.buddy.localize()
  END Localize;
*)

(*  
PROCEDURE Register (         orc: Oracle;
                    READONLY c  : ScrnCursor.Raw;
                             nm : TEXT             := NIL): ScrnCursor.T
  RAISES {TrestleComm.Failure} =
  BEGIN
    IF orc.st.sts = NIL THEN Crash(); END;
    RETURN orc.st.sts[0].cursor.load(c, nm);
  END Register;
*)

(*
PROCEDURE List (orc: Oracle; pat: TEXT; maxResults: CARDINAL := 1):
  REF ARRAY OF TEXT RAISES {TrestleComm.Failure} =
  BEGIN
    IF orc.st.sts = NIL THEN Crash(); END;
    RETURN orc.st.sts[0].cursor.list(pat, maxResults);
  END List;
*)

(*
PROCEDURE Lookup (orc: Oracle; name: TEXT): ScrnCursor.T =
  BEGIN
    IF orc.st.sts = NIL THEN Crash(); END;
    RETURN orc.st.sts[0].cursor.lookup(name);
  END Lookup;
*)

(*
PROCEDURE BuiltIn (orc: Oracle; cs: Cursor.Predefined): ScrnCursor.T =
  BEGIN
    IF orc.st.sts = NIL THEN Crash(); END;
    IF orc.st.sts[0].cursors[cs] = PlttFrnds.noCursor THEN
      EVAL Palette.ResolveCursor(orc.st.sts[0], Cursor.T{cs});
    END;                        (* if *)
    RETURN orc.st.sts[0].cursors[cs];
  END BuiltIn;
*)

(*
EXCEPTION FatalError;

PROCEDURE Crash () =
  <* FATAL FatalError *>
  BEGIN
    RAISE FatalError;
  END Crash;
*)

BEGIN END JoinCursor.

