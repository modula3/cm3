(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* by Steve Glassman, Mark Manasse and Greg Nelson *)
(* Last modified on Tue May 18 13:18:19 PDT 1993 by msm *)
(* modified on Mon Feb 24 13:59:41 PST 1992 by muller *)
(* modified on Wed Nov 20 16:41:02 PST 1991 by gnelson *)
<*PRAGMA LL*>

UNSAFE INTERFACE XClient;

(* NEW(XClient.T).connect(xs) creates a Trestle.T that displays windows on
   the X server xs. *)

IMPORT Trestle, X, TrestleComm, TrestleOnX, VBT, Point, Ctypes;

TYPE
  T_Public = Trestle.T OBJECT dpy: X.DisplayStar END;
  T <: TrestleOnX.Display;

REVEAL TrestleOnX.Display <: T_Public;

PROCEDURE ToName (trsl: T; a: X.Atom): TEXT RAISES {TrestleComm.Failure};
(* Return the name of the atom a of the X server underlying trsl, avoiding
   a round trip if possible.  LL = trsl. *)

PROCEDURE ToAtom (trsl: T; t: TEXT): X.Atom RAISES {TrestleComm.Failure};
(* Return the atom of the X server underlying trsl whose name is t,
   avoiding a round trip if possible.  LL = trsl. *)

PROCEDURE Init ();
(* Register XClient as a source of Trestle connections.  LL = {} *)

PROCEDURE InnerOverlap (         trsl         : T;
                                 v            : VBT.T;
                                 id           : Trestle.ScreenID;
                        READONLY nw           : Point.T;
                                 knownPosition: BOOLEAN;
                        iconic, userPosition := FALSE;
                        prop, type           := X.None;
                        len, format          := 0;
                        addr: Ctypes.unsigned_char_star := NIL)
  RAISES {TrestleComm.Failure};

END XClient.
