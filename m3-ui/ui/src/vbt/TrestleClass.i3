(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman, Mark Manasse and Greg Nelson           *)
(* Last modified on Mon Mar  6 17:44:17 PST 1995 by msm      *)
(*      modified on Mon Feb 24 13:58:25 PST 1992 by muller   *)
(*      modified on Wed Oct 23  0:26:59 PDT 1991 by gnelson  *)
(*      modified on Thu Apr 12 15:21:37 PDT 1990 by steveg   *)
<*PRAGMA LL*>

INTERFACE TrestleClass;

IMPORT Trestle, VBT, Point, Rect, Region, ScrnPixmap, ScrnColorMap,
  Thread;
FROM TrestleComm IMPORT Failure;
FROM Trestle IMPORT ScreenID;

REVEAL Trestle.T <: Public;

VAR connectMu: MUTEX;  <* LL = VBT.mu + epsilon *>
 closeMu: MUTEX; <* LL maximal *>

TYPE
  RootVBT <: VBT.Split;
  Public = RootVBT OBJECT
    closed := FALSE <* LL = closedMu *>
  METHODS
    attach(v: VBT.T) RAISES {Failure};
    (* tr.attach(v) attaches v to tr, leaving v invisible. *)
    (* It must make v a descendant of tr even if it raises Failure *)
    (* v must be attached for the rest of the methods *)
    decorate(v: VBT.T; old, new: Decoration) RAISES {Failure};
    (* new is on v's prop list. *)
    iconize(v: VBT.T) RAISES {Failure};
    overlap(v: VBT.T; id: ScreenID; READONLY nw: Point.T)
      RAISES {Failure};
    moveNear(v, w: VBT.T) RAISES {Failure};
    installOffscreen(v: VBT.T; width, height: CARDINAL; 
      preferredScreenType: VBT.ScreenType) RAISES {Failure};
    setColorMap(v: VBT.T; cm: ScrnColorMap.T);
    getScreens(): Trestle.ScreenArray RAISES {Failure};
    captureScreen(
      id: ScreenID;
      READONLY clip: Rect.T;
      VAR (* out *) br: Region.T)
      : ScrnPixmap.T
    RAISES {Failure};
    allCeded(): BOOLEAN RAISES {Failure};
    tickTime(): INTEGER;
    swap(v, w: VBT.T) RAISES {Failure};
    getName(v: VBT.T): TEXT RAISES {Failure};
    setScreens(sa: Trestle.ScreenArray) RAISES {Failure};
    nameList(nm: TEXT): REF ARRAY OF TEXT RAISES {Failure};
    moveNearByName(v: VBT.T; nm: TEXT)  RAISES {Failure};
    swapByName(v: VBT.T; nm: TEXT) RAISES {Failure};
    deleteByName(nm: TEXT) RAISES {Failure};
    takeOver(id: ScreenID; v: VBT.T) RAISES {Failure};
    (* Initially v is attached *)
    restore(id: ScreenID; v: VBT.T) RAISES {Failure};
    takeOverMouse(id: ScreenID; v: VBT.T) RAISES {Failure};
    releaseMouse(id: ScreenID; v: VBT.T) RAISES {Failure};
    setHighlight(
      id: ScreenID;
      READONLY r: Rect.T;
      border: CARDINAL)
      RAISES {Failure};
    addParent(prnt: VBT.T; id: ScreenID) RAISES {Failure};
    remParent(prnt: VBT.T; id: ScreenID) RAISES {Failure};
    warpCursor(id: ScreenID; READONLY pt: Point.T) RAISES {Failure};
    (* getConfig(id: ScreenID; p: Trestle.ConfigClosure) 
      RAISES {Failure};
    setConfig(id: ScreenID; c: Trestle.Config) RAISES {Failure}; *)
    lastCeded(): VBT.TimeStamp RAISES {Failure};
    (* getParameters(): Trestle.Parameters RAISES {Failure};
    setParameters(p: Trestle.Parameters) RAISES {Failure}; *)
    (* the following methods are to support telepointing on shared windows;
       all have LL = VBT.mu.  For trestleId, the default display name is
       returned when v = NIL; otherwise a specific displayname for the
       display and screen where v appears is returned. *)
    trestleId(v: VBT.T := NIL): TEXT;
    windowId(v: VBT.T): TEXT;
    updateChalk(v: VBT.T; chalk: TEXT);
    updateBuddies(v: VBT.T; READONLY trsls, ids: ARRAY OF TEXT);
    delete(ch: VBT.T); (* default in a T calls Split.Delete(t, ch) *)
  END;

TYPE Decoration = BRANDED REF RECORD
    inst, windowTitle, iconTitle, applName: TEXT;
    bgColorR, bgColorG, bgColorB: REAL;
    iconWindow: VBT.T
  END;

TYPE
  InstallRef = REF RECORD
                     installCount                   := 0;
                     c           : Thread.Condition
                   END;
  (* There is one of these on the property set of every installed
     VBT, with installCount > 0. *)

(* Each different "type" of Trestle.T (e.g., an XClient.T)
   registers a ConnectClosure; these are tried in turn by
   Trestle.Connect. *)

TYPE ConnectClosure = 
  OBJECT METHODS 
    <* LL.sup <= TrestleClass.connectMu *>
    apply(inst: TEXT; localOnly: BOOLEAN; VAR (*OUT*) t: Trestle.T): BOOLEAN
  END;
(* If cc is a ConnectClosure, cc.apply(inst, localOnly, t) is like t
   := Trestle.Connect(inst, localOnly); RETURN TRUE, provided that inst
   and localOnly are acceptable to cc; otherwise it is equivalent to
   t := NIL; RETURN FALSE.  When cc.apply is called, LL.sup <=
   TrestleClass.connectMu.  *)

PROCEDURE RegisterConnectClosure(cc: ConnectClosure);
(* Add cc to the list of ConnectClosures that will be tried
   by Connect. LL.sup < VBT.mu *)

PROCEDURE Connect(
    inst: TEXT := NIL;
    localOnly: BOOLEAN := FALSE)
    : Trestle.T
  RAISES {Failure}; <* LL.sup <= TrestleClass.connectMu *>
  (* Implements Trestle.Connect. *)
  
END TrestleClass.
