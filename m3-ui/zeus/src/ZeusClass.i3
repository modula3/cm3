(* Copyright 1992 Digital Equipment Corporation.           *)
(* Distributed only by permission.                         *)
(* Last modified on Mon Feb 15 16:58:14 PST 1993 by johnh*)
(*      modified on Wed May 13 00:49:38 1992 by mhb  *)

(* The definition of the base class for Zeus algorithms and views. *)

INTERFACE ZeusClass;

IMPORT Rd, VBT, Wr;

EXCEPTION Error(TEXT);

TYPE
  T <: Public;
  Private <: VBT.T;
  Public = Private OBJECT
           METHODS
             (* LL = VBT.mu *)
             install    ();
             delete     ();
             snapshot   (wr: Wr.T) RAISES {Error};
             restore    (rd: Rd.T) RAISES {Error};
             config     (state: StateChange; object: T);
             reactivity (on: BOOLEAN);
           END;

TYPE StateChange = {ViewAttached, ViewDetached, LockedBy, UnlockedBy};

(* install(v) is called to attach to the WM all windows that v needs.
   This procedure should be called only once for each v.  Typically, this
   procedure will fork a thread to wait for the installed window(s) to be
   deleted.  At that time, it should perform any necessary cleanup (calling
   Zeus.DetachView before VBT.Discarding itself, if v is a "view," e.g.).

   Special note:  install should NOT put windows on the screen.  The
   restore method will be called immediately after install, and will
   take care of putting up the windows. *)

(* delete(v) is called to delete from the WM all windows v has installed.
   This procedure will never be called more than once; if it is called, it
   is guaranteed that the install method was previously invoked.

   Note that there are two ways for v to be deleted: by the user issuing a
   delete command to the WM, or by invoking v's delete method (in response
   to some command in the control panel).  Thus any necessary cleanup
   should be performed by the thread that install() forks, rather than by
   the delete() method. *)

(* snapshot(v, wr) is called to snapshot the state of v as exactly one
   s-expression written into the supplied writer.  The state
   should include a description and location of any windows that v
   installs, any data it has gotten from the user, and so on. *)

(* restore(v, rd) is passed a reader positioned just before the
   s-expression that was written by v's snapshot method.  The restore
   method should restore the static state of v from this description.  This
   method and v's snapshot method should be inverses.

   A view's restore method should restore the positions of windows.
   The same is true for the restore method of an algorithm that installs
   windows.

   If the restore method is passed rd = NIL as an argument, it should
   restore the alg or view to its initial state, which in the case of a
   view means putting the windows on the screen in some default position. *)

(* config(v, whatChanged, instigator) is called by Zeus whenever Zeus's
   "configuration" changes.  A configuration change happens when a view is
   attached or detached, or a view has acquired or released the edit
   lock. *)

(* reactivity(v, on) is called by ZeusPanel to enable (on=TRUE) or disable
   (on=FALSE) feedback events.  It will be called when the algorithm
   pauses. *)


(* If a subtype overrides one or more of the default methods, the
   overriding method should call the method of the supertype. *)
END ZeusClass.
