(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Fri Jun  3 14:00:20 PDT 1994 by mhb    *)
(*      modified on Tue Feb 23 10:34:24 PST 1993 by steveg *)
(*      modified on Tue Feb 16 16:18:28 PST 1993 by johnh  *)
<* PRAGMA LL *>

(* This interface provides a simple programmer interface for
   building algorithm animations, and provides a user interface
   for controlling the the selection of algorithms and views, and
   the execution of algorithms.

   For historical reasons, this interface is called "ZeusPanel";
   the "Zeus" interface is at a lower-level than this interface;
   it is useful for creating multi-view editor applications, as
   well as an algorithm animation system. *)

INTERFACE ZeusPanel;

IMPORT Algorithm, FormsVBT, Rsrc, Thread, View;

PROCEDURE Interact (
    title: TEXT := "ZEUS Control Panel";
    path : Rsrc.Path := NIL);
<* LL = 0 *>
(* Called once; doesn't return until the user deletes the control
   panel.  After installing a Zeus control panel in Trestle,
   tries to restore the state to the last time Zeus was exited
   (stored in StateDir) and then awaits user commands.  When the
   user deletes the control panel, either by a WM gesture or
   using the Quit button from the Zeus menu, Zeus tries to
   snapshot the state into StateDir before returning. *)

PROCEDURE GetPath (): Rsrc.Path;
<* LL is arbitrary *>
(* Return the "path" that was specified when "Interact" was
   called.  There is no way to change the path dynamically.  The
   path is mostly part of "ZeusPanel" as a convenience for
   application-writers to share a single path among multiple
   modules.  Also, it's used within "ZeusPanel" to open forms when
   "NewForm" is called. *)

PROCEDURE SetTitle (title: TEXT);
<* LL = 0 *>
(* Use "title" in the control panel's chassis.  Typically not
   called, since a title can be set with a parameter to
   "Interact".  However, the title can be changed dynamically
   (for example, to reflect the name of the current algorithm or
   input). *)

PROCEDURE GetAnimationTime (): REAL;
<* LL = VBT.mu *>
(* Returns the setting of the animation slider.  This is the
   amount of time that a ``one-second animation'' should take.
   By convention, each event takes one second. *)

PROCEDURE ReportError (text: TEXT); 
<* LL = VBT.mu *>
(* Display the specified text as an error message in the control panel. *)

PROCEDURE ReportErrorC (report: BOOLEAN; text: TEXT); 
<* LL = VBT.mu *>
(* IF report THEN ReportError(t) *)

PROCEDURE NewForm (name: TEXT; path: Rsrc.Path := NIL):
  FormsVBT.T;
<* LL arbitrary *>
(* Returns a form stored in the resource "name" using the
   resource "path".  However, if "path" is "NIL", then it uses
   the value returned by "GetPath".  It's a runtime error if
   there are any problems reading the form.  Most causal clients
   will read .fv files using this procedure. *)

TYPE
  NewAlgProc = PROCEDURE (): Algorithm.T;

PROCEDURE RegisterAlg (proc: NewAlgProc; name, sessName: TEXT);
<* LL=0 *>
(* Register an algorithm.  "name" is the name of the algorithm.  "sessName"
   is the name of the session to which the algorithm belongs, that is, the
   basename of the .evt file.  "proc" is a NewAlgProc, a procedure that
   returns an initialized instance of the algorithm.  This means that
   "proc" must call the init() method of the algorithm.  It is a checked
   runtime error if an algorithm with the same name has already been
   registered in the same session. *)


TYPE
  NewViewProc = PROCEDURE (): View.T;

PROCEDURE RegisterView (proc          : NewViewProc;
                        name, sessName: TEXT;
                        alertable     : BOOLEAN       := FALSE;
                        sample        : View.T        := NIL    );
<* LL=0 *> 
(* Register a view.  "name" is the name of the view, and
   "sessName" is the name of the session to which the algorithm
   belongs, that is, the basename of the ".evt" file.  It is a
   checked runtime error if a view with the same name has already
   been registered in the same session. "proc" is a procedure
   that returns an initialized instance of the view.  This means
   that "proc" must call the "init" method of the view.  The
   "proc" can also return "NIL" to indicate that a new view could
   not be created for some reason (e.g., if the view uses a
   remote object that cannot be accessed).  In such cases, "proc"
   should display an error message to the user using
   "ZeusPanel.ReportError".  If "alertable = TRUE", then this
   view is willing to receive an alert as a signal to terminate
   the current event.  If "sample" is non-NIL, it is an
   unitialized instance of the view, passed in so the Zeus
   implementation can call its "isCompat" method.  Use this
   parameter if you don't want Zeus to create an uninstalled
   instance of the view, e.g., if "proc" has side effects like
   creating windows. *)

PROCEDURE SetSessTitle (sessName, sessTitle: TEXT); 
<* LL=0 *>
(* The default title under which a session is listed in the "Sessions" menu
   is its name, that is, the basename of the .evt file.  Use this procedure
   to change the title of session "sessName" to "sessTitle." This procedure
   creates a session named "sessName," if none existed previously. *)

PROCEDURE Pause (alg: Algorithm.T; msg: TEXT := NIL)
  RAISES {Thread.Alerted};
<* LL=0, S=Running *>
(* This procedure may only be called from "alg"s run method.  It
   returns after the user issues a "Resume" or "Step" command, or
   it may raise Thread.Alerted (for instance, if the algorithm is
   aborted).  The "msg", if non-"NIL", is displayed in the
   control panel's status area. *)

PROCEDURE Abort();  
<* LL=VBT.mu *>
(* This procedure acts as if the user had pressed the "Abort" button.
   Be careful: calling this procedure does not cause the algorithms to
   stop running immediately. The algorithms will stop the next time
   that Zeus gets control, which is typically the next time an event
   happens. *)

PROCEDURE StartFeedback (alg: Algorithm.T) RAISES {Thread.Alerted};
<* LL=0, S=Running *>
(* This procedure may only be called from "alg"s run method.  It returns
   after "alg" has called EndFeedback or it may raise Thread.Alerted (for
   instance, if the algorithm is aborted).

   The effect of this procedure is to suspend the algorithm and allow
   feedback events (as if the user had clicked Pause).  When this procedure
   returns, the session continues under interpreter control (returns to the
   Running state).  This procedure is a noop if there already is a
   'pending' StartFeedback for this alg.

   StartFeedback calls the reactivity methods of the algorithm and views to
   enable and disable feedback events. *)

PROCEDURE EndFeedback(alg: Algorithm.T) RAISES {Thread.Alerted};
<* LL=VBT.mu, S=Paused *>
(* This procedure signals a previous call to StartFeedback to return. 
   This procedure is typically called from an algorithm's Feedback 
   method. *) 

END ZeusPanel.
