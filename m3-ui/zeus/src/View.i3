(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Sun Jul 11 11:22:41 PDT 1993 by mhb   *)
(*      modified on Thu Feb 11 16:05:59 PST 1993 by johnh *)

INTERFACE View;

IMPORT ReactivityVBT, VBT, ZeusClass;

(* A View.T is a subclass of a ZeusClass.T with four additional
   methods: *)

(* init(ch) inserts the "ch" argument as the child of the View.T (which is
   a ReactivityVBT.T), sets the reactivity to Passive, and returns the
   View.T. If "ch" is "NIL", then a child is created that displays 
   the background. The init() method should be called just after the View.T is
   created.  Any user-supplied override to the init method must invoke the
   init method of the supertype. *)

(* isCompat(alg) is called by ZeusPanel to determine whether the view is
   compatible with a particular algorithm.  Compatibility means that the
   view is prepared to display the events that the algorithm generates.
   This method generally just calls ISTYPE; the user should not have to
   override it except under unusual circumstances. *)

(* startrun(v) is called by ZeusPanel just after the user issues the
   command to start running the algorithm, and before the algorithm is
   actually run.  Many subclasses will override the "startrun" method to
   erase anything in the window left over from the previous execution of
   the algorithm.  If a view's startrun procedure is called, ZeusPanel
   guarantees that its endrun procedure will eventually be called. *)

(* endrun(v) is called by ZeusPanel just after the algorithm finishes
   running, either because it was aborted by the user, it crashed, or it
   came to a normal completion.  Typically, it is used by views to "clean
   up" after themselves -- to kill any active threads, etc.  An endrun
   method will only be called if its startrun method had been called
   previously. *)

(* The default ZeusClass "config" method is a noop.  The other ZeusClass
   methods have the following defaults:

   "install" tells Trestle to insert self into the window system,
             but not to put self on the screen; restore will be called
             immediately after install, and will do that.

   "delete" tells Trestle to delete self from the window system

   "snapshot" records location of Trestle window

   "restore" installs and moves self to a previously recorded location

   "reactivity" calls ReactivityVBT.Set to make the view Passive or Active.
             The view is made Dormant whenever it is incompatible with
             the current algorithm, and in this case the reactivity method
             has no effect.

   Subclasses of View.T that are created by zume will be extended with
   methods for each OUTPUT event and UPDATE event in the .evt file.  The
   output methods are invoked with LL < VBT.mu.  Update methods (which are
   called in response to a feedback event) are invoked with LL = VBT.mu.
   User-supplied overrides for OUTPUT and UPDATE methods MUST NOT invoke
   the corresponding supertype methods. *)

REVEAL
   ZeusClass.Private <: ReactivityVBT.T;
   
TYPE
  T <: Public;
  Public = ZeusClass.T OBJECT
           METHODS
             (* LL = VBT.mu *)
             init (ch: VBT.T): T;
             isCompat (alg: ZeusClass.T): BOOLEAN;
             (* LL < VBT.mu *)
             startrun ();
             endrun   ();
           END;

END View.
