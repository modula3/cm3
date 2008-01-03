(* Copyright 1993 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* NetObjNotifier.i3 *)
(* Last modified on Mon Nov  7 12:03:08 PST 1994 by wobber     *)

(* The "NetObjNotifier" interface allows the holder of a
   surrogate object to request notification of when the object's
   owner becomes inaccessible.  This can be useful, for example,
   if it is necessary to remove surrogates from a table upon
   termination of the programs holding their corresponding concrete
   objects.
*)

INTERFACE NetObjNotifier;

IMPORT NetObj;

TYPE
  OwnerState = {Dead, Failed};

  NotifierClosure = OBJECT METHODS
    notify(obj: NetObj.T; st: OwnerState);
  END;
    
PROCEDURE AddNotifier(obj: NetObj.T; cl: NotifierClosure);
(* Arrange that a call to "cl.notify" will be
   scheduled when "obj" becomes inaccessible.
   If "obj" is not a surrogate object then "AddNotifier" has no effect.
   If "obj" is already inaccessible at the time "AddNotifier" is called,
   then a call to "cl.notify" is scheduled immediately.
   \ttindex{NetObjNotifier.AddNotifier} *)

END NetObjNotifier.

(* The "notify" method of a "NotifierClosure" is invoked
   when the concrete object corresponding to the surrogate "obj"
   becomes inaccessible.  The procedure "AddNotifier" must have
   been called to enable this notification.  There may be more than
   one "NotifierClosure" for the same surrogate.  At notification time,
   the "st" argument is "Dead" if and only if the object owner
   is known to be permanently inaccessible.  Otherwise "st" is
   "Failed".  It is possible for "notify" to be called multiple
   times on the same object.  Any invocations on "obj" are guaranteed
   to fail in a timely fashion subsequent to a closure notification
   with "st = Dead".

   In general, a surrogate object can still be collected if a notifier
   closure is registered for it.  However, if the closure object
   contains a reference to the surrogate, then its registration
   might delay or prevent collection.  Therefore this should be avoided.

   Although this interface is organized to enable notification of
   owner death on a per object basis, in practice this is achieved by
   monitoring the state of the owner's address space.  This means
   that death notification will be more or less simultaneous for
   all surrogates whose concrete objects have the same owner.  *)


