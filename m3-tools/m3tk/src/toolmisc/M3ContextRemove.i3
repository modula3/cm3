(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE M3ContextRemove;

IMPORT M3Context, M3CUnit;

PROCEDURE Units(
    context: M3Context.T;
    READONLY units: ARRAY OF TEXT;
    ut: M3CUnit.Type);
(* Remove the 'units' of type 'ut' from 'context', plus all units which
reference these, transitively. Any registered notifications will be
called for each dependent unit that is removed. *)

PROCEDURE UnitsWithErrors(
    context: M3Context.T);
(* As 'Units' but remove those units with compilation errors. *)

TYPE Notification = M3Context.Closure;

PROCEDURE AddNotification(n: Notification);
(* Register "n.callback" to be called when a unit is removed as the
   result of a call to "Unit" or "UnitsWithErrors". The notifications 
   are called in the order that they were added. *)

PROCEDURE RemoveNotification(n: Notification);
(* Remove notification "n". *)

END M3ContextRemove.
