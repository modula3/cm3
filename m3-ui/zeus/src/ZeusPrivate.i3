(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Thu Feb 11 17:52:24 PST 1993 by johnh*)



INTERFACE ZeusPrivate;

IMPORT Zeus, ZeusClass;
<* PRAGMA LL *>

TYPE
  PrivateSession = Zeus.PublicSession BRANDED OBJECT
      evtWasHandled: BOOLEAN;   (* Set TRUE by Zeus when an event was
                                   handled by a non-default method. *)
    END;

REVEAL Zeus.Session <: PrivateSession;

PROCEDURE Mark (zeus: Zeus.Session; v: ZeusClass.T);
  <* LL = VBT.mu *>
  (* Mark v as belonging to a particular zeus session.  This is the
     counterpart to the public procedure Resolve. *)

PROCEDURE AlertViews(zeus: Zeus.Session);
  <* LL = arbitrary *>
  (* Send an alert to any view of this zeus session that has registered
     itself as alertable. *)

END ZeusPrivate.
