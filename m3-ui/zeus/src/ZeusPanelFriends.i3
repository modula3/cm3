(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Mon Jul 19 17:06:23 PDT 1993 by steveg *)
(*      modified on Tue Nov 24 02:42:09 PST 1992 by johnh *)
(*      modified on Mon Oct 26 13:19:20 PST 1992 by mhb *)
<* PRAGMA LL *>

INTERFACE ZeusPanelFriends;

IMPORT View;

VAR (* Hack to stop drifting snapshot/restore *)
  XDRIFT: INTEGER := 0;
  YDRIFT: INTEGER := 24;

VAR
  (* READONLY *) whichAlg, whichView: TEXT; 
  (* When the New procedure for an algorithm or view, "whichAlg" or "whichView"
     is set to the name of the selected algorithm or view.  These variables
     are only valid during a call on the New procedure *)

PROCEDURE DetachView (view: View.T);
(* Detach the specified view from its ZeusPanel.Session and remove the
   session from view's property list.  If the view was previously attached
   with Zeus.AttachView, this is identical to Zeus.DetachView.  If the view
   wasn't previously attached, then this just deletes the view from the
   session's list of not-yet-attached views.  LL = VBT.mu *)

END ZeusPanelFriends.
