(* Copyright 1989 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* Last modified on Fri Jul 31 11:11:13 PDT 1992 by birrell*)

(* A FastMenu.T is a subclass of FVTypes.FVMenu, the FormsVBT anchor class,
   that allows middle or right click on the anchor to cause specified button
   actions.  The buttons to be activated are given by "middle" and "right".
   They are typically in the menu popped up by the anchor, but that's
   not actually required.

   When the anchor receives a left-click firstDown, it behaves normally.  If
   it receives a middle or right firstDown, it calls the given VBT's "pre"
   method.  If you drag outside the anchor or you chord, the given VBT's
   "cancel" is called, otherwise lastUp calls the given VBT's action
   procedure. *)

INTERFACE FastMenu;

IMPORT ButtonVBT, FVTypes;

TYPE
  T <: Public;
  Public = FVTypes.FVMenu OBJECT
      middle, right: ButtonVBT.T;
    END;

END FastMenu.
