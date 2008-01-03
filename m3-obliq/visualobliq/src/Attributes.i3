(* Copyright (C) 1993, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* Last modified on Mon Aug 30 13:17:14 PDT 1993 by bharat *)
(* modified on Fri Jul 2 16:33:31 PDT 1993 by mhb *)
<* PRAGMA LL *>

(* An "Attribute.T" is the Attribute Sheet used to manage the attributes of
   widget. *)

INTERFACE Attributes;

IMPORT FormsVBT, ListVBT,  NodeVBT, VBT;

TYPE
  T <: Public;
  Public = FormsVBT.T OBJECT
           METHODS
             <* LL <= VBT.mu *>
             init (): T;
           END;
(* The call initializes the Attribute sheet from file.  The initial state
   is uninstalled *)

VAR
  afv : T; (* attribute sheet fv *)
  currentNode : NodeVBT.T;

PROCEDURE ColorProc (cl  : FormsVBT.Closure;
                     afv  : FormsVBT.T;
                     name : TEXT;
                     time: VBT.TimeStamp);

(* attach your color-helperpopup-buttons to this procedure. The following *)
(* convention needs to be followed. If your color-helper-popup-button *)
(* is called "foo" then your typein should be called "footypein"      *)
(* If you choose to use the macro ColorField (see attributes.fv) then you *)
(* would say   (ColorField footypein foo) *)

PROCEDURE FontProc (cl  : FormsVBT.Closure;
                     afv  : FormsVBT.T;
                     name : TEXT;
                     time: VBT.TimeStamp);

(* attach your font-helper-popup-buttons to this procedure. The following *)
(* convention needs to be followed. If your font-helper-popup-button *)
(* is called "foo" then your typein should be called "footypein"      *)
(* If you choose to use the macro FontField (see attributes.fv) then you *)
(* would say   (FontField footypein foo) *)

PROCEDURE Invoke (v: T; nv: NodeVBT.T);
<* LL = VBT.mu *>
(* Takes a NodeVBT.T as argument.  This loads up the attributes of the
   given NodeVBT into the attribute-sheet and waits for apply to be
   pressed.  The NodeVBT provides access to all relevant info *)
(* If this is the first time it installs it - otherwise it is deiconized
   and made active *)

PROCEDURE Iconize (v: T);
<* LL = VBT.mu *>
(* Iconizes and makes dormant *)

(* Operations of Browsers (which are ListVBTs) *)

PROCEDURE LVFlush(v : ListVBT.T);
(* empty browser *)

PROCEDURE LVAppendText(v : ListVBT.T; t:TEXT);
(* t is a string of entries with \n as the separator and terminator *)


END Attributes.









