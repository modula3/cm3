(* Copyright (C) 1993, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* Last modified on Wed Aug 25 14:18:25 PDT 1993 by bharat *)
(* modified on Fri Jul 2 16:33:31 PDT 1993 by mhb *)
<* PRAGMA LL *>

(* All components (forms/frames/widgets) in "NodeVBT" are instances *)
(* of NodeVBT *)

INTERFACE Browser;

IMPORT NodeVBT;

TYPE
  BrowserNode <: NodeVBT.Widget;
  Browser <: BrowserNode;
  FileBrowser <: BrowserNode;
  (* Browser has a boolean field for multi-browser - at design time
     there is no difference since you cant interact with it anyway  *)
  (* Filebrowsers have a lot of appendages and so are implemented as a subtype *)

PROCEDURE Initialize();
  
END Browser.













