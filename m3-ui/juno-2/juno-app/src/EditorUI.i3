(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Jun 19 09:41:40 PDT 1995 by heydon                   *)
(*      modified on Fri Aug  7 21:51:50 PDT 1992 by myers                    *)
<* PRAGMA LL *>

(* A companion interface to "Editor.i3" that allows processing the
   "UI" declarations.  This is separated from "Editor.i3" to allow
   it to import "View" without creating a cycle of imports among
   the interfaces. *)

INTERFACE EditorUI;

IMPORT View, Editor, VBT, JunoScope;

PROCEDURE CompileUI(
    rt: View.Root;
    e: Editor.T;
    time: VBT.TimeStamp;
    scp: JunoScope.T;
    ): BOOLEAN;
(* Should be called immediately after "Editor.Compile(e, ts, scp, nm, ent)" 
   returns TRUE, and should be called with "scp := ent.scp".  Processes the
   "UI" declarations in "e" and creates and installs the appropriate buttons
   and  menus and returns "TRUE", or displays an error to the user and returns
   "FALSE". *)

END EditorUI.

