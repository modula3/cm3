(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Sat Mar  1 01:08:25 PST 1997 by heydon                   *)
(*      modified on Mon Oct 19 10:13:20 PST 1992 by gnelson                  *)
(*      modified on Fri Aug  7 21:51:55 PDT 1992 by myers                    *)
<* PRAGMA LL *>

(* A "View.T" is the common supertype used by the source and drawing views. *)

INTERFACE View;

IMPORT Editor, CurrCmd, Marquee;
IMPORT FormsVBT;
IMPORT Filter, ButtonVBT;

TYPE
  T = Filter.T OBJECT
    root: Root
  METHODS
    <* LL.sup <= VBT.mu *>
    update();
    <* LL.sup < SELF *>
    modified(how: ModKind);
  END;

  ModKind = { Explicit, ImplicitConsistent, ImplicitOutOfDate };

(* If "t" is a "View.T", then "t.update" is responsible for making the view
   "t" contain the truth according to the current command "t.root.ccmd"; the
   method requires "t.root.astTrue".

   The method "t.modified" is called whenever the view is modified. If the
   "how" argument is "Explicit", then "t" was edited explicitly through "t".
   Otherwise, "t" was edited implicitly through some other view. If "how" is
   "ImplicitConsistent", then the view through which the edit was actually
   made is consistent with the change to "t". If "how" is "ImplicitOutOfDate",
   then the view through which the edit was actually made is out-of-date with
   respect to "t". *)

  PSImpl  <: T;
  Drawing <: PSImpl;

  Root = OBJECT
    ccmd: CurrCmd.T;
    drawing: Drawing;
    animView, currView: PSImpl;
    source: T;
    editor: Editor.T;
    modules: EditorList := NIL;
    currButton: ButtonVBT.T;
    dTrue, astTrue, sTrue, eTrue: BOOLEAN;
    skipify := FALSE;
    marquee: Marquee.T;
  END;

(* A "View.Root" contains the shared data visible to all the views and the
   top-level Juno window. The fields "drawing", "source", "editor", and
   "modules" point to the drawing view, source view, anonymous module editor,
   and named module editors, respectively.

   When Juno is used as a remote Zeus animation view, "animView" is the
   animation view. In any case, "currView" is either "drawing" or "animView",
   and is the view all graphics are drawn to by the implementation of the "PS"
   module.

   "CurrButton" is the current "ToolBox.Button".

   The booleans "dTrue", "sTrue", "astTrue", and "eTrue" reflect whether the
   drawing, source, AST, and compiled editor scope contain the truth,
   respectively.

   If "skipify" is TRUE, we will solve only (and not run) when the current
   command is run implicitly (e.g., for repaints and during drags). *)

  EditorList = REF RECORD
    view: Editor.T;
    form: FormsVBT.T := NIL; (* actually of type "Juno.Toolbox" *)
    mod: TEXT;
    next: EditorList
  END;

(* An "EditorList" is a list of open module editors. If "e: EditorList", then
   "e.view" is the module editor, "e.form" is the toolbox form for the module
   (or NIL if none has been created yet for this module), and "e.mod" is the
   name of the module. *)

END View.
