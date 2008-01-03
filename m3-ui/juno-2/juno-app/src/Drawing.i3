(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Apr 16 15:59:58 PDT 1996 by heydon                   *)
(*      modified on Sun Jun 11 17:26:29 PST 1995 by gnelson                  *)
(*      modified on Sat Aug 22 21:52:18 PDT 1992 by myers                    *)
<* PRAGMA LL *>
<* PRAGMA SPEC *>

(* A "Drawing.T" represents the state of the drawing view. *)

INTERFACE Drawing;

IMPORT JunoConfig, View, JunoPt, JunoAST;
IMPORT VBT, PaintOp, Point;
IMPORT Atom;

TYPE
  T = View.Drawing;

TYPE
  Child <: ChildWriteOnly;
  ChildWriteOnly <: ChildPublic;
  ChildPublic = VBT.Leaf BRANDED "Drawing.ChildPublic" OBJECT
    xform: JunoPt.Transform
  METHODS
    init(origin := JunoConfig.Origin.SW): Child;
    getOrigin(): JunoConfig.Origin;
    setOrigin(origin: JunoConfig.Origin)
  END;

(* The child of the drawing view has type "Child". A new child is created by
   calling "NEW(Child).init()". "xform" is the transformation to use to
   convert between Juno and Trestle coordinates.

   The type "ChildWriteOnly" can be used as the child of a drawing window that
   produces output, but does not handle user input, such as a separate
   animation window.

   The default "rescreen" and "reshape" methods of a "ChildWriteOnly" conspire
   to maintain the "xform" field correctly. In addition, the default "reshape"
   method invokes the child's "repaint" method on "Region.Full". *)

TYPE
  ArgType = {Click, CreateClick, Drag, Text};

  Arg = RECORD
    name: JunoAST.QId;
    loc, locUp: JunoPt.T;		 (* Click, CreateClick, or Drag arg *)
    text: TEXT := NIL;			 (* Text arg *)
  END;

  Tool = OBJECT
    label: TEXT;
    argType: REF ARRAY OF ArgType;
  METHODS
    setup(d: T; time: VBT.TimeStamp) := NIL;
    <* LL.sup = VBT.mu *>
    apply(d: T; READONLY arg: ARRAY OF Arg) := NIL;
  END;

<* SPEC Tool.apply(tl, d, arg) REQUIRES sup(LL) = VBT.mu *>

(* A "Drawing.Tool" represents one of the tools that a user can select
   through the Juno user interface. It should never be instantiated directly.
   Instead, you should instantiate one of its two main subtypes: a
   "Drawing.ArgTool" or a "Drawing.SetTool".

   A tool "t" has the name "t.label", and the types of its arguments (if any)
   are determined by "t.argType". Its "setup" method is called before any
   of the arguments to the tool are available (even if the tool has 0
   arguments); the "apply" method is called after all of the arguments to the
   tool are available. The default "setup" method is a no-op. The "apply"
   method should not do a VBT.Sync on the drawing view. *)

  ArgTool <: ArgToolPublic;
  ArgToolPublic = Tool BRANDED "Drawing.ArgToolPublic" OBJECT
    name: JunoAST.QId;
    in_cnt: CARDINAL := 0
  END;

  ProcToolPublic = ArgTool BRANDED "Drawing.ProcToolPublic" OBJECT
    out_cnt, inout_cnt: CARDINAL := 0
  METHODS
    text(d: T; txt: TEXT);
  END;

  PredTool <: ArgTool;
  FuncTool <: ArgTool;
  ProcTool <: ProcToolPublic;

  SetTool <: Tool;

(* There are two kinds of drawing tools: "ArgTool" and "SetTool". The former
   tool takes one or more arguments, which are clicked or typed through the
   user interface. The latter tool is associated with a procedure for calling
   a "Set" procedure with a fixed set of values; a "SetTool"'s "argType" array
   should be empty.

   A tool "t" takes "NUMBER(t.argType^)" arguments. How the user
   interface accepts these arguments depends on their types, as described
   below. When the tool receives its allotment of arguments, "t.apply" is
   invoked with those arguments. The data stored in each argument depends on
   the type of the corresponding argument, as follows:

   "Click" argument: Clicking selects the point nearest the down click;
   shift-click creates a new point at the location of the down click. In the
   corresponding argument, "name" is the name of the selected point, and "loc"
   is its location.

   "CreateClick" argument: Clicking creates a new point at the location of the
   down click. In the corresponding argument, "name" is the name of the
   new point, and "loc" is its location.

   "Drag" argument: Clicking selects the point nearest the down click; the
   user can then drag the point with the mouse. In the corresponding argument,
   "name" is the name of the selected point, "loc" is its initial location,
   and "locUp" is the location of the last up click. Moreover, the "t.pre"
   method is invoked on the initial down click, the "t.post" method is invoked
   on the final up click, and the "t.during" method is invoked whenever the
   mouse moves while the point is being dragged.

   "Text" argument: Entered from the keyboard, terminated by <RETURN>. This
   type is only allowed as the last argument to a procedure. The "text" method
   will be called when this argument is reached, and after each character (not
   counting the terminating <RETURN>) is typed. The "txt" argument to the
   "text" method is the current sequence of characters typed (as modified by
   line-editing characters like backspace). The corresponding "Arg" "a" has
   "a.name = NIL" and "a.text" equal to the final text string typed by the
   user. *)

PROCEDURE NewArgArray(n: CARDINAL; type: ArgType): REF ARRAY OF ArgType;
(* Return a new "ArgType" array of size "n" all of type "type". *)

PROCEDURE FinishTextTool(d: T): BOOLEAN;
(* If the user was typing the last argument to a text tool, then apply the
   tool, unselect it, and return TRUE. Do nothing and return FALSE otherwise.
*)

PROCEDURE Make(d: T; skipify: BOOLEAN);
(* Redraw "d" by running the version of the current command consistent with
   "skipify". *)

PROCEDURE Exec(d: T): TEXT;
(* Reset the PostScript state, clear the drawing, run the current command,
   and paint the final annotations in "d". Return the text of an error
   message, or NIL if there were no errors. *)

PROCEDURE Annotations(d: T);
(* Paint the final annotations in the drawing "d": the current path, the grid,
   and the point labels. *)

PROCEDURE Sync(v: VBT.T);
(* Equal to "VBT.Sync(v)", but useful for logging purposes. "v" should be the
   "Filter.Child" of a "Drawing.T". *)

PROCEDURE SetLabelStyle(d: T; style: [0..2]);
(* Set "d"'s label style: 0 means no labels, 1 means crosses, and 2 means
   dots and point names. Update the drawing if necessary. *)

PROCEDURE SelectTool(d: T; t: Tool; time: VBT.TimeStamp);
(* Select the tool "t" for the drawing "d". If "t" is "NIL", the current tool
   is deselected. In either case, if the user was currently typing the last
   argument of a text tool, apply the tool as if the user had hit <RETURN>. *)

PROCEDURE NewCreateTool(): ArgTool;
(* Return a new tool taking one "CreateClick" argument whose "apply" method is
   a no-op. *)

VAR (* READONLY *) HorToolSym, VerToolSym, CongToolSym, ParaToolSym: Atom.T;
(* These are the names of the Hor, Ver, Cong, and Para tools. *)

PROCEDURE NewPredTool(name: JunoAST.QId; in_cnt: CARDINAL): ArgTool;
(* Return a new tool taking "in_cnt" "Click" arguments whose "apply" method
   adds to the current command's constraint the application of the named
   predicate to the argument points. *)

PROCEDURE NewFuncTool(name: JunoAST.QId; in_cnt: CARDINAL): ArgTool;
(* Return a new tool taking "1 + in_cnt" "Click" arguments whose "apply"
   method adds to the current command's constraint an equality between the
   first argument point and the application of the named function to the
   remaining "in_cnt" argument points. *)

PROCEDURE NewProcTool(name: JunoAST.QId; in_cnt: CARDINAL;
  out_cnt, inout_cnt: CARDINAL := 0; isText := FALSE): ArgTool;
(* Return a new tool taking "out_cnt + inout_cnt + in_cnt" "Click" arguments
   whose "apply" method adds to the current command the application of the
   named procedure to the argument points. If "isText" is "TRUE", then the
   last argument has type "ArgType.Text". *)

PROCEDURE NewFreezeTool(): ArgTool;
(* Return a new tool taking 1 "Click" argument whose "apply" method toggles
   the frozen attribute of its argument. *)

PROCEDURE NewAdjustTool(): ArgTool;
(* Return a new tool taking 1 "Drag" argument whose "apply" attempts to move
   the point named in its argument to the "locUp" position contained in that
   argument. If the point is frozen, then the point is simply moved.

   If the point is unfrozen, it is temporarily frozen and moved. If this move
   succeeds, the point is returned to its unfrozen state. However, if this
   initial move fails, the point is unfrozen and moved again. *)

PROCEDURE NewRelTool(): ArgTool;
(* Return a new tool taking 3 "Click" arguments whose "apply" method changes
   the current command so that the last argument is "REL" the pair of the
   first two. *)

PROCEDURE NewRel1Tool(): ArgTool;
(* Return a new tool taking 2 "Click" arguments whose "apply" method changes
   the current command so that the first argument is
   "R2.Plus(second argument, (x,y))" for some numbers x and y. *)

PROCEDURE NewGridTools(tb: VBT.Split; d: T): ArgTool;
(* Return a new "GridOn" tool taking 2 "Click" arguments whose "apply" method
   turns grid mode on, unselects the current tool, and replaces the "GridOn"
   tool by a "GridOff" tool in the toolbox "tb" with associated drawing view
   "d". *)

PROCEDURE NewSetTool(procNm: JunoAST.QId; arg: JunoAST.Expr): SetTool;
(* Create a new set tool that, when invoked, adds the procedure call
   "procNm(arg)" to the current command. *)

PROCEDURE PaintPath(d: T);
(* Paint "d"'s current PostScript path as a red line with a white (opaque)
   outline. *)

PROCEDURE PaintPoint(ch: Child; name: TEXT; op: PaintOp.T; pt: Point.T);
(* Paint the point named "name" using "op" at location "pt" in the drawing
   child "ch". *)

PROCEDURE SourceUntrue(d: T; how: View.ModKind);
(* Declare that the source window has been modified; "how" indicates the kind
   of modification. If continuous parsing is turned on, this will update the
   source window. *)

PROCEDURE DisplayError(d: T; errmsg: TEXT); <* LL.sup = VBT.mu *>
(* Display a drawing error in the source window. *)

END Drawing.
