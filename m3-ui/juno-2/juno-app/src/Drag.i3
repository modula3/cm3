(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Apr 15 09:52:32 PDT 1996 by heydon                   *)
<* PRAGMA LL *>

INTERFACE Drag;

(* Dragging points in the drawing view *)

IMPORT Drawing, ExternalProc, PSImpl, VBT, Point;

REVEAL
  Drawing.T <: T;
  Drawing.ArgTool <: ArgTool;

CONST MaxArgs = 100;			 (* maximum # of args to any tool *)

TYPE
  T <: Public;
  Public = PSImpl.T BRANDED "Drag.Public" OBJECT
    (* Stack *)
    stackSize: CARDINAL := 0;
    stack: ARRAY [0..MaxArgs-1] OF Drawing.Arg;
    (* "Drag" args *)
    dragging := FALSE;
    dragger, draggee: Point.T;
    dragName: TEXT;
    dragMode: DragMode;
  END;
  DragMode = { Unconstrained, Hor, Ver };

(* If "d: T", the "dragging", "dragger", "draggee", "dragName", and "dragMode"
   fields are relevant when the current argument of "d.tool" has type
   "ArgType.Drag". The bit "d.dragging" is set during the drag, "d.dragger" is
   the location of the mouse as it is moved (in Trestle coordinates),
   "d.draggee" is the location of the point being dragged (also in Trestle
   coordinates), "d.dragName" is the name of the point being dragged, and
   "d.dragMode" is the drag mode. *)

EXCEPTION Aborted;

(* The "Aborted" method is raised to abort the drag prematurely, as if the
   user had released the mouse button. *)

TYPE
  ArgTool <: ArgToolPublic;
  ArgToolPublic = Drawing.ArgToolPublic OBJECT METHODS
    <* LL.sup = VBT.mu *>
    pre(d: Drawing.T; READONLY cd: VBT.MouseRec; i: INTEGER)
      RAISES {Aborted};
    during(d: Drawing.T; READONLY delta: Point.T; i: INTEGER): Point.T
      RAISES {Aborted};
    post(d: Drawing.T; READONLY cd: VBT.MouseRec; i: INTEGER);
  END;

(* The "pre" method is called at the start of the drag, with "d" the current
   drawing, "cd" the mouse event initializing the drag, and "i" the current
   size of the tool argument stack.

   The "during" method is called each time the mouse moves while dragging a
   "ArgType.Drag" argument. The call "tool.during(d, delta, i)" is made with
   "d" the current drawing, "delta" the difference vector between the current
   mouse position (namely, "d.dragger") and the last one, and "i" the current
   size of the tool argument stack. This routine should return the new
   coordinates for the point "d.draggee" (in Trestle coordinates) as a result
   of moving the mouse. It should raise "Aborted" to abort the drag
   prematurely, as if the user had released the mouse button. *)

PROCEDURE NewTool(): Drawing.ArgTool;
(* Return a new tool taking 1 "Drag" argument whose methods conspire to
   implement continuous solve. *)

PROCEDURE CallbackProc(dc: ExternalProc.Closure): BOOLEAN;
(* The "JunoUI._DRAG" procedure called while dragging a point. *)

END Drag.
