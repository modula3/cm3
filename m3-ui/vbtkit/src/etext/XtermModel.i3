(* Copyright (C) 1992 Digital Equipment Corporation                          *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Sun Mar 21 18:59:13 PST 1993 by meehan                   *)
<* PRAGMA LL *>

INTERFACE XtermModel;

IMPORT TextPortClass;

TYPE T <: TextPortClass.Model;

END XtermModel.

(* The Xterm model, patterned after "xterm(1)", supports a single
   selection, Primary, which is an alias for Source. The Primary
   selection is never in replace-mode.  The Xterm model is not
   influenced by commands in the user's ".Xdefaults" file.

   A single-left-click establishes the keyboard focus and insertion
   point, but it does not change (acquire) the selection. A
   double-left-click selects the current word; a triple-left-click
   selects the current line. More clicks rotate among these three
   options.

   Single-left-click and drag selects a range of characters.
   Double-left-click and drag selects a range of words, and
   triple-left-click and drag selects a range of lines.

   Middle-click pastes the current source selection at the insertion
   point, which need not be at the end of the text (as it would be for
   a ``typescript'').  

   Right-click extends the current selection, re-highlighting it if
   needed.

   The shift key has no effect on the mouse; it is ignored, so that
   shift-left-click, for example, has the same effect as left-click.
   The control and meta (``option'') keys, however, are not ignored;
   they cause the mouse-clicks to be no-ops, and they have different
   keybindings.  Control-left-click, for example, has no effect.

   The only keybindings that are supported are these:

\begin{center}
\begin{tabbing}
option-LongKeyName \= This is just a tab-setting line. \kill
 control-u    \> delete everything from the current position \\
              \> to the beginning of the line \\
 control-z    \> {\bf Undo} \\
 control-shift-z    \> {\bf Redo} \\
 meta-x       \> {\bf Cut} \\ 
 meta-c       \> {\bf Copy} \\
 meta-v       \> {\bf Paste}

\end{tabbing}
\end{center}

   Note that Copy does very little; since Primary is an alias for
   Source, nothing is actually copied.
*)
