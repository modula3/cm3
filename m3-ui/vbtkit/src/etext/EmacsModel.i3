(* Copyright (C) 1992 Digital Equipment Corporation                          *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Sun Mar 21 18:59:17 PST 1993 by meehan                   *)
<* PRAGMA LL *>

INTERFACE EmacsModel;

IMPORT KeyFilter, TextPortClass;

TYPE
  T <: TextPortClass.Model;
  EscapeMetaFilter <: KeyFilter.T;

END EmacsModel. 

(* In the Emacs model, there is only a Primary selection. It is not
   an alias for either Source or Target.

   The model supports a single {\it region}, which is delimited by the
   {\it mark} and the {\it point}.  Control-space and control-@ set
   the mark; the point is the same as the current cursor position,
   which is changed by mouse-gestures, cursor-keys, or control-keys.
   When the region is established by cursor-keys or control-keys, it
   is not highlighted. If the region is highlighted, then any gesture
   that extends it will extend the highlighting as well.

   A single left-click sets the point and ensures that the current
   selection is not in replace-mode.  If you then drag the mouse, the
   location of the downclick becomes the mark, and the point is set to
   the current position of the mouse.  When the region is defined by
   dragging, it is highlighted. A double left-click sets both the mark
   and the point.

   The Cut and Copy commands make a copy of the text in the region
   (i.e., the Primary selection); it becomes the Source selection.
   Middle-click and meta-w call Copy.

   Right-click extends and highlights the current selection.

   The control- and meta-keys in the Emacs model are not
   case-sensitive; control-shift-a, for example, has the same effect
   as control-a.  The Emacs model supports ``Escape + character'' as
   an alternate way to type ``meta-character,'' and ISO Latin-1
   character composition. See Section~\ref{MetaOptionKeys} for an
   explanation of ``meta'' keys and composition.)

\begin{center}
\begin{tabbing}
option-LongKeyName \= This is just a tab-setting line. \kill
 control-space \> set the mark \\
 control-a \> move to the beginning of the line \\
 control-b \> move to the previous character \\
 meta-b \> move to the previous word \\
 control-d \> delete the next character \\
 meta-d \> delete the next word \\
 control-e \> move to the end of the line \\
 control-f \> move to the next character \\
 meta-f \> move to the next word \\
 control-h \> delete the previous character, and move left \\
 meta-h \> delete to the start of the current word \\
 control-i \> invoke the "tabAction" callback \\
 control-j \> insert a newline \\
 control-k \> delete to the end of the line, and make that \\
           \> the source selection \\
 control-m \> invoke the "returnAction" callback \\
 control-n \> move down one line \\
 control-o \> insert a newline without moving the cursor \\
 control-p \> move up one line \\
 control-q \> insert the next character (``quoted insert'')\\
 control-r \> search backward for the current source selection \\
 control-s \> search forward for the current source selection \\
 control-t \> swap the current and previous characters \\
 control-v \> scroll up one screen \\
 meta-v \> scroll down one screen \\
 control-w \> {\bf Cut} \\
 meta-w \> {\bf Copy} \\
 control-y \> {\bf Paste} \\
 control-z \> scroll up one line \\
 meta-z \> scroll down one line \\
 control-\_ \> {\bf Undo} \\
 meta-\_ \> {\bf Redo} \\
 meta-< \> move to the beginning of the buffer \\
 meta-> \> move to the end of the buffer \\
 meta-leftArrow \> move to the previous word (like meta-b) \\
 meta-rightArrow \> move to the next word (like meta-f)

\end{tabbing}
\end{center}
   *)
