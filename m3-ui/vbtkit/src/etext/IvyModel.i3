(* Copyright (C) 1992 Digital Equipment Corporation                          *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Apr 21 10:13:32 PDT 1994 by mhb                      *)
(*      modified on Sun Mar 21 18:59:15 PST 1993 by meehan                   *)

INTERFACE IvyModel;

IMPORT TextPortClass;

TYPE T <: TextPortClass.Model;

END IvyModel.

(* {\tt TextPort} was originally designed after an editor called Ivy
   \cite{IvyRefman} that was developed at SRC.  Ivy was written in
   Modula-2 and included a wealth of features; the Ivy model,
   documented here, implements only a small subset of them.

   The Ivy model supports both local text-selections, Primary and
   Secondary.  Primary is an alias for Target, and Secondary is an
   alias for Source.

   There are two ways of acquiring the Source selection.  The usual
   way is to make a Secondary selection (since Secondary is an alias
   for Source) by shift- or control-clicking to select a point, word, line,
   paragraph, or buffer. The second way is to use the Copy command
   (option-C) or the Cut command (option-X). These commands make a
   copy of the Primary selection; the copy becomes the Source
   selection, but it is not displayed.

   The following list shows the Ivy keybindings. The Ivy model also
   supports ISO Latin-1 character composition.  See
   Section~\ref{MetaOptionKeys} for an explanation of ``option'' keys
   and composition.

\begin{center}
\begin{tabbing}
option-LongKeyName \= This is just a tab-setting line. \kill
 Return              \> invoke the {\tt returnAction} method \\
 shift-Return        \> call {\tt Newline} \\
 option-Return       \> insert a newline after the cursor \\
 Backspace           \> delete primary selection or the previous character \\
 option-Backspace    \> swap the two previous characters \\
 control-A           \> delete previous character \\
 control-B           \> delete whole line \\
 control-C           \> delete to start of line \\
 option-C            \> {\bf Copy} \\
 control-D           \> delete to the start of the current word \\
 control-E           \> {\bf Move}: replace target with source, and clear source \\
 control-F           \> delete to the end of the current word \\
 control-G           \> delete whole word \\
 control-H           \> swap the selection {\it boundaries} \\
 control-I           \> move to the next word \\
 control-J           \> move to previous character \\
 control-K           \> move to next character \\
 control-L           \> move to first non-blank and select line \\
 control-M           \> find previous occurrence \\
 option-M            \> find previous occurrence of primary \\
 control-N           \> find next occurrence of primary \\
 option-N            \> find first occurrence of primary \\
 control-O           \> move up 1 row in the current column \\
 control-P           \> move down 1 row in the current column \\
 control-Q           \> {\bf Clear} (delete the Primary selection) \\
 control-R           \> {\bf Swap}: exchange the selected {\it text} \\
 control-S           \> delete the next character \\
 control-U           \> move to the previous word \\
 control-V           \> delete to end of line \\
 option-V            \> {\bf Paste} \\
 control-W           \> {\bf Paste} \\
 option-X            \> {\bf Cut} \\
 control-Y           \> move to opposite end of selection \\
 control-Z           \> {\bf Undo} \\
 control-shift-Z     \> {\bf Redo} \\
 control-,           \> find next occurrence \\
 control-;           \> move to end of line and select line \\
 control-Space       \> normalize

\end{tabbing}
\end{center}

\subsubsection{The Ivy selection model}

The following table shows the mouse-gestures that establish the
Primary selection; if the Shift or Control key is held down, these same
gestures establish the Secondary selection.

\begin{center}
\begin{tabbing}
quadruple-click \= AllThree \= This is just a tab-setting line \kill
          click \> Left   \> to select a point between characters\\
   double-click \> Left   \> to select a single line\\
   triple-click \> Left   \> to select the entire buffer\\
          drag  \> Left   \> to change the selected point \\
          click \> Middle \> to select a single word\\
   double-click \> Middle \> to select a single paragraph\\
   triple-click \> Middle \> to select the entire buffer\\
           drag \> Middle \> to change the selected word or paragraph \\
          click \> Right  \> to extend the current selection \\
   double-click \> Right  \> to reduce the selection-unit \\
           drag \> Right  \> to extend the current selection
\end{tabbing}
\end{center}

A selection is a sequence of ``units''; a unit is a point, a word, a
line, a paragraph, or the entire buffer.  Double-clicking the right
mouse-button reduces the unit of the current selection from buffer to
paragraph, from paragraph to line, from line to word, and from word to
point.

A single left-click selects the point (zero-length interval) between
two characters. If you move the mouse and then right-click, the
selection is extended to include all the characters between that point
and the new position of the mouse. If you do {\em not} move the mouse,
then a right-click extends the selection to include the character
nearest that point.

A ``word'' is a maximal non-empty character sequence containing (1)
only letters and digits, or (2) one or more space and tab characters,
or (3) a single character that is not a letter, a digit, a space, or a
tab.

A ``line'' is a non-empty character sequence containing at most one
newline, whose first character either is the first character of the buffer or
immediately follows a newline, and whose final character is
either a newline or the last character in the buffer.

A ``paragraph'' is a sequence of lines---either a maximal sequence of
non-blank lines or a maximal sequence of blank lines. (A blank
line contains only spaces, tabs, and at most one newline.)

\subsubsection{Replace-mode selection}\label{ReplaceMode}

When a Primary selection in a non-readonly buffer is extended, the
selection becomes what is called a replace-mode selection, and its
highlighting changes from a red underline to a pale red background. If
you type after making a replace-mode selection, the first character
you type will replace the selection. If you use the Copy or Move
commands, the Secondary selection will replace the Primary selection.

*)
