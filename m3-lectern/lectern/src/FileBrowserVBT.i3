(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Jun 14 21:02:17 PDT 1993 by meehan                   *)
(*      modified on Tue Feb  2 14:30:14 PST 1993 by mhb                      *)
(*      modified on Tue Jun 16 13:08:54 PDT 1992 by muller                   *)
<* PRAGMA LL *>

(* A "FileBrowserVBT" displays the files in a directory, and allows
   the user to traverse the file system and to select one or more
   files.  There are two additional widgets that can be associated
   with a "FileBrowserVBT".  A {\em helper}
   \index{FBHelper}\label{FBHelper} is a type-in field that displays
   the pathname of the directory and allows the user to type new
   pathnames.  A {\em directory-menu}\index{FBDirMenu}
   \label{FBDirMenu} is a menu containing the names of each level in
   the directory tree, with the root at the bottom; you can go to any
   level in the tree by selecting the appropriate item in the menu.

   There are two user-actions, selecting and activating.

   \begin{itemize}

   \item The user may {\it select} items, either by single-clicking on
   an item to select just that one, or by single-clicking and dragging
   to select a range.  Shift-clicking adds to the selection.  A change
   in selection is reported to the client by invoking the
   "selectItems" method.  The client can read the current selection by
   calling "GetFile" or "GetFiles".

   \item The user may {\it activate} an item, either by
   double-clicking on it, or by typing its name in the helper followed
   by Return.

   Activation of a {\it file} is reported to the client by invoking
   the "activateFile" method, whose default is a no-op.

   Activation of a {\it directory} is reported by invoking the
   "activateDir" method, whose default behavior is to call "Set" to
   display the activated directory.

   The client can distinguish between a double-click and Return by
   looking at the "AnyEvent.T" passed to the activation method.  A
   double-click will be reported as an "AnyEvent.Mouse", and Return
   will be reported as an "AnyEvent.Key".

   \end{itemize}

   Directories are indicated in the display by showing some text
   (e.g., ``(dir)'') after the name, but that is not part of the
   pathname returned by "getValue", "GetFile", "GetFiles", or the
   value passed to "activateDir".

   A background thread calls "Refresh(v)" for every open filebrowser
   "v", once per second, to see whether it needs to be updated
   (although a distributed filesystem may cause a substantial delay
   before the change is noticed).

   "FileBrowserVBT" is internally synchronized.

*)

INTERFACE FileBrowserVBT;

IMPORT AnchorSplit, AnyEvent, Font, ListVBT, PaintOp,
       Pathname, Shadow, TextList, TypeinVBT, VBT;

TYPE
  T <: Public;
  Public =
    ListVBT.T OBJECT
    METHODS
      <* LL.sup <= VBT.mu *>
      init (font                      := Font.BuiltIn;
            colors: PaintOp.ColorQuad := NIL           ): T;
      <* LL.sup = VBT.mu *>
      selectItems  (event: AnyEvent.T);
      activateFile (filename: Pathname.T; event: AnyEvent.T);
      activateDir  (dirname : Pathname.T; event: AnyEvent.T);
      error        (err: E);
    END;

(* The call "v.init(...)" initializes "v" as a "FileBrowserVBT".  If
   "v.painter" is a subtype of "ListVBT.TextPainter", "init" calls
   "v.paint.setFont(font)".  The "selector" field must be either "NIL"
   (in which case a new selector is created) or a subtype of
   "FileBrowserVBT.Selector".  The initial state of the filebrowser is
   the current working directory, as returned by
   "Process.GetWorkingDirectory".

   The implementation calls "v.selectItems(event)" when the user
   changes the selection using the mouse.

   When the user double-clicks on a file in the browser, the
   implementation calls "v.activateFile(filename, event)", where
   "filename" in the absolute pathname corresponding to the first selected
   item. If the user types Return in the helper, the implementation
   calls "v.activateFile(filename, event)", where "filename"
   is either the pathname in the helper, if that was absolute, or
   absolute pathname corresponding to
| Pathname.Join (GetDir(v), 'helper text', NIL)

   Don't forget that if "activateFile" is being called because of a
   double-click, multiple files might be selected in the browser, even
   though you are given only one in the "filename" parameter.

   The implementation calls "v.activateDir(dir)" when a directory is
   activated.  The normal action is simply to set "v" to view that
   directory, relative to "GetDir(v)".  If an error occurs during the
   activation, the "error" method is invoked.

   The implementation calls "v.error(...)" when an error occurs during
   user action in "v", and the "Error" exception cannot be raised
   (e.g., because it happened in a separate thread).  Some examples of
   errors are as follows: the user has typed a nonexistent directory
   in the path; the current directory has become inaccessible; the
   user has no permission to read the directory.  The default method
   is a no-op.  By overriding this method, the client can provide
   better information to the user.

   The "error" method is passed an "E" object containing information
   about the error that occurred. Here is its definition: *)

EXCEPTION Error (E);
TYPE
  E = OBJECT
        v   : T;
        text: TEXT       := "";
        path: Pathname.T := ""
      END;
(* The argument to the "Error" exception includes the
   "FileBrowserVBT" itself, along with a descriptive message and
   the pathname in question when the error occurred. *)

(* Finally, if you create a subtype of "FileBrowserVBT" (which is a
   subtype of "ListVBT.T") and you specify a selector for it, it
   must be a subtype of "Selector": *)

TYPE Selector <: ListVBT.MultiSelector;

(* \subsubsection{The Helper} 
   
   The FileBrowser's helper (see page \pageref{FBHelper}) is a
   "TypeinVBT".  Once the user types in the helper, any selected items
   in the browser are unselected.  If the user types Return in the
   browser, that will activate the name in the Helper.
   
   If an error occurs during the activation, the "error" method of the
   filebrowser to which the helper is attached will be invoked. *)

TYPE Helper <: TypeinVBT.T;

PROCEDURE SetHelper (v: T; helper: Helper) RAISES {Error};
<* LL.sup = VBT.mu *>
(* Sets the helper for "v" to be "helper", and fills it with
   "GetDir(v)". *)

(* \subsubsection{The Directory-Menu} *)

(* The directory menu shows the name of each of the parent directories,
   going back to the root directory. *)

TYPE
  DirMenu <: PublicDirMenu;
  PublicDirMenu =
    AnchorSplit.T OBJECT
    METHODS
      <* LL.sup <= VBT.mu *>
      init (font             := Font.BuiltIn;
            shadow: Shadow.T := NIL;
            n     : CARDINAL := 0             ): DirMenu
    END;

(* The "font" and "shadow" control the appearance of the text
   within the menu.  As usual, if "shadow" is "NIL", then
   "Shadow.None" is used instead.  The parameter "n" is used by
   "AnchorSplit" to determine the "ZSplit" in which to install
   the menu. *)


PROCEDURE SetDirMenu (v: T; dm: DirMenu);
<* LL.sup = VBT.mu *>
(* Sets the directory-menu of "v" to be "dm" and fill it with the
   current directory. *)


(* \subsubsection{FileBrowser options} *)

(* A file browser can be ``read-only'': *)

PROCEDURE SetReadOnly (v: T; readOnly: BOOLEAN);
<* LL.sup = VBT.mu *>
(* Change the ``read-only'' mode of "v" to be "readOnly". *)

(* If a file browser is ``read-only'' then in subsequent calls to
| v.activateFile(filename)
   "filename" is guaranteed to exist.  Otherwise, the user can type the
   name of a non-existing file into the helper.  A newly initialized
   "FileBrowserVBT" is not read-only.

   By default all files in the directory are displayed, but the
   following procedure can be used to filter which files are
   shown: *)

PROCEDURE SetSuffixes (v: T; suffixes: TEXT);
<* LL.sup = VBT.mu *>
(* Specify which "suffixes" are to be displayed. *)

(* If "suffixes" is not the empty string, only files with the
   specified suffixes (and all directories) will be displayed.
   The format of "suffixes" is a sequence of suffixes (not
   including the period) separated by non-alphanumeric characters
   (e.g., spaces).  The special suffix "$" indicates ``files with
   no suffix.''  Calling "SetSuffixes" procedure does not force
   "v" to be redisplayed. *)

(* \subsubsection{Setting the displayed directory} *)

PROCEDURE Set (v       : T;
               pathname: Pathname.T;
               time    : VBT.TimeStamp := 0) RAISES {Error};
<* LL.sup = VBT.mu *>
(* Set the display state of v. *)

(* The "pathname" may be absolute or relative; if it's relative, it
   is relative to the current displayed directory.

   If "pathname" refers to a non-existent or inaccessible directory,
   "Error" will be raised.  The exception will also be raised if
   "pathname" refers to a non-existent file and "v" is read-only.

   If "time" is not zero and there is a helper, then the helper
   will take the keyboard focus and will display its new contents
   in replace-mode, ready for the user to type something in its
   place. *)

PROCEDURE Unselect (v: T);
<* LL.sup = VBT.mu *>
(* Put "v" into the no-selection state, without changing the
   current directory. Equivalent to "v.selectNone()". *)

PROCEDURE Refresh (v: T) RAISES {Error};
<* LL.sup = VBT.mu *>
(* Update the display without changing the directory.*)

(* If "v"'s domain is not empty, and its directory has been "Set", and
   the directory has changed since the last time it was displayed,
   then "v" will be marked for redisplay.  "Error" is raised only if
   the directory has become inaccessible for some reason; in this
   case, the browser goes to the empty state, so that if the client
   catches "Error" and takes no other action, the browser will be
   empty but not broken. *)

(* \subsubsection{Retrieving selections from the browser} *)

PROCEDURE GetFiles (v: T): TextList.T RAISES {Error};
<* LL.sup = VBT.mu *>
(* Return the current selections of "v", or "NIL" if there are no
   selections. The list includes ``full'' pathnames; they satisfy
   "Pathname.Absolute", but they may contain symbolic links. Use
   "FS.GetAbsolutePathname" to get a pathname with no symbolic links.
   *)

PROCEDURE GetFile (v: T): Pathname.T RAISES {Error};
<* LL.sup = VBT.mu *>
(* Return the first selection, or the empty string if there are no
   selections. *)

PROCEDURE GetDir (v: T): Pathname.T;
<* LL.sup = VBT.mu *>
(* Return the current displayed directory of "v".  Returns an empty
   string if "v" is in the ``empty'' state. *)

END FileBrowserVBT.
