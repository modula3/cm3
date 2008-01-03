(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Fri Mar 12 10:57:52 PST 1993 by meehan     *)
(*      modified on Tue Jun 16 21:55:37 PDT 1992 by muller     *)
<* PRAGMA LL *>

INTERFACE Manpage;

IMPORT FormsVBT, Rsrc;

TYPE
  ErrorReporter = OBJECT
                  METHODS
                    apply (msg: TEXT) <* LL = VBT.mu *>
                  END;

PROCEDURE Init (fv           : FormsVBT.T;
                name         : TEXT;
                er           : ErrorReporter;
                helpfindfirst                  := "helpfindfirst";
                helpfindnext                   := "helpfindnext";
                helpfindprev                   := "helpfindprev";
                helpfindtext                   := "helpfindtext";
                manpagetext                    := "manpagetext";
                notfound                       := "notfound";
                helpcase                       := "helpcase";
                path         : Rsrc.Path       := NIL              )
  RAISES {FormsVBT.Error};

PROCEDURE TextReverse (t: TEXT): TEXT;
(* This produces a new text with the characters in opposite order. *)

END Manpage.

(* "fv" is a FormsVBT.T that will be used for displaying a file (e.g., a
   manpage) and providing some string-search capabilities.  "name" is the name
   of a resource (e.g., a file) containing the text of the manpage.  "fv"
   should have buttons and various text-fields, described below, that are used
   for displaying the file and searching.  "Init" attaches procedures to the
   buttons, and it forks a thread to read the file into one of the
   text-fields, so that the text will be ready the first time the user asks
   for it.  The intention is that the text will be displayed in pop-up window
   attached to a Help button of some sort.

   The file is opened without locking VBT.mu, so this shouldn't interfere with
   user actions.  After the file has been opened, VBT.mu is locked briefly
   while a pointer to the text is stored in the object; we don't actually copy
   the bytes until the user views that part of the file.  If the user opens
   the window before the file has been read, the process will block until the
   text is ready.

   If the name of the Boolean (helpcase) or popup (notfound) is NIL,
   then there will be no attachment; the Boolean defaults to being
   case-insensitive.
   
   If the named buttons are missing, then Init will raise an exception.  All
   errors that occur in the thread that reads the file, or during the search,
   are signaled by calling "er.apply" with VBT.mu locked.  An unsuccessful
   search isn't an error; that causes a subwindow to pop up and disappear
   two seconds later.

   The form, "fv", must have the following named VBTs:

| helpfindtext '-- the name of the TypeIn that contains the
|    string to search for'
| helpfindfirst '-- the name of the Button that will cause
|    a search for the first occurrence of the string'
| helpfindnext '-- the name of the Button that will cause
|    a search for the next occurrence of the string'
| helpfindprev `-- the name of the Button that will cause
|    a search for the previous occurrence of the string'
| manpagetext `-- the name of the TextEdit (should be ReadOnly)
|    where the text of the file should appear'
| notfound `-- the name of a ZChild or ZChassis displaying a
|    message like ``Not found'', which will pop up if the
|    search is unsuccessful.'
| helpcase `-- the name of the Boolean that controls
|    case-sensitivity during the search. ("TRUE" means
|    case-sensitive.)'

   *)

