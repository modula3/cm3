(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Tue Sep 22 16:48:53 PDT 1992 by meehan     *)
(*      modified on Tue Jun 16 21:55:40 PDT 1992 by muller     *)
<* PRAGMA LL *>

(* {\bf WARNING:} This interface is very tentative. It is likely
   to change quite soon. 

   A "FormsEditVBT" is a filter whose child is a "FormsVBT".  When
   you initialize a "FormsEditVBT" with a file or other
   description, you get a second "FormsVBT", the {\it editor},
   which can installed and used to edit the filter's child.

   The stand-alone application called "formsedit" is the
   major client of this interface, but any program that
   constructs "FormsVBT"'s can do so through this interface and get
   an editor that can change the form dynamically.  This could be
   helpful during the development of an application, where
   changes can be made as the application is running.  Obviously,
   you could do major damage to the application (by removing
   named VBTs, for instance), but having an online editor for
   changing colors and shapes, for example, could be valuable.

   When the editor parses a new description, it constructs a new
   "FormsVBT" and replaces the filter's child with it, discarding
   the previous child, if any.  It also copies all the attached
   procedures from the old child to the new child.  Doing so will
   raise an error if the attachment fails, typically because the
   new form lacks a named VBT that the old form contained.  No
   other state-information is preserved, such as the contents of
   text-fields. *)

INTERFACE FormsEditVBT;

IMPORT Filter, FormsVBT, Thread, TrestleComm, XParam;

TYPE
  T <: Public;
  Public = Filter.T OBJECT
           METHODS
             editor (): FormsVBT.T;
             init (description: TEXT := NIL): T
                   RAISES {FormsVBT.Error}; <* LL = VBT.mu *>
             initFromFile (filename: TEXT): T
                           RAISES {FormsVBT.Error, Thread.Alerted};
             <* LL = VBT.mu *>
           END;

(* The "init" and "initFromFile" methods are nearly the same as
   those in the "FormsVBT" interface.  The "raw" parameter is
   not supported, and the default value for "description" is
   "NIL", which causes a ``dummy'' form to be used. *)

  EditorRoot <: PublicRoot;
  PublicRoot =
    Thread.Closure OBJECT
    METHODS
      init (v: T; Xdisplay := ":0.0"; Xgeometry := "-50-50"):
            EditorRoot RAISES {TrestleComm.Failure, XParam.Error}
    END;

END FormsEditVBT.

(* Once a "FormsEditVBT" has been created, you use it to initialize
   an EditorRoot, along with specifications as to its initial
   placement.  The EditorRoot's "init" method will install the
   VBT in Trestle.  Then you call "Thread.Fork" on the
   initialized EditorRoot to start an editor.  The EditorRoot's
   "apply" method will await the deletion of this and other VBTs:
   since the editor has facilities for opening other files, it
   may spawn other threads, all of which are connected to the
   EditorRoot.  If you Alert the EditorRoot thread, or if the
   user activates the Quit button, all the threads will be be
   alerted, and the EditorRoot thread will return.  If the user
   closes all the windows, the EditorRoot thread will return.

| TYPE MyEd = FormsEditVBT.T OBJECT ... END;
| WITH v = NEW (MyEd, ...).initFromFile ("myfile.fv"),
|      eroot = NEW (FormsEditVBT.EditorRoot).init (v,
|                   ":0.2"),
|      thread = Thread.Fork (eroot) DO
|   EVAL Thread.Join (thread)
|   END;

   *)

