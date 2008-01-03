(* Copyright 1989 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* Last modified on Sun Mar 17 09:29:21 PST 1996 by mhb        *)
(*      modified on Wed Aug 17 15:55:06 PDT 1994 by bharat     *)
(*      modified on Wed Jun  9 20:44:43 PDT 1993 by meehan     *)
(*      modified on Tue Oct 22 10:36:46 PDT 1991 by steveg     *)
(*      modified on Mon Jan 15 16:48:10 PST 1990 by brooks     *)
(*      modified on Sun May 21 17:17:10 PDT 1989 by gidi       *)
<* PRAGMA LL *>
<* PRAGMA SUBTYPE *>

(* FormsVBT is a system for building graphical user interfaces.
   FormsVBT provides a special-purpose language for describing
   user interfaces, an interface-builder that allows editing of
   such descriptions, and a runtime library for applications to
   make use of the user interfaces.

   The locking level for any procedure in this interface that may
   alter an installed "VBT" is "LL.sup = VBT.mu". (See the 
   {\it Trestle Reference Manual\/} for a complete description
   of locking levels~\cite{TrestleTutorial}.) Most applications 
   don't need to worry about "VBT.mu" because their event-handlers 
   don't fork any threads that call FormsVBT. *)

INTERFACE FormsVBT;

IMPORT AnyEvent, Color, Filter, Rd, Rsrc, Sx, Thread, VBT,
       Wr, ZSplit;

EXCEPTION
  Error (TEXT);
  Unimplemented;
  Mismatch;

(* \section{Creation, allocation, and initialization} *)

(* An object "fv" of type "FormsVBT.T" (or simply, a {\it form}) is
   created by parsing an S-expression.  These expressions are usually
   stored in files with the suffix ".fv".  One way of creating a form
   is to call the procedure "NewFromFile", or the method
   "fv.initFromFile", with the name of such a file; the expression is
   parsed, and if there are no errors, a new "VBT" is created and
   stored in the form, which is returned.

   It is also possible for a program to generate a description
   ``on the fly'' and then use it to create a form.  The methods
   "fv.init", "fv.initFromRd", and "fv.initFromSx" support these
   options. Forms can also be stored in resources ("fv.initFromRsrc")
   and in URLs ("fv.initFromURL").  *)

TYPE
  T <: Public;
  <* SUBTYPE T <: MultiFilter.T *> (* ... *)
  Public = Filter.T OBJECT
           METHODS
             <* LL.sup <= VBT.mu *>
             init (description : TEXT;
                   raw         : BOOLEAN   := FALSE;
                   path        : Rsrc.Path := NIL    ): T
                   RAISES {Error};

             initFromFile (filename : TEXT;
                           raw      : BOOLEAN   := FALSE;
                           path     : Rsrc.Path := NIL    ): T
                 RAISES {Error, Rd.Failure, Thread.Alerted};

             initFromRd (rd   : Rd.T;
                         raw  : BOOLEAN   := FALSE;
                         path : Rsrc.Path := NIL    ): T
                 RAISES {Error, Rd.Failure, Thread.Alerted};

             initFromSx (sx   : Sx.T;
                         raw  : BOOLEAN   := FALSE;
                         path : Rsrc.Path := NIL    ): T
                 RAISES {Error};

             initFromRsrc (name : TEXT;
                           path : Rsrc.Path;
                           raw  : BOOLEAN    := FALSE): T
                 RAISES {Error, Rd.Failure,
                         Rsrc.NotFound, Thread.Alerted};

             initFromURL(baseURL : TEXT;
                         raw     : BOOLEAN := FALSE): T
                 RAISES {Error, Rd.Failure, Thread.Alerted};

             realize (type, name: TEXT): VBT.T RAISES {Error};

             <* LL.sup = VBT.mu *>
             snapshot (wr: Wr.T) RAISES {Error};
             restore  (rd: Rd.T) RAISES {Mismatch, Error};

           END;

(* The call "fv.init(description, raw, path)" initializes "fv" as a form
   and returns "fv".  It creates a "VBT", {\it v}, from "description",
   which must contain a single, valid S-expression.  The methods
   "initFromFile", "initFromRd", "initFromSx", "initFromRsrc", and
   "initFromURL" provide analogous support for files, readers,
   S-expressions, and named resources.

   The "raw" parameter is used to control that actual internal structure
   "fv".  Regardless of the value of "raw," "fv" is a multi-filter and
   "MultiFilter.Child(fv)" will always return {\it v}.  Internally, "fv" is
   a filter; if "raw" is "TRUE", then the filter's child is {\it v}.
   Otherwise, "fv" is ``cooked'', which means there are several filters
   inserted between {\it v} and "fv", so that the filter's child has the
   following structure:

| (ZSplit
|   (Filter
|     (HighlightVBT
|       (Filter `{\it v}`))))

   The filter above {\it v} supports the common case of making an entire
   form passive without requiring an explicit "Filter" interactor in the
   description.  It also functions to restore the keyboard focus to
   whichever of the form's descendant-"VBT"s had most recently acquired the
   keyboard focus.  The "ZSplit" supports menus and other pop-up
   operations, even if there is no "ZSplit" explicitly mentioned in the
   description.  To get the "ZSplit" that is inserted, use "GetZSplit".
   Clients should not traverse a cooked form directly.  We reserve the
   right to change the filters that are inserted.

   The "path" parameter is used for looking up all resources that are
   mentioned in the form: the name of a "Pixmap" or "Image"; a
   file for "Insert"; the "ItemFromFile" property on "Browser" and
   "MultiBrowser"; and the "From" property on "Text", "TypeIn", and
   "TextEdit".  (The "initFromURL" looks up resources as URLs, relative to
   "baseURL".)

   Briefly, the description of a form is an S-expression whose first
   element is the name of a component (e.g., "HBox"), and whose other
   elements are either properties (e.g., "Color"), or other components,
   typically describing the "VBT"-children of the outer component.

   The "VBT"-tree is created during a depth-first traversal of the
   S-expression.  On the way down, each "VBT" is {\it allocated}, typically
   with a call to "NEW(...)".  Then the subexpressions, if any, are
   traversed.  On the way back up, each "VBT" is {\it initialized},
   typically with a call to "v.init(...)".  The result is returned to the
   caller, where it is typically an argument to the parent's "init" method.

   In other words, allocation occurs top-down, and initialization occurs
   bottom-up.  (For more details on allocation, see Section~\ref{realize}.)

   For each subexpression, the parser produces a "VBT" whose type is
   defined in the "FVTypes" interface, and whose name corresponds to the
   first element of the subexpression.  For example, from the S-expression
   "(HBox ...)", the parser creates an object of type "FVTypes.FVHBox". *)

PROCEDURE NewFromFile (filename: TEXT;
                       raw                 := FALSE;
                       path    : Rsrc.Path := NIL    ): T
  RAISES {Error, Rd.Failure, Thread.Alerted};
(* Create a new form from the description in the file.  "Rd.EndOfFile" is
   signalled as "Error".

   Equivalent to "NEW(T).initFromFile (name, raw, path)" *)

PROCEDURE GetZSplit (fv: T): ZSplit.T RAISES {Error};
(* Return the "ZSplit" that ``cooked'' mode inserts.  An
   exception is raised if "fv" was created with "raw =
   TRUE". *)

(*
\section{Events and Symbols} \label{sec:programming-events}
\subsection{Attaching event-handlers}

   Most interactive components in the user interface generate
   events. (See the Appendix~\ref{ap:longcatalog} for a
   description of all components.) To register an event-handler for such a component,
   the component must be named, and the client must call "Attach"
   or "AttachProc", giving the name of the component and a
   procedure to be called when an event occurs in that
   component. *)

PROCEDURE Attach (fv: T; name: TEXT; cl: Closure) RAISES {Error};
(* Attach an event-handler (``callback'') to the component of "fv"
   whose name is given by "name".  If there is no such component, or
   if that component does not generate events (e.g., "Text"), then
   "Error" will be raised.  If "cl" is "NIL", then any existing
   event-handler for that component is removed.  Otherwise, when an
   event occurs in the named component, the implementation calls
|    cl.apply(fv, name, `{\it time}`)
   *)

TYPE
  Closure = OBJECT
            METHODS
              apply (fv: T; name: TEXT; time: VBT.TimeStamp);
            END;

PROCEDURE AttachProc (fv       : T;
                      name     : TEXT;
                      p        : Proc;
                      eventData: REFANY := NIL) RAISES {Error};
(* This is an alternate, somewhat simpler way to attach an
   event-handler.  When an event occurs in the named component,
   the implementation calls
| p(fv, name, eventData, `{\it time}`)
   *)

TYPE
  Proc = PROCEDURE (fv       : T;
                    name     : TEXT;
                    eventData: REFANY;
                    time     : VBT.TimeStamp);

(* These event-handlers do not provide any other details, such as
   what key or mouse button was pressed, or whether it was a
   double-click.  If such information is needed, call
   "GetTheEvent" to retrieve it. *)

PROCEDURE AttachEditOps (fv        : T;
                         editorName: TEXT;
                         cut, copy, paste, clear,
                         selectAll, undo, redo,
                         findFirst, findNext, findPrev: TEXT := NIL)
  RAISES {Error};                <* LL.sup = VBT.mu *>
(* Create and attach event-handlers for common editing operations. *)

(* "editorName" must be the name of a text-editing component:
   "TextEdit", "TypeIn", "Numeric", or "Typescript". If "cut" is not
   "NIL", then it must be the name of a component (typically a
   menu-button), and "AttachEditOps" will create an event-handler for
   it that will invoke the {\bf Cut} operation on the text-editing
   component.  Similarly, if "copy" is not "NIL", then it should name
   a component for which "AttachEditOps" will create an event-handler
   that invokes the {\bf Copy} operation on the text-editing
   component. Likewise for "paste", "clear", and so on. *)

(* \subsection{Access to the current event} *)

PROCEDURE GetTheEvent     (fv: T): AnyEvent.T    RAISES {Error};
PROCEDURE GetTheEventTime (fv: T): VBT.TimeStamp RAISES {Error};
(* Retrieve the details of the event that is currently in
   progress.  These routines may be called only during the
   dynamic extent of an event-handler attached to some component
   via "Attach" or "AttachProc". *)

PROCEDURE MakeEvent (fv: T; name: TEXT; time: VBT.TimeStamp)
  RAISES {Error};
(* "MakeEvent" invokes the event-handler for the component of "fv"
   whose name is "name". A component has an event-handler if attached
   via "Attach" or "AttachProc", or if the component is a PopButton,
   PopMButton, PageButton, PageMButton, LinkButton, or LinkMButton.
*)

(* "MakeEvent" is useful when one part of a large program wishes to
   communicate with another part, by pretending that the named event
   occurred. For example, a client might want typing a particular
   control-character in a text-editing component to have
   the same effect as selecting a menu-item such as ``Quit.''
   "MakeEvent" provides a way to link the two events to the same
   handler. *)

VAR MakeEventMiscCodeType: VBT.MiscCodeType; (* CONST *)
(* The exact type of the result of "GetTheEvent" depends on the user
   action that caused the event to be generated, a key, a mouse-click,
   etc.  If the event was actually caused by a call to "MakeEvent",
   the type of the result will be "AnyEvent.Misc", and the value of
   its "type" field will be "MakeEventMiscCodeType". *)


(* \subsection{Symbol management} *)

PROCEDURE AddSymbol (fv: T; name: TEXT) RAISES {Error};
(* Add a ``virtual'' component to "fv" with the given "name".
   The form will behave as if there was a component called "name"
   (i.e., the call "GetVBT(fv, name)" will return a valid
   "VBT").

   This procedure is most useful as a means to communicate
   between distant parts of a large program.  One part of the
   program would use "AddSymbol" to create a new symbol; another
   part would call "MakeEvent" to invoke an event-handler for the
   symbol.

   "Error" is raised if "name" is already defined in "fv". *)

PROCEDURE AddUniqueSymbol (fv: T): TEXT;
(* Just like "AddSymbol", but finds a name that has not been used
   yet.  The name is returned. *)

(* \section{Reading and Changing State}

   \label{sec:programming-state}

   In response to an event or other occurrence, a program may
   want to read or change the state of various interactors in the
   form.  This is handled by the various Get and Put procedures.
   Get procedures take the form and the name of the interactor,
   and return its value.  Put procedures take the form, the name
   of the interactor, and the new value to be set.

   There are several Get procedures and several Put procedures,
   for convenient handling of various Modula-3 types.  These should
   be used as appropriate to the type of the interactor:
   "GetText" for a "TypeIn", "GetInteger" for a "Numeric",
   "GetBoolean" for a "Boolean" or "Choice", etc. However, some
   conversions are supported: "PutInteger" to a "TypeIn" will
   convert the integer into text; "GetInteger" will likewise
   attempt to convert the text of the "TypeIn" to an integer
   (and return 0 in case of failure).  All Get and Put
   procedures, however, will raise "Error" if applied to a
   component that does not have a value.

   \subsection{Access to the {\tt Main} and {\tt Value} properties}
*) 

PROCEDURE GetText (fv: T; name: TEXT): TEXT
  RAISES {Error, Unimplemented};
(* This is implemented for "Browser", "FileBrowser", "Numeric",
   "Text", "Typescript", and the text-interactors: "TextEdit",
   "TypeIn", and "TextArea". *)
   
PROCEDURE PutText (fv: T; name: TEXT; t: TEXT; append := FALSE)
  RAISES {Error, Unimplemented};
(* This is implemented for "Browser", "FileBrowser", "Pixmap", "Text",
   "Typescript", and the text-interactors: "TextEdit", "TypeIn", and
   "TextArea".  For "Text" and the text-interactors, if "append" is
   true, then "t" is added to the end of the current text, rather than
   replacing it. *)

PROCEDURE GetInteger (fv: T; name: TEXT): INTEGER
  RAISES {Error, Unimplemented};
PROCEDURE PutInteger (fv: T; name: TEXT; n: INTEGER)
  RAISES {Error, Unimplemented};
(* These are implemented for "Browser", "Numeric", "Scroller", and
   "TSplit". "PutInteger" only is implemented for "Audio". *)

(* If you use "PutInteger" to select the "n"th child of a "TSplit" and
   that child has a text-editing component that has the "FirstFocus"
   property, then the text-editor will acquire the keyboard focus, and
   if it's a "TypeIn", its text will be selected in replace-mode. *)

PROCEDURE GetBoolean (fv: T; name: TEXT): BOOLEAN
  RAISES {Error, Unimplemented};
PROCEDURE PutBoolean (fv: T; name: TEXT; val: BOOLEAN)
  RAISES {Error, Unimplemented};
(* These are implemented for "Boolean" and "Choice". *)

(* \subsection{Access to arbitrary properties}

   FormsVBT provides access to properties other than "Main" and
   "Value".  The intention is to provide access to all the inherited
   and class properties.  For example, the "Scroller" component has an
   integer-valued property named "Min", so it should be possible to
   call
| GetIntegerProperty(fv, name, "Min")
   to retrieve that value, or
| PutIntegerProperty(fv, name, "Min", 6)
   to change the value to 6.
 
   \vspace {5mm}
  {\bf WARNING: The current implementation provides access
   only to the inherited properties, and even that access is limited.}
   \vspace {5mm}

   Note also that changing the value of a property in a component will not affect
   its subcomponents. *)

PROCEDURE GetTextProperty (fv: T; name, propertyName: TEXT): TEXT
  RAISES {Error, Unimplemented};
(* This is implemented for the "Font" and "LabelFont" properties for
   all components, as well as the "Items" and "Select" properties of "Browser"s,
   and the "ActiveTarget" property of "Source"s. *)
  
PROCEDURE PutTextProperty (fv: T; name, propertyName: TEXT; value: TEXT)
  RAISES {Error, Unimplemented};
(* This is implemented for the "Color", "BgColor", "Font", and
   "LabelFont" properties for all components, as well as the "Items" and "Select" 
   properties of "Browser"s. *)

PROCEDURE GetIntegerProperty (fv: T; name, propertyName: TEXT):
  INTEGER RAISES {Error, Unimplemented};
PROCEDURE PutIntegerProperty (fv                : T;
                              name, propertyName: TEXT;
                              value             : INTEGER)
  RAISES {Error, Unimplemented};
(* This is implemented for the "Min" and "Max" properties of
   "Numeric"s; the "Min", "Max", "Step", and "Thumb" properties
   of "Scroller"s; and the "Quality", "ImageWidth", "ImageHeight"
   and "MSecs" properties of "Video"s, and the "NorthEdge",
   "SouthEdge", "EastEdge" and "WestEdge" properties of all
   VBTs.*)

PROCEDURE GetRealProperty (fv: T; name, propertyName: TEXT): REAL
  RAISES {Error, Unimplemented};
PROCEDURE PutRealProperty (fv                : T;
                           name, propertyName: TEXT;
                           value             : REAL  )
  RAISES {Error, Unimplemented};
(* This is implemented for the "HScale" and "VScale" properties of
   "Scale"s. *)

PROCEDURE GetColorProperty (fv: T; name, property: TEXT): Color.T
  RAISES {Error, Unimplemented};
(* Return the color used by the named component. "property" must
   be one of "Color", "BgColor", "LightShadow", or "DarkShadow". *)

PROCEDURE PutColorProperty (         fv            : T;
                                     name, property: TEXT;
                            READONLY color         : Color.T)
  RAISES {Error, Unimplemented};
(* Set the color used by the named component. "property" must be
   "Color" or "BgColor". *)

PROCEDURE GetBooleanProperty (fv: T; name, propertyName: TEXT):
  BOOLEAN RAISES {Error, Unimplemented};
PROCEDURE PutBooleanProperty (fv                : T;
                              name, propertyName: TEXT;
                              value             : BOOLEAN)
  RAISES {Error, Unimplemented};
 (* This is implemented for the "ReadOnly" property of
   "TextEdit"s, and the shadow styles of "Frame"s.  The
   "PutBooleanProperty" is implemented for the "Synchronous",
   "Paused" and "FixedSize" properties of "Video", and for the
   "Mute" and "MuteWhenUnmapped" properties of "Audio" *)

(* \subsection{Access to the underlying {\tt VBT}s} *)

PROCEDURE GetVBT (fv: T; name: TEXT): VBT.T RAISES {Error};
(* Return the "VBT" corresponding to a named interactor in "fv".
   "Error" is raised if there is no such "VBT". *)

PROCEDURE GetName (vbt: VBT.T): TEXT RAISES {Error};
(* If "vbt" is the "VBT" corresponding to a named interactor
   in some form, returns the name given to that interactor.
   Otherwise, raisses "Error". *)


(* \subsection{Radios and Choices} *)

PROCEDURE GetChoice (fv: T; radioName: TEXT): TEXT
  RAISES {Error, Unimplemented};
PROCEDURE PutChoice (fv: T; radioName, choiceName: TEXT)
  RAISES {Error, Unimplemented};
(* Get/Put the name of the selected "Choice" in a radio-button
   group. If there is no selection, "GetChoice" returns "NIL".
   If "choiceName" is "NIL", the radio-group will have no selection. *)

PROCEDURE MakeSelected (fv: T; choiceName: TEXT) RAISES {Error};
PROCEDURE IsSelected (fv: T; choiceName: TEXT): BOOLEAN
  RAISES {Error};
(* Set/test a "Choice"-button without referring to its group. *)

(* \subsection{Generic interactors} *)

PROCEDURE GetGeneric (fv: T; genericName: TEXT): VBT.T
  RAISES {Error};
(* Retrieve the "VBT" used by the named "Generic" interactor. *)

PROCEDURE PutGeneric (fv: T; genericName: TEXT; vbt: VBT.T)
  RAISES {Error};
(* Replace the named "Generic" interactor with "vbt", which may
   be "NIL".  When "NIL" is specified, a default (and initial)
   "VBT" is used: a "TextureVBT" with 0 size and 0 stretch in
   each dimension. *)

(* \subsection{Special controls for Filters}

   The "(Filter ...)" expression in FormsVBT supports a feature called
   {\em reactivity}.\index{reactivity} This has one of four states:
   Active, Passive, Dormant, or Vanished.  The state can be specified
   in the description and changed by the application at runtime.  The
   default state is Active.  In the Passive state, the component and
   its descendants, if any, are unresponsive to mouse clicks.  The
   Dormant state is like Passive, but the component and descendants
   are ``grayed out.'' Dormant is often to be preferred over Passive,
   because it provide additional feedback to the user.  In the
   Vanished state, the component becomes unreactive and disappears
   entirely. 

   A cursor is specified when the state is set, and the name is interpretted
   by the Trestle implementation. An empty string (the default value)
   indicates that you don't care about the cursor shape. 
   
   Standard X screentypes support the cursors named in {\it X Window
   System} by Scheifler et.  al. \cite{XSpec} Appendix B. Therefore, for
   example, "XC_arrow" returns a cursor that behaves like the X arrow 
   cursor on X screentypes, and like the default cursor on screentypes 
   that have no cursor named "XC_arrow". *)


PROCEDURE MakeActive  (fv: T; name: TEXT; cursor:= "") RAISES {Error};
PROCEDURE MakePassive (fv: T; name: TEXT; cursor:= "") RAISES {Error};
PROCEDURE MakeDormant (fv: T; name: TEXT; cursor:= "") RAISES {Error};
PROCEDURE MakeVanish  (fv: T; name: TEXT; cursor:= "") RAISES {Error};
(* Find the nearest ancestor of the named component that is of
   type "FVFilter", and set its state and cursor as indicated. 
   The exception is raised if no such ancestor can be found. *)

PROCEDURE IsActive   (fv: T; name: TEXT): BOOLEAN RAISES {Error};
PROCEDURE IsPassive  (fv: T; name: TEXT): BOOLEAN RAISES {Error};
PROCEDURE IsDormant  (fv: T; name: TEXT): BOOLEAN RAISES {Error};
PROCEDURE IsVanished (fv: T; name: TEXT): BOOLEAN RAISES {Error};
(* Find the nearest ancestor of the named component that is of
   type "FVFilter", and test its state as indicated.  The
   exception is raised if no such ancestor can be found. *)


(* \subsection{Access to Subwindows} *)

PROCEDURE PopUp (fv        : T;
                 name      : TEXT;
                 forcePlace: BOOLEAN         := FALSE;
                 time      : VBT.TimeStamp := 0   )
  RAISES {Error};
(* Pop up the named subwindow. *)

(* Assuming that "name" is the name of an element of "fv" that
   can be popped up, pop it up.  That is, the named element must
   be a non-background child of a "ZSplit", or some descendant
   thereof.  In the latter case, the ancestor that is a direct
   child of the "ZSplit" will be the thing popped up.  Call this
   ancestor {\it zchild}.  "PopUp" is equivalent to activating

| (PopButton (For `{\it zchild}`) ...)

   If the target {\it zchild} is already open or has been opened
   before and has been moved by the user (to a location that is
   now visible), it will normally be left where the user left it.
   The "forcePlace" option will force it instead to be returned
   to its canonical place.

   If the subwindow contains a text-editing component that has the
   "FirstFocus" property, then that component will acquire the keyboard
   focus, and if it's a "TypeinVBT.T", its text will be selected in
   replace-mode. *)

PROCEDURE PopDown (fv: T; name: TEXT) RAISES {Error};
(* The inverse of "PopUp": make the named element (or suitable
   ancestor) invisible.  This is implemented using "ZSplit"'s
   unmapping.  (Unfortunately, this doesn't cause the keyboard
   focus to be lost.)  The exception is raised if "name" is not
   the name of an element of "fv". *)

(* \subsection{Special controls for text-interactors} *) 

PROCEDURE TakeFocus (fv       : T;
                     name     : TEXT;
                     eventTime: VBT.TimeStamp;
                     select                     := FALSE)
  RAISES {Error};
(* Give the keyboard focus to a specified interactor.  An exception is raised
   if the interactor is not of a suitable class to take it; however, no
   exception is raised if the keyboard focus cannot be taken because of a
   timeout, i.e., an invalid "eventTime".  If "select" is "TRUE" and the focus
   was taken, then select the entire contents of the interactor's "TextPort"
   as a primary selection in replace-mode. *)

(* \section{Saving and restoring state} \label{sec:programming-snapshot} *)

(* FormsVBT allows clients
   to save and restore the entire state of a
   form. 

   A {\em snapshot\/} is an S-expression that captures the state of components
   in a form.  The call "fv.snapshot(wr)" writes a snapshot of "fv" to the
   writer "wr", and the call "fv.restore(rd)" reads a snapshot from the reader
   "rd" and restores the components of "fv" to the state in
   the snapshot.

   A snapshot produced by the default method contains only those named
   components that have a modifiable value.  More precisely, a
   component is part of a snapshot if (1) it has a name and (2) the
   call to "GetText", "GetInteger", "GetReal", "GetBoolean", or
   "GetChoice" does not raise an exception. If you want to include
   a component into the snapshot that has state but does not respond
   to "GetText", "GetInteger", etc., then you need to override
   the defaults methods.

   The "snapshot" method raises the "Error" exception if there is a problem
   writing the snapshot to the writer.

   The "restore" method raises the "Error" exception if there is a syntax
   error in the S-expression or if there is any type of problem with the
   reader.

   When restoring, the snapshot need not precisely match the set of
   interactors in the form.  If the snapshot lacks values for some
   fields that the form contains, those fields will be left alone.  If
   the snapshot has values for some fields that the form does not
   contain, the "restore" method should raise "Mismatch", but only
   after restoring all the values that do match.  If the snapshot has
   a value for a field that the form contains, but the types do not
   agree, this is a show-stopping error; the "restore" method should
   raise "Error".  Catching "Mismatch" is useful when you want to
   continue to tolerate snapshots from old versions of a form.

   The default "snapshot" and "restore" methods write S-expressions in the
   following format:

|    ((name1 value1)
|     (name2 value2) ...)

  *)

(* \section{Dynamic Alteration of Forms}
   \label{sec:programming-dynamicforms}

   FormsVBT provides facilities for modifying a form while a program is
   running.  For example, one might want to add or delete items in a menu.

   The procedure "Insert" parses a description of new form in the context
   of an existing form, and "Delete" removes a component and all of its
   descendents.

   The procedure "InsertVBT" is used for forms within forms, where the
   subforms are independent from the forms containing them to avoid name
   clashes.  You need to use "DeleteVBT" to delete a subform inserted this
   way.

   Any resizing that may be appropriate after the modifications to the form
   is performed automatically.  For the common case of modifying menus,
   this is not an issue because the menu is (almost certainly) not visible
   at the time the alteration takes place. *)

PROCEDURE Insert (fv         : T;
                  parent     : TEXT;
                  description: TEXT;
                  n          : CARDINAL := LAST(CARDINAL)): VBT.T
  RAISES {Error};
<* LL.sup = VBT.mu *>
(* "Insert" parses a description in the context of an existing form, that
   is, in "fv"'s namespace, so that names already defined in "fv" are
   visible while the description is being parsed, and with the state
   (color, resource-path, etc.) that was in effect for "parent".

   Once the new "VBT" is created, it is inserted into the named component,
   which must be a Split, as the "n"th child.  It is also returned. *)

PROCEDURE InsertFromFile (fv      : T;
                          parent  : TEXT;
                          filename: TEXT;
                          n       : CARDINAL := LAST(CARDINAL)): VBT.T
  RAISES {Error, Rd.Failure, Thread.Alerted};
  <* LL.sup = VBT.mu *>

PROCEDURE InsertFromRsrc (fv    : T;
                          parent: TEXT;
                          name  : TEXT;
                          path  : Rsrc.Path;
                          n     : CARDINAL    := LAST(CARDINAL)): VBT.T
  RAISES {Error, Rd.Failure, Rsrc.NotFound, Thread.Alerted};
<* LL.sup = VBT.mu *>
(* "InsertFromFile" and "InsertFromRsrc" read a description from a file or
   named resource, and then call "Insert". *)

PROCEDURE Delete (fv    : T;
                  parent: TEXT;
                  n     : CARDINAL;
                  count : CARDINAL   := 1) RAISES {Error};
  <* LL.sup = VBT.mu *>
(* Delete the children whose indices are in the range
   "[n .. (n + count - 1)]" from the named component, which must be a
   Split. The names of the "n" components, as well as the names
   of all of the desendants of those components, are removed from
   "fv"'s namespace. *)


PROCEDURE InsertVBT (fv    : T;
                     name  : TEXT;
                     child : VBT.T;
                     n     : CARDINAL := LAST (CARDINAL))
  RAISES {Error};
  <* LL.sup = VBT.mu *>
(* Insert "child" as the "n"th child of the named component, which must be
   a Split.  The names of components in "child" are not added to "fv"'s
   namespace.  Thus, "InsertVBT" is typically used for ``forms within
   forms.'' *)

PROCEDURE DeleteVBT (fv    : T;
                     name  : TEXT;
                     n : CARDINAL;
                     count    : CARDINAL := 1)
  RAISES {Error};
  <* LL.sup = VBT.mu *>
(* Like "Delete", this procedure deletes the children whose indices are in
   the range "[n ..  (n + count - 1)]" from the named component, which must
   be a Split.  Unlinke "Delete", the names of the "n" components, as well
   as the names of all of the desendants of those components, are {\it not}
   removed from "fv"'s namespace.  Thus, "DeleteVBT" is typically only used
   with children that were inserted using "InsertVBT". *)

END FormsVBT.

(* \section{Subclasses of components} \label{realize}

   As the subexpressions describing the form "fv" are being parsed, the
   "VBT"-components are created (allocated) by calling
| fv.realize(`{\it type}`, `{\it name}`)
   where {\it type} is the name of the first element of the subexpression,
   and {\it name} is the "Name" property specified in the
   subexpression, or the empty string if no such property was
   specified.  For example, if the description contains the
   expression
| (Menu %mainMenu ...)
   then the FormsVBT parser will call
| fv.realize("Menu", "mainMenu")
   to create the "VBT".

   By overriding the "realize" method of "fv", the client can
   create subtypes for any or all of the components.  For each
   kind of form, there is a corresponding type in the "FVTypes"
   interface.  For example, the result of parsing "(Menu ...)" is
   an object that is a subtype of "FVTypes.FVMenu".  The
   "realize" method must allocate and return a "VBT" that is a
   subtype of the corresponding type in "FVTypes".

   For example, suppose you wanted the form to keep a count of the
   number of menus it contains, and for each menu to store its own
   index.

| TYPE
|   MyForm = FormsVBT.T OBJECT
|              count: CARDINAL := 0
|            OVERRIDES
|              realize := Realize
|            END;
|   MyMenu = FVTypes.FVMenu OBJECT
|                index: CARDINAL
|            END;
|
| PROCEDURE Realize (fv: MyForm; type, name: TEXT):
|   VBT.T RAISES {FormsVBT.Error} =
|   BEGIN
|     IF Text.Equal (type, "Menu") THEN
|       WITH m = NEW (MyMenu, index := fv.count) DO
|         INC (fv.count);
|         RETURN m
|       END
|     ELSE                     (* use the default *)
|       RETURN FormsVBT.T.realize (fv, type, name)
|     END
|   END Realize;

   Note that the "realize" method does not {\it initialize} the
   "VBT" that it allocates.  Actually, it may initialize any {\it
   private} fields, such as the "index" field in this example,
   but the "VBT"'s "init" method should not be called inside the
   call to "fv.realize", since it will be called later during
   a ``bottom-up'' initialization phase.  Of course, the
   client may also override the "init" method to control what
   happens in that phase.

   A more complicated case arises with text-editing components.
   Textports are {\em contained} in three forms: "TextEdit",
   "Typescript", and "Numeric".  In a "TextEdit" components, the
   textport is in an exported field, "TextEditVBT.T.tp". If the
   "realize" method allocates a "TextPort.T", even a private subtype
   of "TextPort.T", it should not call the textport's "init" method,
   since FormsVBT will do that in the initialization phase, passing
   some of the current state information (such as background color and
   the width of the ``turn margin'') to the textport's "init" method.
   The same applies to "Typescript" components, since
   "TypescriptVBT.T" is a subtype of "TextEditVBT.T".  Similarly, the
   textport in a "Numeric" component is in an exported field,
   "NumericVBT.T.typein"; again, it may be allocated but not
   initialized in the "realize" method.

   If you wish to redefine the interpretation of keystrokes, you do so
   by overriding the "filter" method of the textports.  The following
   code illustrates how to do this.

| TYPE
|   MyForm = FormsVBT.T OBJECT
|               OVERRIDES realize := Realize END;
| 
| PROCEDURE Realize (fv: MyForm; type, name: TEXT): VBT.T
|   RAISES {FormsVBT.Error} =
|   BEGIN
|     IF Text.Equal (type, "TextEdit") THEN
|       RETURN
|         NEW (FVTypes.FVTextEdit,
|              tp := NEW (TextPort.T, filter := MyFilter))
|     ELSIF Text.Equal (type, "Numeric") THEN
|       RETURN NEW (FVTypes.FVNumeric,
|                   typein := NEW (NumericVBT.Typein,
|                                  filter := MyFilter))
|     ELSIF Text.Equal (type, "Typescript") THEN
|       RETURN NEW (FVTypes.FVTypescript,
|                   tp := NEW (TypescriptVBT.Port,
|                              filter := MyFilter))
|     ELSIF Text.Equal (type, "TypeIn") THEN
|       RETURN NEW (FVTypes.FVTypein, filter := MyFilter)
|     ELSE                         (* use the default *)
|       RETURN FormsVBT.T.realize (fv, type, name)
|     END
|   END Realize;


   The "realize" method can also be used to integrate any VBT,
   including leafs and filters into a form. The components "Any",
   "AnyFilter", and "AnySplit" are defined to be "VBT.Leaf",
   "VBT.Filter", and "VBT.Split" respectively.
*)
