(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* Last modified on Wed Jun  9 12:42:44 PDT 1993 by meehan *)
(*      modified on Mon Jan  4 12:56:26 PST 1993 by mhb *)
(* modified on Fri Oct 16 10:21:13 PDT 1992 by steveg *)
<* PRAGMA LL *>

(* "XTrestle" checks for X-style ``"-display"'' and ``"-geometry"''
   command-line switches and installs a top-level window accordingly.
   If your application install more than one top-level window, you may
   find the routines in the "XParam" interface helpful. *)

INTERFACE XTrestle;

IMPORT TrestleComm, VBT;

EXCEPTION Error;

PROCEDURE Install (v          : VBT.T;
                   applName   : TEXT    := NIL;
                   inst       : TEXT    := NIL;
                   windowTitle: TEXT    := NIL;
                   iconTitle  : TEXT    := NIL;
                   bgColorR   : REAL    := -1.0;
                   bgColorG   : REAL    := -1.0;
                   bgColorB   : REAL    := -1.0;
                   iconWindow : VBT.T   := NIL   )
  RAISES {TrestleComm.Failure, Error};
<* LL.sup = VBT.mu *>
(* This is like "Trestle.Install" except that the locking level is
   different and the command line is parsed for X-style "-display" and
   "-geometry" options. *)

END XTrestle.

(* The syntax of these switches is described in the X manpage and in
   {\it The X Window System\/} \cite{XWindowSystem}.

   If there is a "-display" argument, it will be made the default
   Trestle connection for those procedures in the "Trestle" interface
   that take a "Trestle.T" as a parameter.

   The "TrestleComm.Failure" exception is raised if a call to
   "Trestle" raises that exception.  The "Error" exception is raised
   if the parameter following "-display" or "-geometry" contains any
   syntax errors (or is missing). *)
