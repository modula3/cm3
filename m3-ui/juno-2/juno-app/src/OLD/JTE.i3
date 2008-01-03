(* Copyright 1992 Digital Equipment Corporation                              *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Oct 12 16:48:40 PDT 1992 by heydon                   *)
(*      modified on Sat Aug 22 20:04:23 1992 by myers                        *)

INTERFACE JTE;

IMPORT VBT, Editor, JunoClosure AS Closure;

(* The procedure "New" creates a "Editor.T" inside various FormsVBT
   goop; it sets "te" to this text editor and returns the goop window,
   which is suitable for installing as a top level window.  The argument
   "filename" contains a filename which is placed into the editor; if it is
   "NIL" the editor is initially empty.  Only one call to "New" may be
   made.  After each successful parse of "te", the closure "c" is
   invoked. *)

PROCEDURE New (
  filename: TEXT;
  VAR (*OUT*) te: Editor.T;
  c: Closure.T := NIL): VBT.T;

PROCEDURE ApplicationError (msg: TEXT; near: VBT.T);
(* Display the message "msg" in "app_error", and insert "app_error" as a
   child in the first "FVTypes.FVZSplit" above "near" whose FormsVBT name
   is "z".  Its initial position will be near "near". *)

(* The VBT "app_error" is a global inside the module. *)

END JTE.
