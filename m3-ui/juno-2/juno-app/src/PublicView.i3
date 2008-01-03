(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Wed Aug 16 13:41:54 1995 by gnelson                      *)
(*      modified on Wed Feb  3 16:51:38 PST 1993 by heydon                   *)

INTERFACE PublicView;

(* Defines a public view on a Juno module. *)

IMPORT TextPort, Editor;

TYPE
  T <: Public;
  Public = TextPort.T OBJECT METHODS
    init(e: Editor.T): T
  END;

(* The call "NEW(PublicView.T).init(e)" returns a new public view that
   displays the Juno parse tree "Editor.Trees(e)" in a read-only text port. The
   trees are unparsed to the new window width every time the public view is
   reshaped. *)

END PublicView.
