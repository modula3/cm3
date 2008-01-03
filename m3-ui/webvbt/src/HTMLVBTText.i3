(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Sun Jul 23 21:14:19 PDT 1995 by mhb                      *)
<* PRAGMA LL *>

(* An "HTMLVBT" is a VBT class for diplaying HTML using text-only. *)

INTERFACE HTMLVBTText;

IMPORT HTML, HTMLVBT;

TYPE
  T <: Public;
  Public = HTMLVBT.T OBJECT
           METHODS
             <* LL <= VBT.mu *>
             init (html: HTML.T): T;
           END;

END HTMLVBTText.
