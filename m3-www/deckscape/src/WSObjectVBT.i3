(* Copyright 1996 Digital Equipment Corporation.              *)
(* Distributed only by permission.                            *)
(*                                                            *)
(* Last modified on Mon Aug 19 22:13:34 PDT 1996 by mhb       *)

INTERFACE WSObjectVBT;

(* A WSObjectVBT.T (workspace object VBT) represents something
   that can appear in the workspace and contain documents, a
   DeckVBT.T or a FreeDocVBT.T. *)

IMPORT DocVBT, FormsVBT, VBT;


TYPE
  T <: Public;
  Public = FormsVBT.T OBJECT
           METHODS
             getWorkspace (): VBT.T;
             setWorkspace (v: VBT.T);
             (* to be overriden by client; no defaults: *)
             addDoc     (doc: DocVBT.T);
             remDoc     (doc: DocVBT.T);
             replaceDoc (old, new: DocVBT.T);
           END;
(* The FormsVBT.T mouse method is overriden
   to trap right mouse clicks to raise the object to the 'top'
   (if any of it is obscured) or to 'bottom' the object
   (otherwise). *)

END WSObjectVBT.
