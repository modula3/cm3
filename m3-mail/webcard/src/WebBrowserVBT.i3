(* Copyright 1996 Digital Equipment Corporation.              *)
(* Distributed only by permission.                            *)
(*                                                            *)
(* Last modified on Mon Aug 19 21:59:54 PDT 1996 by mhb       *)

INTERFACE WebBrowserVBT;

IMPORT FormsVBT, TextList, VBT;

TYPE 

  (* caller can override symbols DetachOpen2 and CloseOpen2 *)

  T <: Public;
  Public =
    FormsVBT.T OBJECT 
    METHODS
      <* LL <= VBT.mu *>
      init (): T;

      <* LL = VBT.mu *>
      visit (url: TEXT);
      (* pretend that user clicked on a link with absolute url "url" *)

      (* to be overriden: *)

      surf (base: TEXT; links: TextList.T);
      (* called when user hit "surf" button *)

      hotlink (link: TEXT; READONLY cd: VBT.MouseRec);
      (* called when user click on an anchor *)

    END;

END WebBrowserVBT.

