(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Tue Jul 28 23:35:18 PDT 1992 by johnh *)
(*      modified on Mon Jul 20 16:24:44 PDT 1992 by sclafani *)

INTERFACE ZeusDataView;

IMPORT View;
<* PRAGMA LL *>

TYPE
  T <: View.T;

PROCEDURE New (): View.T;  <* LL.sup <= VBT.mu *>

END ZeusDataView.
