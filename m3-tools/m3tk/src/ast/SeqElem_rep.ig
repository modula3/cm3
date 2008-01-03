(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

GENERIC INTERFACE SeqElem_rep(SeqElem, Elem);

REVEAL 
  SeqElem.Iter = OBJECT next: SeqElem.T := NIL; END BRANDED OBJECT 
    elem: Elem.T;
  END;

  SeqElem.T = SeqElem.Iter BRANDED OBJECT END;

END SeqElem_rep.
