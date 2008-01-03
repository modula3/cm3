(* Copyright (C) 1993, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* Last modified on Sun Sep  5 15:40:15 PDT 1993 by bharat *)
(* modified on Fri Jul 2 16:33:31 PDT 1993 by mhb *)
<* PRAGMA LL *>

(* All components (forms/frames/widgets) in "NodeVBT" are instances *)
(* of NodeVBT *)

INTERFACE RW;

IMPORT NodeVBT, Rd, Wr;


PROCEDURE rint(r : Rd.T; VAR i : INTEGER);
PROCEDURE rbool(r : Rd.T; VAR i : BOOLEAN);
PROCEDURE rtext(r : Rd.T; VAR i : TEXT);
PROCEDURE rcard(r : Rd.T; VAR i : CARDINAL);
PROCEDURE rtuple(r : Rd.T; VAR  i : NodeVBT.Tuple);


PROCEDURE wint(w : Wr.T; o : INTEGER);
PROCEDURE wbool(w : Wr.T; o : BOOLEAN);
PROCEDURE wtext(w : Wr.T;  o : TEXT);
PROCEDURE wcard(w : Wr.T;  o : CARDINAL);
PROCEDURE wtuple(w : Wr.T;  o : NodeVBT.Tuple);

PROCEDURE ptot(i : NodeVBT.T) : NodeVBT.Tuple;
PROCEDURE ttop(i : NodeVBT.Tuple) : NodeVBT.T;

END RW.













