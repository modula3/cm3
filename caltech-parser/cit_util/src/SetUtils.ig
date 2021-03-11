GENERIC INTERFACE SetUtils(Elem, ElemSet);

(* assumes there is a procedure
|  Elem.Format(e: Elem.T);
*)

TYPE
  ElemT = Elem.T;
  T = ElemSet.T;

PROCEDURE Format(set: T; postDelim:=" "; skipLastDelim:=TRUE): TEXT;

END SetUtils.
