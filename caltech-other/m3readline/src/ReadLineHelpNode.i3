(* $Id$ *)

INTERFACE ReadLineHelpNode;

TYPE
  T = OBJECT
    name  : TEXT;
    short : TEXT;
    long  : TEXT;
    children : List := NIL;
  END;

  List = OBJECT
    head : T;
    tail : List := NIL;
  END;

  Arr = ARRAY OF T;

PROCEDURE New(name, short, long : TEXT; children : List := NIL) : T;
PROCEDURE NewList(READONLY a : Arr) : List;

PROCEDURE ExtendNode(node : T; list : List) : T;
  (* copy node, extending its list with list (not copying list) *)

CONST Brand = "ReadLineHelpNode";

END ReadLineHelpNode.
