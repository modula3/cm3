(* $Id$ *)

MODULE ReadLineHelpNode;

PROCEDURE New(name, short, long : TEXT; children : List) : T =
  BEGIN
    RETURN NEW(T, name := name, short := short, long := long,
               children := children)
  END New;

PROCEDURE NewList(READONLY a : Arr) : List =
  VAR
    res : List := NIL;
  BEGIN
    FOR i := LAST(a) TO FIRST(a) BY -1 DO
      res := NEW(List, head := a[i], tail := res)
    END;
    RETURN res
  END NewList;

PROCEDURE ExtendNode(node : T; list : List) : T =
  BEGIN
    WITH new = NEW(T) DO
      new.short := node.short;
      new.long := node.long;      
      new.name := node.name;

      VAR pp := list; p := list.tail; BEGIN
        WHILE p # NIL DO
          pp := p; p := p.tail
        END;
        p := node.children;
        WHILE p # NIL DO
          pp.tail := NEW(List, head := p.head);
          pp := pp.tail;
          p := p.tail
        END;
        pp.tail := NIL;
        new.children := list
      END;

      RETURN new
    END
  END ExtendNode;

BEGIN END ReadLineHelpNode.
