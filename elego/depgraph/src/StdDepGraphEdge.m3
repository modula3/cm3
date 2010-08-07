(*--------------------------------------------------------------------------*)
MODULE StdDepGraphEdge;

(*--------------------------------------------------------------------------*)
PROCEDURE New(v : INTEGER) : T =
  VAR e := NEW(T);
  BEGIN
    e.val := v;
    RETURN e;
  END New;

BEGIN
END StdDepGraphEdge.
