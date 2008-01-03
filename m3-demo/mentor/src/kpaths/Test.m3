(* Copyright 1993 by Digital Equipment Corporation. *)

MODULE Test EXPORTS Main;

IMPORT Atom, ShortestPath, RefList, IO, Stdio, Rd, Sx, Fmt;

VAR 
  EdgeSym := Atom.FromText("Edge");
  VertexSym := Atom.FromText("Vertex");
  QuerySym := Atom.FromText("Find");

VAR t := NEW(ShortestPath.T).init();

PROCEDURE Int(x: REF INTEGER): INTEGER =
  BEGIN
    RETURN x^
  END Int;

VAR sx: RefList.T; BEGIN
  TRY
    LOOP
      TRY
        sx := Sx.Read(Stdio.stdin);
        IF sx = NIL THEN 
          RAISE Sx.ReadError("Because!")
        ELSIF sx.head = EdgeSym THEN
          t.addEdge(sx.tail)
        ELSIF sx.head = VertexSym THEN
          t.addVertex(sx.tail)
        ELSIF sx.head = QuerySym THEN
          Sx.Print(Stdio.stdout, t.shortestPath(sx.tail.head, sx.tail.tail.head, Int(sx.tail.tail.tail.head)));
          IO.Put("\n");
        END
      EXCEPT
        Sx.ReadError(why) => IO.Put("\nSx Read error (" & why & ") at " & Fmt.Int(Rd.Index(Stdio.stdin)))
      END
    END
  EXCEPT
    Rd.EndOfFile => (* SKIP *)
  | Sx.ReadError(why) => IO.Put("\nSx Read error (" & why & ") at " & Fmt.Int(Rd.Index(Stdio.stdin)))
  END
END Test.