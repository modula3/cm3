(* Copyright 1993 Digital Equipment Corporation.                             *)
(* Distributed only by permission.                                           *)
(*                                                                           *)
(* Last modified on Fri Aug 13 12:04:20 PDT 1993 by heydon                   *)

MODULE HeapSort EXPORTS Main;

IMPORT IntPQ, Random, Stdio, Wr, Fmt, Thread;

<* FATAL Wr.Failure, Thread.Alerted *>

CONST NumElts = 20;

VAR
  elt: ARRAY [1..NumElts] OF IntPQ.Elt;
  pq: IntPQ.T;
  random := NEW(Random.Default).init(fixed := FALSE);

BEGIN
  FOR i := 1 TO NumElts DO
    VAR p := random.integer(min := 0, max := NumElts); BEGIN
      elt[i] := NEW(IntPQ.Elt, priority := p)
    END
  END;
  pq := NEW(IntPQ.Default).fromArray(elt);
  WHILE pq.size() > 0 DO
    <* FATAL IntPQ.Empty *>
    VAR e := pq.deleteMin(); BEGIN
      Wr.PutText(Stdio.stdout, Fmt.Int(e.priority) & " ")
    END
  END;
  Wr.PutChar(Stdio.stdout, '\n');
  Wr.Flush(Stdio.stdout)
END HeapSort.
