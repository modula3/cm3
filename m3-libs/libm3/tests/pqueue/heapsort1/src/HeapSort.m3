(* Copyright 1993 Digital Equipment Corporation.                             *)
(* Distributed only by permission.                                           *)
(*                                                                           *)
(* Last modified on Fri Aug 13 12:04:07 PDT 1993 by heydon                   *)

MODULE HeapSort EXPORTS Main;

IMPORT IntPQ, Random, Stdio, Wr, Fmt, Thread;

<* FATAL Wr.Failure, Thread.Alerted *>

CONST NumElts = 20;

VAR
  pq := NEW(IntPQ.Default).init(sizeHint := NumElts);
  random := NEW(Random.Default).init(fixed := FALSE);

BEGIN
  FOR i := 1 TO NumElts DO
    VAR p := random.integer(min := 0, max := NumElts); BEGIN
      pq.insert(NEW(IntPQ.Elt, priority := p))
    END
  END;
  WHILE pq.size() > 0 DO
    <* FATAL IntPQ.Empty *>
    VAR elt := pq.deleteMin(); BEGIN
      Wr.PutText(Stdio.stdout, Fmt.Int(elt.priority) & " ")
    END
  END;
  Wr.PutChar(Stdio.stdout, '\n');
  Wr.Flush(Stdio.stdout)
END HeapSort.
