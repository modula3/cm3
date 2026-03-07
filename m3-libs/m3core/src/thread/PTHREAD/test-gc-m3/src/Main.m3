(*
 * Main.m3 — Stress-test GC stack scanning on ARM64
 *
 * Creates multiple threads that allocate traced objects in tight loops.
 * On ARM64 with M3_STACK_ADJUST=0, the GC may miss heap pointers stored
 * in the red zone (128 bytes below SP), causing premature collection
 * and use-after-free.
 *
 * A healthy run prints all "PASS" lines.  A buggy GC causes SIGSEGV,
 * SIGILL (PAC failure on Apple Silicon), or detected corruption.
 *)

MODULE Main;

IMPORT Thread, IO, Fmt, Word;

CONST
  NumThreads = 8;
  Iterations = 100000;
  ChainLen   = 20;    (* length of linked list per iteration *)

TYPE
  Node = REF RECORD
    val:  INTEGER;
    next: Node;
  END;

  Closure = Thread.Closure OBJECT
    id: INTEGER;
  OVERRIDES
    apply := Worker;
  END;

VAR
  threads: ARRAY [0..NumThreads-1] OF Thread.T;
  results: ARRAY [0..NumThreads-1] OF INTEGER;

(* Build a linked list of ChainLen nodes, then walk it and sum values.
   The intermediate nodes are only reachable through local variables
   on this thread's stack — exactly what the GC must scan correctly. *)
PROCEDURE Worker(cl: Closure): REFANY =
  VAR
    head, p: Node;
    sum: INTEGER;
    errors := 0;
  BEGIN
    FOR i := 0 TO Iterations - 1 DO
      (* Allocate a chain — all nodes traced, only reachable from stack *)
      head := NEW(Node, val := i, next := NIL);
      p := head;
      FOR j := 1 TO ChainLen - 1 DO
        p.next := NEW(Node, val := i + j, next := NIL);
        p := p.next;
      END;

      (* Walk the chain and verify values.  If GC collected any node
         prematurely, we'll read garbage or crash. *)
      sum := 0;
      p := head;
      FOR j := 0 TO ChainLen - 1 DO
        IF p = NIL THEN
          INC(errors);
          EXIT;
        END;
        IF p.val # i + j THEN
          INC(errors);
        END;
        sum := Word.Plus(sum, p.val);
        p := p.next;
      END;
    END;

    results[cl.id] := errors;
    RETURN NIL;
  END Worker;

BEGIN
  IO.Put("GC stress test: " & Fmt.Int(NumThreads) & " threads x "
         & Fmt.Int(Iterations) & " iterations x "
         & Fmt.Int(ChainLen) & " node chains\n");

  (* Launch workers *)
  FOR i := 0 TO NumThreads - 1 DO
    threads[i] := Thread.Fork(NEW(Closure, id := i));
  END;

  (* Wait for all *)
  FOR i := 0 TO NumThreads - 1 DO
    EVAL Thread.Join(threads[i]);
  END;

  (* Report *)
  VAR total := 0; BEGIN
    FOR i := 0 TO NumThreads - 1 DO
      total := total + results[i];
      IF results[i] > 0 THEN
        IO.Put("  Thread " & Fmt.Int(i) & ": " & Fmt.Int(results[i])
               & " errors\n");
      END;
    END;
    IF total = 0 THEN
      IO.Put("PASS: no corruption detected\n");
    ELSE
      IO.Put("FAIL: " & Fmt.Int(total) & " total corruptions\n");
    END;
  END;
END Main.
