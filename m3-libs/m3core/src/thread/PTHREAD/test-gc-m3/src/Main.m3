(* Main.m3 — Stress-test GC red zone scanning on ARM64
 *
 * Protocol:
 *   1. Tester allocates a traced Node, LOOPHOLEs to ADDRESS, NILs the ref
 *   2. Tester calls C RedZoneHold — C stores ptr in red zone, spins on flag
 *   3. Pressure threads allocate heavily, triggering GC collections
 *   4. Releaser thread sets the flag after a delay, releasing the C spin
 *   5. Tester checks if the object survived
 *
 * The GC window is step 3: the tester thread is suspended in the C spin
 * loop with the pointer ONLY in the red zone.  If M3_STACK_ADJUST=0,
 * the GC misses it and may collect the object.
 *)

UNSAFE MODULE Main;

IMPORT Thread, IO, Fmt, RedZone;

CONST
  NumPressure = 4;
  Rounds      = 500;
  PressureAllocs = 500000;  (* per pressure thread per round *)
  Magic       = 16_CAFEBABE;

TYPE
  Node = REF RECORD
    magic: INTEGER;
    val:   INTEGER;
    pad:   ARRAY [0..5] OF INTEGER;
  END;

  PressureClosure = Thread.Closure OBJECT
  OVERRIDES
    apply := PressureWorker;
  END;

  ReleaserClosure = Thread.Closure OBJECT
    flag: UNTRACED REF INTEGER;
  OVERRIDES
    apply := ReleaserWorker;
  END;

VAR
  pressureGo: INTEGER := 0;
  pressureStop: INTEGER := 0;

(* Allocate to trigger GC, forever *)
PROCEDURE PressureWorker(<*UNUSED*> cl: PressureClosure): REFANY =
  VAR junk: Node;
  BEGIN
    WHILE pressureStop = 0 DO
      FOR i := 0 TO 999 DO
        junk := NEW(Node, magic := i, val := i);
      END;
    END;
    EVAL junk;
    RETURN NIL;
  END PressureWorker;

(* Wait a bit then release the flag *)
PROCEDURE ReleaserWorker(cl: ReleaserClosure): REFANY =
  VAR junk: Node;
  BEGIN
    (* Do some allocation ourselves to help trigger GC *)
    FOR i := 0 TO PressureAllocs - 1 DO
      junk := NEW(Node, magic := i, val := i);
    END;
    EVAL junk;
    (* Release the tester *)
    cl.flag^ := 1;
    RETURN NIL;
  END ReleaserWorker;

BEGIN
  IO.Put("GC red zone stress test (M3 + C hybrid)\n");
  IO.Put(Fmt.Int(NumPressure) & " pressure threads, "
       & Fmt.Int(Rounds) & " rounds\n");

  (* Start pressure threads *)
  VAR
    pressure: ARRAY [0..NumPressure-1] OF Thread.T;
    total := 0;
    obj: Node;
    addr: ADDRESS;
    recovered: ADDRESS;
    go: INTEGER;
    releaser: Thread.T;
  BEGIN
    FOR i := 0 TO NumPressure - 1 DO
      pressure[i] := Thread.Fork(NEW(PressureClosure));
    END;

    FOR round := 0 TO Rounds - 1 DO
      (* Allocate a traced object *)
      obj := NEW(Node, magic := Magic, val := round);
      addr := LOOPHOLE(obj, ADDRESS);
      obj := NIL;

      (* Set up spin flag *)
      go := 0;

      (* Fork a releaser that allocates (triggering GC) then releases *)
      releaser := Thread.Fork(NEW(ReleaserClosure, flag := ADR(go)));

      (* Call into C: stores addr in red zone, spins until go != 0.
         While we're spinning, the pressure + releaser threads are
         allocating and triggering GC. *)
      recovered := RedZone.Hold(addr, ADR(go));

      EVAL Thread.Join(releaser);

      (* Check if the object survived *)
      IF recovered # NIL THEN
        WITH p = LOOPHOLE(recovered, UNTRACED REF RECORD magic: INTEGER END) DO
          IF p.magic # Magic THEN
            INC(total);
            IF total <= 10 THEN
              IO.Put("  Round " & Fmt.Int(round) & ": CORRUPTED "
                   & "(magic=0x" & Fmt.Unsigned(p.magic) & ")\n");
            END;
          END;
        END;
      END;
    END;

    pressureStop := 1;
    FOR i := 0 TO NumPressure - 1 DO
      EVAL Thread.Join(pressure[i]);
    END;

    IF total = 0 THEN
      IO.Put("PASS: no corruption detected in " & Fmt.Int(Rounds)
           & " rounds\n");
    ELSE
      IO.Put("FAIL: " & Fmt.Int(total) & " corruptions in "
           & Fmt.Int(Rounds) & " rounds\n");
    END;
  END;
END Main.
