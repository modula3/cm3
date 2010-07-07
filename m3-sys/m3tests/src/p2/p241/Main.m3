(* Compiling this program has crashed some backends.
 * (gcc backend with -O3, no volatile, 'ter')
 * This is reduced from libm3/Formatter.m3
 * The point therefore is merely to compile it without crashing.
 *)

MODULE Main;
 
VAR NoAlignOp: CARDINAL;

PROCEDURE PeekOp(<*NOWARN*> i: CARDINAL): CARDINAL =
  BEGIN RETURN NoAlignOp; END PeekOp;

VAR pos1: CARDINAL := 0;

<*NOWARN*> PROCEDURE PrintAlign(VAR pos: CARDINAL) =
  VAR op, endRun: CARDINAL := 0;
  BEGIN
    TRY
      LOOP
        op := PeekOp(pos);
        LOOP
          IF op = NoAlignOp THEN
            IF endRun = pos THEN
              INC(endRun);
            END;
            EXIT;
          END;
          op := PeekOp(pos1);
        END;
      END;
    FINALLY
    END;
  END PrintAlign;

BEGIN
END Main.
