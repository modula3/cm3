
MODULE CompiledStuff;


TYPE
  Result = RECORD
    result   : Scheme.Object;
    tailCall : TailCall;
  END;

  TailCall = RECORD
    f     : F;
    args  : ARRAY [0..MaxArgs-1] OF Scheme.Object;
  END;

  F = PROCEDURE (READONLY args     : ARRAY OF Scheme.Object;
                 VAR      defStack : ARRAY OF Scheme.Object;
                 READONLY freeVars : ARRAY OF Accessor) : Result
      RAISES { Scheme.E };

  Accessor = RECORD 
    var         : SchemeSymbol.T;
    accelerator : SchemeEnvironment.Accelerator; 
    (* something that speeds up env access? *)
  END;

(define (f n) 
  (if (= 0 n) 
      1 
      (f (- n 1))))

VAR Constants :=  ARRAY OF Scheme.Object { LR(0), LR(1) };

PROCEDURE CP(READONLY args     : ARRAY OF Scheme.Object;
             VAR      defStack : ARRAY OF Scheme.Object;
             READONLY freeVars : ARRAY OF Accessor) : Result
  RAISES { Scheme.E } =
  (* version 1, normal tail call *)
  BEGIN
    IF TruthO(PrimEquals(ARRAY OF Scheme.Object { Constants[0],
                                                  args[1] } THEN
      RETURN Result { result := Constants[1] }
    ELSE
      RETURN Result {
        tailCall := TailCall { f := CP,
                               args := ARRAY OF Scheme.Object { 
                                 PrimMinus(ARRAY OF Scheme.Object {
                                              args[1],
                                              Constants[1] }) },
                               freeVars := freeVars }
      }
    END
  END CP;

PROCEDURE CP(READONLY args     : ARRAY OF Scheme.Object;
             VAR      defStack : ARRAY OF Scheme.Object;
             READONLY freeVars : ARRAY OF Accessor) : Result
  RAISES { Scheme.E } =
  (* version 2, tail call to myself detected *)
  VAR
    args2 := args;
  BEGIN
    LOOP
      IF TruthO(PrimEquals(ARRAY OF Scheme.Object { Constants[0],
                                                    args2[1] } THEN
        RETURN Result { result := Constants[1] }
      ELSE
        args2 := ARRAY OF Scheme.Object { 
                PrimMinus(ARRAY OF Scheme.Object {  args2[1],
                                                    Constants[1] }) }
      END
    END
  END CP;

BEGIN END CompiledStuff.
