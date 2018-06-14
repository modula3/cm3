UNSAFE MODULE Coroutine;
IMPORT Debug;
IMPORT ContextC;
FROM Fmt IMPORT F, Int;
IMPORT WeakRef;
IMPORT RTCollector;

REVEAL
  T = BRANDED Brand OBJECT
    main    : BOOLEAN;
    context : ADDRESS;
  END;
  
PROCEDURE CallPair(p : Entry; pArg : REFANY;
                   q : Entry; qArg : REFANY) =
  VAR
    <*NOWARN*>stack : ARRAY [0..128*1024-1] OF CHAR;
    curC := ContextC.Current();
    qa := NEW(Arg, arg := qArg, co := Trace(NEW(T, context := curC, main := TRUE)));
    qaT := Trace(NEW(T, main := FALSE, context := ContextC.MakeContext(q, ADR(stack[0]), NUMBER(stack), curC,  qa)));
  BEGIN
    qa.this := qaT;
    
    p(NEW(Arg, arg := pArg, this := qa.co, co := qa.this))
  END CallPair;

PROCEDURE Call(to : T; from : T) =
  BEGIN
    <*ASSERT to#NIL*>
    <*ASSERT from#NIL*>
(*    IF from.main THEN RTCollector.Disable() END;*)
    ContextC.SwapContext(from.context, to.context);
(*    IF from.main THEN RTCollector.Enable() END*)
  END Call;

PROCEDURE Trace(t : T) : T =
  BEGIN
    EVAL WeakRef.FromRef(t, Cleanup);
    RETURN t
  END Trace;

PROCEDURE Cleanup(<*UNUSED*>READONLY self : WeakRef.T; ref : REFANY) =
  VAR
    dead : T := ref;
  BEGIN
    ContextC.DisposeContext(dead.context);
    dead.context := NIL
  END Cleanup;

BEGIN END Coroutine.
