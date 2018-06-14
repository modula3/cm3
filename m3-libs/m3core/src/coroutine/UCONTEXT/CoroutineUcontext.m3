UNSAFE MODULE CoroutineUcontext EXPORTS Coroutine, CoroutineUcontext;
IMPORT ContextC;
IMPORT WeakRef;
IMPORT Thread;

(*  (* cant import from libm3 into m3core *)
FROM Fmt IMPORT F, Int;
IMPORT Debug;
*)

REVEAL
  T = BRANDED OBJECT
    context : ContextC.T; (* ucontext_t *)
    thread  : Thread.T;
    isAlive := TRUE;
    id      : UNTRACED REF INTEGER; (* Tabulate() fills this in *)
    initial :=  TRUE; (* initial caller *)
    arg     : Arg;
  END;

VAR coArr := NEW(REF ARRAY OF T, 1); (* entry 0 not used *)
    coMu  := NEW(MUTEX); (* protect coArr and coId *)

PROCEDURE CreateInitialCoroutine() : T =
  VAR
    t := NEW(T,
             initial := FALSE,              (* since we're not a Closure *)
             arg := NIL,                    (* since initial is FALSE *)
             context := ContextC.Current(),
             thread  := Thread.Self());
  BEGIN
    Tabulate(t);
    RETURN t
  END CreateInitialCoroutine;
  
PROCEDURE Create(cl : Closure) : T =
  VAR
    me, t : T;
  BEGIN
    (* make sure that the current coroutine is defined, it will not be
       if this is the first/second coroutine created of this thread *)
    WITH cur = ContextC.GetCurrentCoroutine() DO
      IF LOOPHOLE(cur,INTEGER) = 0 OR cur^ = 0 THEN
        
        me := CreateInitialCoroutine();

        (* the next line seems to cause a segfault in 
           RTAllocator__GetTracedRef ?? *)
        ContextC.SetCurrentCoroutine(me.id)

      ELSE
        LOCK coMu DO
          me := coArr[cur^]
        END
      END
    END;

    (* need to get the context here *)
    <*ASSERT cl # NIL*>
    WITH arg   = NEW(Arg, arg := cl, dbg := 'B'),
         ssz   = 16384,
         ctx   = ContextC.MakeContext(Run, ssz, NIL, arg) DO
      ContextC.DbgPtr(LOOPHOLE(arg,ADDRESS));
      t := NEW(T, arg := arg, context := ctx, thread := Thread.Self());
    END;
    Tabulate(t);
    RETURN t
  END Create;

PROCEDURE Run(arg : Arg) =
  BEGIN
    ContextC.DbgPtr(LOOPHOLE(arg,ADDRESS));
    ContextC.Dbg(ORD(arg.dbg));
    <*ASSERT arg # NIL*>
    <*ASSERT arg.arg # NIL*>
    WITH cl = NARROW(arg.arg, Closure) DO
      EVAL cl.apply(arg.this) (* return value? *)
    END
  END Run;

PROCEDURE Tabulate(t : T) =
  BEGIN
    LOCK coMu DO
      FOR i := 1 TO LAST(coArr^) DO
        IF coArr[i] = NIL THEN
          t.id := NEW(UNTRACED REF INTEGER);
          t.id^ := i;
          coArr[t.id^] := t;
          RETURN
        END
      END;
      WITH new = NEW(REF ARRAY OF T, NUMBER(coArr^)+1) DO
        SUBARRAY(new^,0,NUMBER(coArr^)) := coArr^;
        coArr := new;
        t.id := NEW(UNTRACED REF INTEGER);
        t.id^ := LAST(coArr^);
        coArr[t.id^] := t
      END
    END
  END Tabulate;

PROCEDURE Call(to : T) : T =
  VAR
    myId := ContextC.GetCurrentCoroutine();
    me : T;
  BEGIN
    LOCK coMu DO
      me := coArr[myId^]
    END;
    <*ASSERT me # NIL*>
    <*ASSERT to # NIL*>
    <*ASSERT to.isAlive*>
    <*ASSERT me.thread = to.thread*>

    IF to.initial THEN
      (* check for first call, set arg.this *)
      to.initial := FALSE;
      to.arg.this := me;
    END;
    
    ContextC.SetCurrentCoroutine(to.id);
    ContextC.SwapContext(me.context, to.context);
    RETURN me
  END Call;

PROCEDURE IsAlive(t : T) : BOOLEAN =
  BEGIN RETURN t.isAlive END IsAlive;


  (**********************************************************************)

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

BEGIN
  ContextC.InitC()
END CoroutineUcontext.
