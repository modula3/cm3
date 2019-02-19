(* $Id$ *)

MODULE SX EXPORTS SX, SXSelect, SXDebug;
IMPORT SXClass;
IMPORT Thread, ThreadF;
IMPORT XTime AS Time;
IMPORT IntSet, IntSetDef;
IMPORT IntRefTbl;
IMPORT Debug;
IMPORT Fmt;
IMPORT RefList;
IMPORT SXRef;
IMPORT Integer, RefanyArraySort;
IMPORT Word;
IMPORT SXRefTbl;

(* methods for garbage collecting...?  Would imply using WeakRef. *)

REVEAL
  T = SXClass.Private BRANDED Brand OBJECT
    id        : CARDINAL;
    selecters : IntSet.T; (* set of thread IDs *)
    c         : Thread.Condition;
    when      : Time.T;
    dependers : RefList.T; (* of type T *)
    dMu       : MUTEX;
  OVERRIDES
    depends   := Depends;
    propagate := Propagate;
    touch     := Touch;
    init      := InitT;
    wait      := WaitT;
  END;


PROCEDURE Equal(a, b : T) : BOOLEAN = BEGIN RETURN a = b END Equal;

PROCEDURE Hash(a : T) : Word.T = BEGIN RETURN a.id END Hash;

PROCEDURE Depends(t : T; depender : T) =
  BEGIN
    LOCK t.dMu DO
      t.dependers := RefList.Cons(depender,t.dependers)
    END
  END Depends;

PROCEDURE WaitT(t : T) =
  BEGIN Thread.Wait(t.mu,t.c) END WaitT;

VAR idMu := NEW(MUTEX);
VAR nextId : CARDINAL := 1;

PROCEDURE InitT(t : T) : T =
  BEGIN 
    LOCK idMu DO
      t.id := nextId; INC(nextId) 
    END;

    Debug.Out("SX.InitT, id = " & Fmt.Int(t.id));

    t.mu := NEW(MUTEX); t.c := NEW(Thread.Condition);
    t.dMu := NEW(MUTEX);
    t.dependers := NIL;
    t.selecters := NEW(IntSetDef.T).init();
    RETURN t 
  END InitT;

PROCEDURE Propagate(t : T; when : Time.T; locked : BOOLEAN) = 
  
  PROCEDURE DoIt() = 
    BEGIN
      IF t.selecters.size() > 0 THEN
        LOCK giant DO
          VAR
            u := t.selecters.copy();
            s := u.iterate();
            i : INTEGER;
            c : REFANY;
            x : BOOLEAN;
          BEGIN
            WHILE s.next(i) DO
              x := threadCondTbl.get(i,c);
              <* ASSERT x *>
              WITH tl = NARROW(c,ThreadLock) DO 
                (* could be Thread.Signal...? *)
                (* if we get here, we should be able to assert that
                   thread tl is eventually going to be in Thread.Wait. *)
                <*ASSERT tl.okToLock*>
                
                (* at this point, this thread is committed to waking
                   up thread i, therefore unmark all ts marked as
                   selected on by i *)
                
                FOR tp := FIRST(tl.onc^) TO LAST(tl.onc^) DO
                  WITH t = tl.onc[tp] DO
                    EVAL t.selecters.delete(i)
                  END
                END;
                tl.onc := NIL;
                
                (* it better be the case that i is either waiting 
                   or will soon be waiting *)
                <*ASSERT tl.okToLock*>
                LOCK tl.tMu DO Thread.Broadcast(tl.c) END;
                
                
                (* need to delete i from all other things it's 
                   selecting too... *)
                
              END
            END
          END
        END
      END
    END DoIt;

  VAR
    d : RefList.T;
  BEGIN

    VAR cb : REFANY; BEGIN
      IF tracing AND tab.get(t,cb) THEN
        NARROW(cb,Callback).changed(t)
      END
    END;

    Thread.Broadcast(t.c);

    LOCK t.dMu DO
      d := t.dependers;
      WHILE d # NIL DO
        NARROW(d.head,T).touch(when,locked);
        d := d.tail
      END
    END;

    IF locked THEN
      AssertLocked(Array { t });
      DoIt()
    ELSE
      LOCK t.mu DO DoIt() END
    END
  END Propagate;

VAR tracing := FALSE;
    
PROCEDURE Trace(on : BOOLEAN) = BEGIN tracing := on END Trace;

VAR tab := NEW(SXRefTbl.Default).init();
    tabMu := NEW(MUTEX);

PROCEDURE Register(sx : T; callback : Callback) =
  BEGIN
    LOCK tabMu DO
      EVAL tab.put(sx,callback)
    END
  END Register;

VAR giant := NEW(MUTEX);

PROCEDURE Touch(t : T; when : Time.T; locked : BOOLEAN) =
  BEGIN
    (* propagate called after recalc, so values will be
       consistent from dependencies to dependers *)
    IF t.recalc(when) THEN t.propagate(when,locked) END
  END Touch;

PROCEDURE Wait(READONLY on : ARRAY OF T; touched : REF ARRAY OF BOOLEAN) = 
  <* FATAL Exception *>
  BEGIN WaitE(on, NIL, touched) END Wait;

PROCEDURE Wait1(on : T) = BEGIN Wait(ARRAY OF T { on }, NIL) END Wait1;

PROCEDURE WaitE(READONLY on : ARRAY OF T; 
                except      : SXRef.T;
                touched     : REF ARRAY OF BOOLEAN) 
  RAISES { Exception } =

  PROCEDURE CheckExcept() RAISES { Exception } = 
    BEGIN
      IF except # NIL THEN
        TRY
          WITH val = except.value() DO
            IF val # NIL THEN RAISE Exception(val) END
          END
        EXCEPT
          Uninitialized => (* skip *)
        END
      END
    END CheckExcept;

  VAR
    r       : REFANY;
    updates : REF ARRAY OF CARDINAL;
    myId := ThreadF.MyId();
    nExcept := ARRAY BOOLEAN OF CARDINAL { 0, 1 } [ except # NIL ];
  BEGIN
    (* set up my waiting condition *)
    LOCK gMu DO
      IF NOT threadCondTbl.get(myId,r) THEN
        r := NEW(ThreadLock, tMu := NEW(MUTEX), c := NEW(Thread.Condition));
        EVAL threadCondTbl.put(myId,r)
      END
    END;

    CheckExcept();

    (*
    Debug.Out("Adding thread " & Fmt.Int(myId) & 
      " to " & Fmt.Int(NUMBER(on)+nExcept) & " selecter lists");
    *)

    WITH l = NARROW(r,ThreadLock) DO

      (* here we need to atomically unlock all the variables we're
         waiting on -- a kind of multi-wait *)
      LOCK giant DO
      LOCK l.tMu DO 
        FOR i := FIRST(on) TO LAST(on) DO
          WITH t = on[i] DO
            EVAL t.selecters.insert(myId)
          END
        END;
        IF except # NIL THEN EVAL except.selecters.insert(myId) END;

        (* record # of updates, if applicable *)
        IF touched # NIL THEN
          updates := NEW(REF ARRAY OF CARDINAL, NUMBER(touched^));
          FOR i := 0 TO MIN(NUMBER(updates^),NUMBER(on))-1 DO
            updates[i] := on[i].numUpdates()
          END
        END;

        (* unlock variables *)
        (*Unlock(on);*)
        AssertLocked(on); 
        IF except # NIL THEN AssertLocked(ARRAY OF T { except }) END;

        (* 
           LOCKING ORDER:
           t.mu < giant < l.tMu 

           global mu not used in this module
           table locks are > l.tMu

           giant SHOULD NOT BE NECESSARY IN BELOW CODE.. why is it?

         *)
        l.okToLock := TRUE;

        WITH onc = NEW(REF ARRAY OF T, NUMBER(on)+nExcept) DO
          SUBARRAY(onc^,0,NUMBER(on)) := on;
          IF nExcept = 1 THEN onc[LAST(onc^)] := except END;
          l.onc := onc
        END;
          
        Thread.Release(giant);

        WITH locks = UnlockAll() DO
          Thread.Wait(l.tMu,l.c); l.okToLock := FALSE; 

          (* here we should be able to assert that we're on any 
             selector list *)
          FOR i := FIRST(on) TO LAST(on) DO
            <*ASSERT NOT on[i].selecters.member(myId)*>
          END;
          IF except#NIL THEN <*ASSERT NOT except.selecters.member(myId)*> END;

          Lock(locks^)
        END;
        Thread.Acquire(giant);

        IF touched # NIL THEN
          FOR i := FIRST(touched^) TO LAST(touched^) DO
            touched[i] := (i <= LAST(updates^)) AND 
                          on[i].numUpdates() # updates[i]
          END
        END;
                                                             
      END (* LOCK l.tMu *)
      END (* LOCK giant *)
    END;

    CheckExcept()
  END WaitE;

TYPE 
  ThreadLock = OBJECT
    c           : Thread.Condition;
    tMu         : MUTEX;
    okToLock := FALSE; (* TRUE if OK for foreign thread to lock,
                          protected by giant *)
    onc         : REF ARRAY OF T := NIL;
  END;

VAR gMu := NEW(MUTEX);
VAR threadCondTbl := NEW(IntRefTbl.Default).init(); 
    (* holds cond vars for each thread *)

PROCEDURE IdCompare(a, b : REFANY) : [-1..1] = 
  BEGIN
    (* we can get nil pointers from the Unlock code *)
    IF    a = NIL THEN RETURN 1 
    ELSIF b = NIL THEN RETURN -1 
    ELSE
      WITH an = NARROW(a, T), bn = NARROW(b, T) DO
        RETURN Integer.Compare(an.id,bn.id) 
      END
    END
  END IdCompare;

PROCEDURE Lock(READONLY arr : Array) =
  VAR
    a := NEW(REF ARRAY OF REFANY, NUMBER(arr));
    bb : REFANY;
    myId := ThreadF.MyId();
  BEGIN
    FOR i := FIRST(arr) TO LAST(arr) DO 
      a[i] := arr[i]
    END;
    RefanyArraySort.Sort(a^,IdCompare);

    FOR i := FIRST(a^) TO LAST(a^) DO
      (* allow specifying same var multiple times *)
      IF i = FIRST(a^) OR a[i] # a[i-1] THEN
        Thread.Acquire(NARROW(a[i],T).mu)
      END
    END;

    LOCK lockMu DO
      IF lockTab.get(myId, bb) THEN
        (* nested locks, scary but OK if programmer knows what hes doing... *)
        (* extend locked array with new locks *)
        WITH b = NARROW(bb,REF ARRAY OF REFANY),
             c = NEW(REF ARRAY OF REFANY, NUMBER(a^) + NUMBER(b^)) DO
          SUBARRAY(c^,0,NUMBER(a^)) := a^;
          SUBARRAY(c^,NUMBER(a^),NUMBER(b^)) := b^;
          a := c;
          RefanyArraySort.Sort(a^,IdCompare)
        END
      END;
      EVAL lockTab.put(myId, a)
    END
  END Lock;

VAR lockMu  := NEW(MUTEX);
    lockTab := NEW(IntRefTbl.Default).init();

PROCEDURE Lock1(t : T) = BEGIN Lock(Array { t }) END Lock1;

PROCEDURE Unlock1(t : T) = BEGIN Unlock(Array { t }) END Unlock1;

PROCEDURE Unlock(READONLY arr : Array) =
  VAR
    a := NEW(REF ARRAY OF REFANY, NUMBER(arr));
    br : REFANY;
    b : REF ARRAY OF REFANY;
    bi, nz := 0;
    myId := ThreadF.MyId();
  BEGIN
    FOR i := FIRST(arr) TO LAST(arr) DO 
      a[i] := arr[i]
    END;
    RefanyArraySort.Sort(a^,IdCompare);

    LOCK lockMu DO
      WITH hadLocks = lockTab.get(myId, br) DO
        <* ASSERT hadLocks *>
        b := br;
      END
    END;

    FOR i := FIRST(a^) TO LAST(a^) DO
      (* allow specifying same var multiple times *)
      IF i = FIRST(a^) OR a[i] # a[i-1] THEN
        Thread.Release(NARROW(a[i],T).mu);
        WHILE b[bi] # a[i] DO 
          IF b[bi] # NIL THEN INC(nz) END; (* count non-nil locks *)
          INC(bi); 
        END; (* skip irrelevants *)
        WHILE bi <= LAST(b^) AND b[bi] = a[i] DO
          b[bi] := NIL; (* zero matches *)
          INC(bi)
        END
      END
    END;

    IF nz = 0 THEN 
      (* we have completely unlocked our locks *)
      LOCK lockMu DO
        EVAL lockTab.delete(myId, br)
      END
    END
  END Unlock;

PROCEDURE AssertLocked(READONLY a : Array) =
  VAR
    myId := ThreadF.MyId();
    br : REFANY;
    found : BOOLEAN;
  BEGIN
    LOCK lockMu DO
      WITH hadLocks = lockTab.get(myId, br) DO
        <*ASSERT hadLocks*>
      END
    END;
    WITH b = NARROW(br, REF ARRAY OF REFANY)^ DO
      FOR ai := FIRST(a) TO LAST(a) DO
        found := FALSE;
        FOR bi := FIRST(b) TO LAST(b) DO
          IF ai = bi THEN found := TRUE; EXIT END
        END;
        <* ASSERT found *>
      END
    END
  END AssertLocked;

PROCEDURE UnlockAll() : REF Array =
  (* unlock all locks held by current thread and return array of the locks *)
  VAR
    myId := ThreadF.MyId();
    br : REFANY;
    j := 0;
  BEGIN
    LOCK lockMu DO
      WITH hadLocks = lockTab.get(myId, br) DO
        <*ASSERT hadLocks*>
      END
    END;

    WITH b = NARROW(br, REF ARRAY OF REFANY)^,
         c = NEW(REF Array, NUMBER(b)) DO
      FOR i := FIRST(b) TO LAST(b) DO
        IF (i = FIRST(b) OR b[i] # b[i-1]) AND b[i] # NIL THEN
          c[j] := b[i]; INC(j)
        END
      END;
      
      WITH d = NEW(REF Array, j) DO
        d^ := SUBARRAY(c^, 0, j);
        Unlock(d^);
        RETURN d
      END
    END
  END UnlockAll;


BEGIN 
  mu := NEW(MUTEX);
END SX.

