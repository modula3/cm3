UNSAFE MODULE Evt;

IMPORT EvtRep, IntRefTbl, RefRefTbl, IntPQ, IO;
IMPORT RTType, RTHeap;
IMPORT FRefRefTbl;

TYPE
  ToChars = UNTRACED REF ARRAY [0 .. 1000] OF CHAR;
  InterestList = OBJECT
                   mu     : MUTEX;
                   evtList: FRefRefTbl.T;
                 END;
  Elt = IntPQ.Elt BRANDED OBJECT evt: T;  END;

VAR
  tbl  : IntRefTbl.T := NEW(IntRefTbl.Default).init(30);
  tblMu              := NEW(MUTEX);
  pq   : IntPQ.T;

VAR DEBUG := FALSE;

PROCEDURE PrintKey (msg: TEXT; etc: INTEGER; evt: T) =
  BEGIN
    IO.Put(msg); IO.PutInt(etc); IO.Put(" count: ");
    IO.PutInt(EventCount(evt)); IO.Put("\n");
  END PrintKey;

PROCEDURE CopyData (from, to: REFANY) =
  VAR
    src, dest: ADDRESS;
    size     : CARDINAL;
  BEGIN
    (* this does simple copy *)
    src := RTHeap.GetDataAdr(from);
    dest := RTHeap.GetDataAdr(to);
    size :=
      RTHeap.GetDataSize(from)   (*- 2 * BYTESIZE(ADDRESS) *)(*test leave
                                                                private *);
    IF DEBUG THEN
      IO.Put("data size "); IO.PutInt(size); IO.Put("\n");
    END;

    SUBARRAY(LOOPHOLE(dest, ToChars)^, 0, size) :=
      SUBARRAY(LOOPHOLE(src, ToChars)^, 0, size);

  END CopyData;

PROCEDURE EI (etc: INTEGER; evt: T) =
  VAR
    interestList: InterestList;
    any         : REFANY;
  BEGIN
    IF tbl.get(etc, any) THEN
      interestList := NARROW(any, InterestList);
    ELSE
      interestList := NEW(InterestList);
      interestList.mu := NEW(MUTEX);
      interestList.evtList := NEW(FRefRefTbl.Default).init(30);
      LOCK tblMu DO EVAL tbl.put(etc, interestList); END;
    END;
    LOCK interestList.mu DO
      (* expressing interest more than once is allowed *)
      EVAL interestList.evtList.put(evt, evt);
    END;
  END EI;

PROCEDURE ExpressInterest (evt: T; includeSupertypes: BOOLEAN := FALSE) =
  VAR etc, btc: INTEGER;
  BEGIN
    etc := TYPECODE(evt);
    IF DEBUG THEN PrintKey("Express Interest key: ", etc, evt); END;

    EI(etc, evt);

    IF includeSupertypes THEN
      btc := TYPECODE(T);
      WHILE btc # etc DO etc := RTType.Supertype(etc); EI(etc, evt); END;
    END;
  END ExpressInterest;

PROCEDURE RI (etc: INTEGER; evt: T): BOOLEAN =
  VAR
    interestList: InterestList;
    any         : REFANY;
  BEGIN
    IF tbl.get(etc, any) THEN
      IF DEBUG THEN
        IO.Put("RI Found key "); IO.PutInt(etc); IO.Put("\n");
      END;

      interestList := NARROW(any, InterestList);
      LOCK interestList.mu DO
        IF interestList.evtList.delete(evt, any) THEN
          RETURN TRUE;
        ELSE
          (* cannot find object to delete *)
          RETURN FALSE;
        END;
      END;
    ELSE
      (* ignore any events not expressed so cannot be revoked *)
      IF DEBUG THEN
        IO.Put("RI cant find key "); IO.PutInt(etc); IO.Put("\n");
      END;
      RETURN FALSE;
    END;
  END RI;

PROCEDURE RevokeInterest (evt: T) =
  VAR etc, btc: INTEGER;
  BEGIN
    etc := TYPECODE(evt);
    IF DEBUG THEN PrintKey("Revoke Interest key: ", etc, evt); END;

    EVAL RI(etc, evt);

    (* check if any supertypes expressed *)
    btc := TYPECODE(T);
    WHILE btc # etc DO etc := RTType.Supertype(etc); EVAL RI(etc, evt); END
  END RevokeInterest;

(* send event, return number of callbacks called *)
PROCEDURE Send (v: T): INTEGER =
  VAR
    evtList        : RefRefTbl.T;
    interestList   : InterestList;
    iter           : RefRefTbl.Iterator;
    obj            : T;
    any, e, private: REFANY;
    key, ind       : INTEGER;
  BEGIN
    key := TYPECODE(v);

    IF DEBUG THEN
      IO.Put("Evt.Send: typecode"); IO.PutInt(key); IO.Put("\n");
    END;

    ind := 0;
    IF tbl.get(key, any) THEN
      interestList := NARROW(any, InterestList);
      evtList := interestList.evtList;

      IF evtList.size() = 0 THEN
        (* not an error. *)
        IF DEBUG THEN
          IO.Put("No objects registered for event. Send failed\n");
        END;
        RETURN 0;
      END;

      (* loop through all objects *)
      iter := evtList.iterate();
      WHILE iter.next(e, e) DO
        INC(ind);
        IF DEBUG THEN
          IO.Put("callback "); IO.PutInt(ind); IO.Put("\n");
        END;

        obj := NARROW(e, T);
        private := obj.private;

        (* this clobbers obj's callback since v has no callback. could save
           callback in a temp then reinstate it after ie obj := DeepCopy.Copy(v); *)
        CopyData(v, obj);
        obj.private := private;

        obj.callback();
      END;
    ELSE
      (* this is same as no objects registered except here no object ever
         expressed interest so the list was never created.  Its not an
         error *)
      IF DEBUG THEN IO.Put("Evt: Cant find seq. Send failed.\n"); END;
    END;
    RETURN ind;
  END Send;

PROCEDURE QueueEvent (v: T) =
  VAR
    elt : Elt;
    next: INTEGER;
  (* next should be global in an object see the btree thing maybe the
     thread object contains both the pq and the next like cache in btree *)

  BEGIN
    (* put v on pri queue and signal non empty the thread will eventually
       take it off and send it - make a copy first and then delete off
       queue.  Use a sequence not a queue if we had priority threads then
       could use those *)
    pq := NEW(IntPQ.Default).init();
    elt := NEW(Elt, priority := next, evt := v);
    INC(next);
    pq.insert(elt);

  END QueueEvent;

PROCEDURE EventCount (v: T): INTEGER =
  VAR
    evtList     : RefRefTbl.T;
    interestList: InterestList;
    any         : REFANY;
    key         : INTEGER;
  BEGIN
    key := TYPECODE(v);
    IF tbl.get(key, any) THEN
      interestList := NARROW(any, InterestList);
      evtList := interestList.evtList;
      RETURN evtList.size();
    ELSE
      RETURN 0;
    END;
  END EventCount;

PROCEDURE DumpEvents () =
  VAR
    interestList: InterestList;
    evtList     : FRefRefTbl.T;
    iter        : RefRefTbl.Iterator;
    tblIter     : IntRefTbl.Iterator;
    e           : REFANY;
    ind, k      : INTEGER;
  BEGIN
    tblIter := tbl.iterate();
    WHILE tblIter.next(k, e) DO
      IO.Put("tbl entry "); IO.PutInt(k); IO.Put("\n");

      interestList := NARROW(e, InterestList);
      evtList := interestList.evtList;
      iter := evtList.iterate();

      (* loop through all objects *)
      ind := 0;
      WHILE iter.next(e, e) DO
        IO.Put("obj "); IO.PutInt(ind); IO.Put(" "); 
        IO.PutInt(LOOPHOLE(e, INTEGER)); IO.Put("\n");
        INC(ind);
      END;
    END;
  END DumpEvents;

BEGIN
END Evt.

