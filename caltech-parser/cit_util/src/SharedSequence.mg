(* $Id$ *)

GENERIC MODULE SharedSequence(Elem, ElemSeq);

REVEAL
  T = Public BRANDED Brand OBJECT
    seqs : DequeR;
  OVERRIDES
    init := Init;
    addSeqHi := SSAddSeq;
    get := SSGet;
    size := SSSize;

    put := SSPut;
    addhi := SSAddHi;
    addlo := SSAddLo;
    remhi := SSRemHi;
    remlo := SSRemLo;
    gethi := SSGetHi;
    getlo := SSGetLo;
    fromArray := SSAbortFA;
  END;

PROCEDURE SSAbortFA(<*UNUSED*>t : T; 
                    <*UNUSED*> READONLY a : ARRAY OF Elem.T) : ElemSeq.T=
  BEGIN 
    <* ASSERT FALSE *>
  END SSAbortFA;

TYPE 
  DequeR = OBJECT
    t : ElemSeq.T;
    next, prev : DequeR;
  END;

PROCEDURE Init(t : T) : T =
  VAR 
    (* sentinel *)
    r := NEW(DequeR, t := NIL);
  BEGIN  
    r.next := r; r.prev := r;

    t.seqs := r;

    t.addSeqHi(NEW(ElemSeq.T).init()); (* so you can add things to it... *)

    RETURN t
  END Init;

PROCEDURE SSAddSeq(ss : T; s : ElemSeq.T) = 
  BEGIN 
    <* ASSERT s # NIL *>
    WITH r = NEW(DequeR, t := s, prev := ss.seqs.prev, next := ss.seqs) DO
      ss.seqs.prev.next := r;
      ss.seqs.prev := r;
    END
  END SSAddSeq;

PROCEDURE SSGet(ss : T; i : CARDINAL) : Elem.T =
  VAR
    p := ss.seqs.next;
  BEGIN
    LOOP
      WITH pp = p.t,
           s = pp.size() DO
        IF i < s THEN
          RETURN pp.get(i)
        ELSE
          DEC(i,s);
          p := p.next
        END
      END
    END
  END SSGet;

PROCEDURE SSPut(ss : T; i : CARDINAL; READONLY e : Elem.T) =
  VAR
    p := ss.seqs.next;
  BEGIN
    LOOP
      WITH pp = p.t,
           s = pp.size() DO
        IF i < s THEN
          pp.put(i,e);
          RETURN
        ELSE
          DEC(i,s);
          p := p.next
        END
      END
    END
  END SSPut;

PROCEDURE SSSize(ss : T) : CARDINAL =
  VAR
    p := ss.seqs.next;
    res : CARDINAL := 0;
  BEGIN
    WHILE p # ss.seqs DO
      WITH pp = p.t DO INC(res, pp.size()) END;
      p := p.next
    END;
    RETURN res
  END SSSize;

PROCEDURE SSAddHi(t : T; READONLY e : Elem.T) = 
  BEGIN t.seqs.prev.t.addhi(e) END SSAddHi;

PROCEDURE SSAddLo(t : T; READONLY e : Elem.T) = 
  BEGIN t.seqs.next.t.addlo(e) END SSAddLo;

PROCEDURE SSRemLo(t : T) : Elem.T = 
  VAR 
    p := t.seqs.next;
  BEGIN 
    WHILE p.t.size() = 0 DO p := p.next END;
    RETURN p.t.remlo() 
  END SSRemLo;

PROCEDURE SSRemHi(t : T) : Elem.T = 
  VAR 
    p := t.seqs.prev;
  BEGIN 
    WHILE p.t.size() = 0 DO p := p.prev END;
    RETURN p.t.remhi() 
  END SSRemHi;

PROCEDURE SSGetLo(t : T) : Elem.T = 
  VAR 
    p := t.seqs.next;
  BEGIN 
    WHILE p.t.size() = 0 DO p := p.next END;
    RETURN p.t.getlo() 
  END SSGetLo;

PROCEDURE SSGetHi(t : T) : Elem.T = 
  VAR 
    p := t.seqs.prev;
  BEGIN 
    WHILE p.t.size() = 0 DO p := p.prev END;
    RETURN p.t.gethi() 
  END SSGetHi;

BEGIN END SharedSequence.
