(* Copyright 1993 Digital Equipment Corporation.             *)
(* Distributed only by permission.                           *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* Last modified on Thu Sep 22 19:45:14 PDT 1994 by heydon   *)
(*      modified on Thu Sep  8 15:15:04 PDT 1994 by kalsow   *)
(*      modified on Tue Aug  9 15:58:24 PDT 1994 by detlefs  *)
(*      modified on Mon Oct 25 11:00:36 PDT 1993 by mcjones  *)
(*      modified on Mon Jun 28 13:20:59 PDT 1993 by gnelson  *)
<*PRAGMA SPEC, LOOPINV*>

GENERIC MODULE Sequence(Elem, Seq, Rep);
(* Where "Seq = Sequence(Elem)" and "Rep = SequenceRep(Elem, Seq)". *)

REVEAL Seq.T = Rep.Public BRANDED Seq.Brand OBJECT
  OVERRIDES
    init := Init;
    fromArray := FromArray;
    addhi := Addhi;
    addlo := Addlo;
    remhi := Remhi;
    remlo := Remlo;
    put := Put;
    size := Size;
    gethi := Gethi;
    getlo := Getlo;
    get := Get
  END;

VAR zero: Elem.T;
(* Modula-3 requires "zero" to be initialized to a value of type "Elem.T".
   With high probability the references within that value will be NIL.
   Hence, setting elements of the sequence to "zero" should avoid
   storage leaks. *)

<*SPEC INVARIANT (ALL [t1: Seq.T, t2: Seq.T]
                    t1 # t2 IMPLIES t1.elem # t2.elem) *>

PROCEDURE Init(s: Seq.T; sizeHint: CARDINAL): Seq.T = 
  BEGIN
    IF s.elem = NIL OR NUMBER(s.elem^) = 0 THEN
      s.elem := NEW(Rep.RefArray, MAX(sizeHint, 1))
    END (* IF *);
    s.sz := 0; s.st := 0;
    RETURN s
  END Init;

PROCEDURE FromArray(s: Seq.T; READONLY a: ARRAY OF Elem.T): Seq.T = 
  <*SPEC LET numA := NUMBER(a)*>
  <*SPEC LET raPre := Rep.RefArray *>
  BEGIN
    s.sz := NUMBER(a);
    s.st := 0;
    s.elem := NEW(Rep.RefArray, MAX(s.sz, 1));
    FOR i := 0 TO s.sz-1 DO
      <*LOOPINV (ALL [j: INTEGER]
                   (0 <= j AND j < i) IMPLIES s.elem^[j] = a[j])
                AND NUMBER(s.elem^) = numA
                AND (ALL [r: Rep.RefArray]
                        Rep.RefArray[r] = raPre[r] OR r = s.elem) *>
      s.elem[i] := a[i]
    END;
    RETURN s
  END FromArray;


PROCEDURE Addhi(s: Seq.T; READONLY x: Elem.T) =
  BEGIN
    IF s.sz = NUMBER(s.elem^) THEN Expand(s) END;
    VAR i := s.st + s.sz; BEGIN
      IF i >= NUMBER(s.elem^) THEN i := i - NUMBER(s.elem^) END;
      s.elem[i] := x
    END;
    INC(s.sz)
  END Addhi;

PROCEDURE Addlo(s: Seq.T; READONLY x: Elem.T) =
  BEGIN
    IF s.sz = NUMBER(s.elem^) THEN Expand(s) END;
    VAR i := s.st; BEGIN
      IF i = 0 THEN i := LAST(s.elem^) ELSE i := i - 1 END;
      s.elem[i] := x;
      s.st := i
    END;
    INC(s.sz)
  END Addlo;

PROCEDURE Expand(s: Seq.T) =
  VAR 
    n := NUMBER(s.elem^);
    new := NEW(Rep.RefArray, 2 * n);
    m := n - s.st;
  BEGIN
    SUBARRAY(new^, 0, m) := SUBARRAY(s.elem^, s.st, m);
    SUBARRAY(new^, m, s.st) :=
      SUBARRAY(s.elem^, 0, s.st);
    s.st := 0;
    s.elem := new
   END Expand; 

PROCEDURE Remhi(s: Seq.T): Elem.T =
  VAR
    j := s.st + s.sz - 1;
    res: Elem.T;
  BEGIN
    IF j >= NUMBER(s.elem^) THEN j := j - NUMBER(s.elem^) END;
    DEC(s.sz);
    WITH z = s.elem[j] DO  res := z;  z := zero;  END;
    RETURN res;
  END Remhi;

PROCEDURE Remlo(s: Seq.T): Elem.T =
  VAR res: Elem.T;
  BEGIN
    WITH z = s.elem[s.st] DO  res := z;  z := zero;  END;
    DEC(s.sz);
    INC(s.st);
    IF s.st = NUMBER(s.elem^) THEN s.st := 0 END;
    RETURN res
  END Remlo;

PROCEDURE Put(s: Seq.T; i: CARDINAL; READONLY x: Elem.T) =
  VAR j := s.st + i; BEGIN
    <* ASSERT i < s.sz *>
    IF j >= NUMBER(s.elem^) THEN j := j - NUMBER(s.elem^) END;
    s.elem[j] := x
  END Put;

PROCEDURE Get(s: Seq.T; i: CARDINAL): Elem.T =
  VAR j := s.st + i; BEGIN
    <* ASSERT i < s.sz *>
    IF j >= NUMBER(s.elem^) THEN j := j - NUMBER(s.elem^) END;
    RETURN s.elem[j]
  END Get;

PROCEDURE Size(s: Seq.T): CARDINAL =
  BEGIN
    RETURN s.sz
  END Size;

PROCEDURE Gethi(s: Seq.T): Elem.T =
  VAR j := s.st + s.sz - 1; BEGIN
    <* ASSERT s.sz > 0 *>
    IF j >= NUMBER(s.elem^) THEN
      j := j - NUMBER(s.elem^)
    END;
    RETURN s.elem[j]
  END Gethi;

PROCEDURE Getlo(s: Seq.T): Elem.T =
  BEGIN 
    <* ASSERT s.sz > 0 *>
    RETURN s.elem[s.st]
  END Getlo;

PROCEDURE Cat(s, t: T): T =
  VAR u := NEW(Seq.T);
      <*SPEC LET raPre := Rep.RefArray *>
      <*SPEC LET dPreS := Data[s] *>
      <*SPEC LET dPreT := Data[t] *>
  BEGIN
    u.sz := s.sz + t.sz;
    u.elem := NEW(Rep.RefArray, MAX(u.sz, 1));
    FOR i := 0 TO s.sz-1 DO
      <*LOOPINV NUMBER(u.elem^) = MAX(u.sz, 1)
            AND (ALL [x: Rep.RefArray] 
                     Rep.RefArray[x] = raPre[x] OR x = u.elem)
            AND (ALL [j: INTEGER] (0 <= j AND j < i) IMPLIES
                   u.elem^[j] = Rep.Abs(raPre[s.elem], s.st, s.sz)[j]) *>
      u.elem[i] := s.get(i)
    END;
    FOR i := 0 TO t.sz-1 DO
      <*LOOPINV NUMBER(u.elem^) = MAX(u.sz, 1)
            AND (ALL [x: Rep.RefArray] 
                     Rep.RefArray[x] = raPre[x] OR x = u.elem)
            AND (ALL [j: INTEGER] (0 <= j AND j < s.sz) IMPLIES
                   u.elem^[j] = dPreS[j])
                AND
                (ALL [j: INTEGER] (0 <= j AND j < i) IMPLIES
                   u.elem^[j + s.sz] = dPreT[j]) *>
      u.elem[i + s.sz] := t.get(i)
    END;
    RETURN u
  END Cat;

PROCEDURE Sub(s: T; start: CARDINAL;
    length: CARDINAL := LAST(CARDINAL)): T =
  VAR u := NEW(Seq.T);
      <*SPEC LET raPre := Rep.RefArray *>
      <*SPEC LET dPreS := Data[s] *>
  BEGIN
    IF start >= s.sz OR length = 0 THEN
      u.sz := 0
    ELSE
      u.sz := MIN(length, s.sz - start)
    END;
    u.elem := NEW(Rep.RefArray, MAX(u.sz, 1));
    FOR i := 0 TO u.sz-1 DO
      <*LOOPINV NUMBER(u.elem^) = MAX(u.sz, 1)
            AND (ALL [x: Rep.RefArray] 
                     Rep.RefArray[x] = raPre[x] OR x = u.elem)
            AND (ALL [j: INTEGER] (0 <= j AND j < i) IMPLIES
                   u.elem^[j] = dPreS[start + j]) *>
      u.elem[i] := s.get(start + i)
    END;
    RETURN u
  END Sub;

(* ESC specs... *)
<*SPEC Init(s, sizeHint)
       MODIFIES Valid[s], Data[s]
       REQUIRES s # NIL
       ENSURES RES = s AND Valid'[s] AND NUMBER(Data'[s]) = 0
*>

<*SPEC FromArray(s, a)
       MODIFIES Valid[s], Data[s]
       REQUIRES s # NIL
       ENSURES RES = s AND Valid'[s] AND NUMBER(Data'[s]) = NUMBER(a)
           AND (ALL [i: INTEGER] (0 <= i AND i < NUMBER(a)) IMPLIES
                                 Data'[s][i] = a[i])
*>

<*SPEC Addhi(s, x)
       MODIFIES Valid[s], Data[s]
       REQUIRES Valid[s] AND s # NIL
       ENSURES Valid'[s]
           AND NUMBER(Data'[s]) = NUMBER(Data[s])+1
           AND (ALL [i: INTEGER] 0 <= i AND i < NUMBER(Data[s]) IMPLIES
                  Data'[s][i] = Data[s][i])
           AND Data'[s][NUMBER(Data[s])] = x
*>

<*SPEC Addlo(s, x)
       MODIFIES Valid[s], Data[s]
       REQUIRES Valid[s] AND s # NIL
       ENSURES Valid'[s]
           AND NUMBER(Data'[s]) = NUMBER(Data[s])+1
           AND Data'[s][0] = x
           AND (ALL [i: CARDINAL]
                 (0 <= i AND i < NUMBER(Data[s])) IMPLIES 
                    Data'[s][i+1] = Data[s][i])
*>

<*SPEC Expand(s)
       MODIFIES Valid[s], Data[s]
       REQUIRES Valid[s]
       ENSURES Valid'[s]
           AND s.st' = 0 AND s.sz' = s.sz
           AND NUMBER(s.elem'^') > NUMBER(s.elem^)
           AND (ALL [i: INTEGER] (0 <= i AND i < NUMBER(Data[s])) IMPLIES
                  Data'[s][i] = Data[s][i])
           AND FRESH(s.elem')
*>

<*SPEC Remhi(s)
       MODIFIES Valid[s], Data[s]
       REQUIRES s # NIL AND Valid[s] AND NUMBER(Data[s]) > 0
       ENSURES Valid'[s] AND NUMBER(Data'[s]) = NUMBER(Data[s])-1
           AND (ALL [i: INTEGER] (0 <= i AND i < NUMBER(Data'[s])) IMPLIES
                  Data'[s][i] = Data[s][i])
*>

<*SPEC Remlo(s)
       MODIFIES Valid[s], Data[s]
       REQUIRES s # NIL AND Valid[s] AND NUMBER(Data[s]) > 0
       ENSURES Valid'[s] AND NUMBER(Data'[s]) = NUMBER(Data[s])-1
           AND (ALL [i: INTEGER] (0 <= i AND i < NUMBER(Data'[s])) IMPLIES
                  Data'[s][i] = Data[s][i+1])
*>

<*SPEC Put(s, i, x)
       MODIFIES Valid[s], Data[s][i]
       REQUIRES s # NIL AND Valid[s] AND i < NUMBER(Data[s])
       ENSURES Valid'[s] AND Data'[s][i] = x
           AND NUMBER(Data'[s]) = NUMBER(Data[s])
*>

<*SPEC Size(s)
       REQUIRES s # NIL AND Valid[s]
       ENSURES RES = NUMBER(Data[s])
*>

<*SPEC Gethi(s)
       REQUIRES s # NIL AND Valid[s] AND NUMBER(Data[s]) > 0
       ENSURES RES = Data[s][NUMBER(Data[s])-1]
*>

<*SPEC Getlo(s)
       REQUIRES s # NIL AND Valid[s] AND NUMBER(Data[s]) > 0
       ENSURES RES = Data[s][0]
*>

<*SPEC Get(s, i)
       REQUIRES s # NIL AND Valid[s] AND i < NUMBER(Data[s])
       ENSURES RES = Data[s][i]
*>

BEGIN
END Sequence.
