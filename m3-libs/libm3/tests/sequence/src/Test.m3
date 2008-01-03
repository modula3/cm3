(* Copyright 1994 Digital Equipment Corporation.             *)
(* Distributed only by permission.                           *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* Last modified on Tue Nov  1 09:10:02 PST 1994 by kalsow   *)
(*      modified on Thu Dec  9 11:45:28 PST 1993 by mcjones  *)

MODULE Test EXPORTS Main;
IMPORT IntSeq;
FROM IntSeq IMPORT Cat, Sub;

TYPE T = IntSeq.T;

PROCEDURE Eq(s, t: T): BOOLEAN =
  VAR i := 0; BEGIN
    IF s.size() # t.size() THEN RETURN FALSE END;
    WHILE i # s.size() AND s.get(i) = t.get(i) DO
      INC(i)
    END;
    RETURN i = s.size()
  END Eq;

PROCEDURE Iota(n: CARDINAL): T =
  VAR res := NEW(T).init(); BEGIN
    FOR i := 0 TO n - 1 DO
      res.addhi(i)
    END;
    RETURN res
  END Iota;

PROCEDURE Rev(s: T): T = 
  VAR res := NEW(T).init(s.size()); BEGIN
    FOR i := 0 TO s.size() - 1 DO
      res.addlo(s.get(i))
    END;
    RETURN res
  END Rev;

VAR s := Iota(5); 

BEGIN
  <* ASSERT Eq(Rev(Rev(Iota(10))), Iota(10)) *>
  <* ASSERT NOT Eq(Iota(5), Iota(0)) *>
  <* ASSERT NOT Eq(Iota(5), Rev(Iota(5))) *>
  FOR i := 4 TO 0 BY -1 DO
    <* ASSERT s.remhi() = i *>
  END;
  <* ASSERT Eq(s, NEW(T).init()) *>
  s := Iota(100);
  <* ASSERT s.size() = 100 *>
  FOR i := 0 TO 99 DO
    <* ASSERT s.remlo() = i *>
  END;
  <* ASSERT Eq(s, NEW(T).init()) *>
  <* ASSERT Iota(12).gethi() = 11 *>
  <* ASSERT Iota(12).getlo() = 0 *>
  s := Iota(10);
  FOR i := 0 TO 9 DO
    s.put(i, i)
  END;
  <* ASSERT Eq(s, Iota(10)) *>

  WITH empty = NEW(REF ARRAY OF INTEGER, 0),
           s = NEW(T).fromArray(empty^) DO
    s.addhi(0); s.addhi(1); s.addhi(2);
    <* ASSERT Eq(Iota(3), s) *>
  END;
  <* ASSERT Eq(Iota(5), NEW(T).fromArray(ARRAY OF INTEGER{0, 1, 2, 3, 4})) *>

  WITH empty = NEW(T).init(),
           s = Cat(empty, empty) DO
    s.addhi(0); s.addhi(1); s.addhi(2);
    <* ASSERT Eq(Iota(3), s) *>
  END;
  <* ASSERT Eq(Iota(5), Cat(Iota(5), Iota(0))) *>
  <* ASSERT Eq(Iota(5), Cat(Iota(0), Iota(5))) *>
  s := Cat(Iota(5), Rev(Iota(5)));
  FOR i := 0 TO 4 DO
    <* ASSERT s.get(i) = i *>
    <* ASSERT s.get(i + 5) = 4 - i *>
  END;

  <* ASSERT(Eq(Sub(Iota(5), 3), NEW(T).fromArray(ARRAY OF INTEGER{3, 4}))) *>
  <* ASSERT Eq(Sub(Iota(5), 0), Iota(5)) *>
  <* ASSERT Eq(Sub(Iota(5), 5, 5), Iota(0)) *>
  <* ASSERT Eq(Sub(Iota(5), 0, 4), Iota(4)) *>
  <* ASSERT Eq(Sub(Rev(Iota(5)), 1, 4), Rev(Iota(4))) *>
  VAR a := NEW(T).init(5); BEGIN
    a.addhi(0);
    a.addhi(0);
    EVAL a.remlo();
    EVAL a.remlo();
    FOR i := 0 TO 4 DO a.addhi(i) END;
    <* ASSERT Eq(a, Iota(5)) *>
  END
END Test.
