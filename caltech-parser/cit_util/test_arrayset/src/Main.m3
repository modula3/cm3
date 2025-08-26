(* $Id: Main.m3,v 1.1 2007/06/24 22:17:50 mika Exp $ *)

MODULE Main;
IMPORT Random, CardSetDef, CardSetArray;
IMPORT IO;
IMPORT Fmt;

VAR rand := NEW(Random.Default).init();

VAR s1 := NEW(CardSetDef.T).init();
    s2 := NEW(CardSetArray.T).init();

CONST Iters = 1000000;
CONST MaxCard = 20;

VAR maxSize := 0;

PROCEDURE Check() = 
  BEGIN
    <* ASSERT s1.size() = s2.size() *>
    <* ASSERT s1.equal(s2) *>
    maxSize := MAX(maxSize, s1.size())
  END Check;

BEGIN
  FOR i := 0 TO Iters DO
    CASE rand.integer(0,5) OF
      0 => 
        s1 := NEW(CardSetDef.T).init();
        s2 := NEW(CardSetArray.T).init();
        Check()
    |
      1,4,5 => 
      WITH v = rand.integer(0,MaxCard),
           x1 = s1.insert(v),
           x2 = s2.insert(v) DO
        <* ASSERT x1 = x2 *>
        Check()
      END
    |
      2 => 
      WITH v = rand.integer(0,MaxCard),
           x1 = s1.delete(v),
           x2 = s2.delete(v) DO
        <* ASSERT x1 = x2 *>
        Check()
      END
    |
      3 => s1 := s1.copy(); s2 := s2.copy(); Check()
    ELSE
      <* ASSERT FALSE *>
    END
  END;

  IO.Put("maxSize = " & Fmt.Int(maxSize) & "\n")
END Main.
