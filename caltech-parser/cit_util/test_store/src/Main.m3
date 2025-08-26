(* $Id: Main.m3,v 1.3 2007/05/31 22:50:05 mika Exp $ *)

MODULE Main;
IMPORT PersistentIntRefTbl AS ObjectStore;
IMPORT Debug;
IMPORT Fmt;
IMPORT Random;
IMPORT IntRefTbl;

CONST Iters = 100000;
CONST Max = 300;
CONST Path = "store";
CONST InitSize = 1;
VAR
  store := NEW(ObjectStore.T).init(Path,
                                   initSize := InitSize,
                                   mode := ObjectStore.Mode.Replace);
  ref := NEW(IntRefTbl.Default).init();
  rand := NEW(Random.Default).init();
BEGIN
  FOR i := 0 TO Iters-1 DO
    CASE rand.integer(1,7) OF
      1,6,7 =>
      WITH k = rand.integer(-Max,Max),
           v = rand.integer(-Max,Max),
           new = NEW(REF INTEGER) DO
        new^ := v;
        Debug.Out("Put(" & Fmt.Int(k) & "," & Fmt.Int(v) & ")");
        <* ASSERT store.put(k,new) = ref.put(k,new) *>
        <* ASSERT store.size() = ref.size() *>
        
        VAR r1 : REFANY; BEGIN <* ASSERT store.get(k,r1) *> END
          
      END

    |
      2 =>
      WITH k = rand.integer(-Max,Max) DO
        VAR
          r1, r2 : REFANY;
          g1 := store.get(k,r1);
          g2 := ref.get(k,r2);
        BEGIN
          Debug.Out("Get(" & Fmt.Int(k) & ")");
          <* ASSERT g1 = g2 *>
          IF g1 THEN 
            <* ASSERT NARROW(r1,REF INTEGER)^ = NARROW(r2, REF INTEGER)^ *>
          END
        END
      END
    |
      3 =>
      WITH k = rand.integer(-Max,Max) DO
        VAR
          r1, r2 : REFANY;
          g1 := store.delete(k,r1);
          g2 := ref.delete(k,r2);
        BEGIN
          Debug.Out("Delete(" & Fmt.Int(k) & ")");
          <* ASSERT g1 = g2 *>
          IF g1 THEN 
            <* ASSERT NARROW(r1,REF INTEGER)^ = NARROW(r2, REF INTEGER)^ *>
          END
        END
      END
    |
      4 => Debug.Out("CheckSize()"); <* ASSERT store.size() = ref.size() *>
    |
      5 => Debug.Out("Init()"); 
      WITH sz = store.size() DO
        store.close();
        store := NEW(ObjectStore.T).init(Path, initSize := InitSize);
        <* ASSERT store.size() = sz *>
      END
    ELSE
      <* ASSERT FALSE *>
    END
  END
END Main.
