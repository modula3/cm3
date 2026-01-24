MODULE Main;

(* a bug in StackX86 in clearall asserted with a register still in use
   I think. The original bug in m3tk/targer/M3CBackendC.m3 in Compare
   But cannot reproduce using cm3 -DTARGET=NT386 so dont know what
   is the problem and dont have my windows machine at hand. *)

IMPORT IO;

TYPE
  Base = OBJECT
    bval : CHAR;
  END;

  IntO = Base OBJECT
    val : INTEGER;
  END;

  LongO = Base OBJECT
    val : LONGINT;
  END;

VAR
  t1,t2 : Base;
  res : INTEGER;

PROCEDURE T(e1,e2 : Base) : INTEGER =
  BEGIN
    TYPECASE e1 OF
    | IntO(x) =>
      TYPECASE e2 OF
      | IntO(y) =>
          IF x.val = y.val THEN
            RETURN 0;
          ELSIF x.val < y.val THEN
            RETURN -1;
          ELSE
            RETURN 1;
          END;
      | LongO(y) =>
          IF VAL(x.val,LONGINT) = y.val THEN
            RETURN 0;
          ELSIF VAL(x.val,LONGINT) < y.val THEN
            RETURN -1;
          ELSE
            RETURN 1;
          END;
      ELSE
        RETURN 1;
      END;

    | LongO(x) =>
      TYPECASE e2 OF
      | LongO(y) =>
          IF x.val = y.val THEN
            RETURN 0;
          ELSIF x.val < y.val THEN
            RETURN -1;
          ELSE
            RETURN 1;
          END;
      | IntO(y) =>
          IF x.val = VAL(y.val,LONGINT) THEN
          (* should crash here on NT in stackx86.m3 in clearall
             using the integrated backend *)
            RETURN 0;
          ELSIF x.val < VAL(y.val,LONGINT) THEN
            RETURN -1;
          ELSE
            RETURN 1;
          END;
       ELSE
         RETURN 1;
      END;
    ELSE
      RETURN 1;
    END;

  END T;


BEGIN
  t1 := NEW(IntO, val := 2);
  t2 := NEW(LongO, val := 3L);
  res := T(t1,t2);
  IO.Put("OK");
END Main.

