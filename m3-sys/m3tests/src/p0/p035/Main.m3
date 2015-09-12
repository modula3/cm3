(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: equality of procedures *)

MODULE Main;

IMPORT Text;
FROM Test IMPORT msg, checkI, checkB, check, done;

PROCEDURE dummy (<*UNUSED*> n: Text.T) =
  BEGIN
  END dummy;

TYPE P = PROCEDURE (o: Text.T);

PROCEDURE coco () =
BEGIN
  msg ("group A"); checkB (dummy = msg, FALSE); 
  checkB (dummy # msg, TRUE);
  msg ("group B"); checkB (dummy = dummy, TRUE); 
  checkB (dummy # dummy, FALSE);

  PROCEDURE foo (p: P; same: BOOLEAN) =
    BEGIN
      checkB (dummy = p, same);  
      checkB (dummy # p, NOT same);
    END foo;
  BEGIN
    msg ("group C"); 
    foo (dummy, TRUE);
    msg ("group D"); 
    foo (msg,   FALSE);
  END;


  PROCEDURE foo (p: P; same: BOOLEAN) =
    BEGIN
      checkB (dummy = p, same); 
      checkB (dummy # p, NOT same);
    END foo;
  PROCEDURE bar (p: P; same: BOOLEAN) = 
    BEGIN
      foo (p, same);
    END bar;
  BEGIN
    msg ("group E"); 
    bar (dummy, TRUE);
    msg ("group F"); 
    bar (msg,   FALSE);
  END;


  PROCEDURE foo (p, q: P; same: BOOLEAN) =
    BEGIN
      checkB (p = q, same); 
      checkB (p # q, NOT same);
    END foo;
  BEGIN
    msg ("group G"); 
    foo (dummy, dummy, TRUE);
    msg ("group H"); 
    foo (dummy, msg,   FALSE);
  END;


  PROCEDURE foo (p, q: P; same: BOOLEAN) =
    BEGIN
      checkB (p = q, same);  
      checkB (p # q, NOT same);
    END foo;
  PROCEDURE bar (p, q: P; same: BOOLEAN) =
    BEGIN
      foo (p, q, same);
    END bar;
  BEGIN
    msg ("group I"); 
    bar (dummy, dummy, TRUE);
    msg ("group J"); 
    bar (dummy, msg,  FALSE);
  END;


  VAR x, y: P;
  PROCEDURE foo (same: BOOLEAN) =
    BEGIN
      checkB (x = y, same); 
      checkB (x # y, NOT same);
    END foo;
  BEGIN
    msg ("group K");  
    x := dummy; 
    y := dummy; 
    foo (TRUE);
    msg ("group L");              
    y := msg;   
    foo (FALSE);
  END;


  PROCEDURE foo (p: P; same: BOOLEAN) =
    BEGIN
      checkB (bar = p, same); 
      checkB (bar # p, NOT same); 
    END foo;
  PROCEDURE bar (<*UNUSED*> t: Text.T) =
    BEGIN
    END bar;
  BEGIN
    msg ("group M"); 
    foo (bar, TRUE);
    msg ("group N"); 
    foo (dummy, FALSE);
  END;

  VAR i: INTEGER;
  PROCEDURE foo (p: P; same: BOOLEAN) =
    BEGIN
      checkB (bar = p, same); 
      checkB (bar # p, NOT same);
    END foo;
  PROCEDURE bar (<*UNUSED*> t: Text.T) =
    BEGIN
      i := 4;
    END bar;
  BEGIN
    msg ("group O"); 
    foo (bar, TRUE);
    msg ("group P"); 
    foo (dummy, FALSE);
  END;

  PROCEDURE foo (d, i: INTEGER): INTEGER =
    PROCEDURE bar (): INTEGER =
      BEGIN
      IF d = 0 THEN 
        RETURN (foo (1, i + 3))
      ELSE          
        RETURN i + 5; END;
      END bar;
    BEGIN
      RETURN (bar ());
    END foo;
  BEGIN
    checkI (foo (0, 3), 11);
  END;

  TYPE P = PROCEDURE ();
  VAR i: INTEGER;
  PROCEDURE barbar () = 
    BEGIN
      INC (i);
    END barbar;
  PROCEDURE foofoo (p: P; same: BOOLEAN) =
    VAR pp, ppp: P;
    BEGIN 
      msg ("group Q"); 
      checkB (p = barbar, same);
      msg ("group R"); 
      checkB (p = pp, NOT same);
      msg ("group S"); 
      check (pp = ppp);
    END foofoo;
  BEGIN
    foofoo (barbar, TRUE);
  END;

  done ();
END coco;


BEGIN
  coco ();
END Main.
