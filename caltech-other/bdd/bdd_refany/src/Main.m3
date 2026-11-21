MODULE Main;
IMPORT BDD, IO;

PROCEDURE P() : REFANY =
  BEGIN
    VAR x : REFANY;
    BEGIN
      x := BDD.New("a");
      Z(x);
      RETURN x
    END
  END P;

PROCEDURE Q() =
  BEGIN
    IO.Put(P() & "\n")
  END Q;

(*
PROCEDURE Z(VAR x : REFANY) =
  BEGIN
    TYPECASE x OF
      BDD.T(b) => x := BDD.Format(b)
    END;
  END Z;
*)
PROCEDURE Z(VAR x : REFANY) =
   VAR b: BDD.T;
   BEGIN
     b := x;
     x := BDD.Format(b)
   END Z;



BEGIN
  Q()
END Main.
