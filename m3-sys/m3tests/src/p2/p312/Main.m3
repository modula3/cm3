(* An object type's method as a procedure value (T.m): bound by WITH, held in
   a variable, and called directly, including for a subtype of an opaque type,
   whose method offsets are known only at run time. *)

MODULE Main;
IMPORT IO, Fmt, Op;

TYPE
  S = Op.T OBJECT k := 4 METHODS
        n (): INTEGER := N;
        n2 (x: INTEGER): INTEGER := N2;
      END;
  U = S OBJECT OVERRIDES n := NU END;
  O = OBJECT v: INTEGER METHODS m (): INTEGER := M END;

PROCEDURE N  (s: S): INTEGER = BEGIN RETURN s.k * 10 END N;
PROCEDURE N2 (s: S;  x: INTEGER): INTEGER = BEGIN RETURN s.k + x END N2;
PROCEDURE NU (s: S): INTEGER = BEGIN RETURN s.k * 100 END NU;
PROCEDURE M  (o: O): INTEGER = BEGIN RETURN o.v END M;

PROCEDURE Run () =
  VAR s  := NEW (S);  u := NEW (U);  o := NEW (O, v := 3);
      pm := S.n;
  BEGIN
    WITH a = S.n, b = U.n, c = U.n2, mm = O.m DO
      IO.Put (Fmt.Int (a (s)) & " " & Fmt.Int (b (u)) & " "
              & Fmt.Int (c (u, 3)) & " " & Fmt.Int (mm (o)) & "\n");
    END;
    IO.Put (Fmt.Int (pm (u)) & " " & Fmt.Int (S.n (u)) & "\n");
  END Run;

BEGIN
  Run ();
END Main.
