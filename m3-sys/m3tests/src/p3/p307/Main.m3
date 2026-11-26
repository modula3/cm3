MODULE Main;

(* Subset comparisons of sets wider than a word.  The C back end passed the
   operands of m3_set_le / m3_set_lt in the wrong order, so a <= b computed
   b <= a; = and # were inverted (memcmp's result read the wrong way round);
   and a unit whose only set operation was a comparison did not compile
   (SET undeclared). *)

IMPORT IO;

TYPE Big = SET OF [10 .. 200];

VAR
  a := Big {12};
  b := Big {10, 12, 13, 150};
  c := Big {11, 199};
  e := Big {};

PROCEDURE B (x: BOOLEAN): TEXT =
  BEGIN IF x THEN RETURN "T" ELSE RETURN "F" END END B;

BEGIN
  IO.Put ("a<=b " & B (a <= b) & " a<b " & B (a < b) & " a>=b " & B (a >= b)
          & " a>b " & B (a > b) & "\n");
  IO.Put ("b<=a " & B (b <= a) & " b<a " & B (b < a) & " b>=a " & B (b >= a)
          & " b>a " & B (b > a) & "\n");
  IO.Put ("a<=a " & B (a <= a) & " a<a " & B (a < a) & " a>=a " & B (a >= a)
          & " a>a " & B (a > a) & "\n");
  IO.Put ("a<=c " & B (a <= c) & " c<=a " & B (c <= a) & " e<=a " & B (e <= a)
          & " e<a " & B (e < a) & " a<=e " & B (a <= e) & "\n");
  IO.Put ("a=a " & B (a = a) & " a=b " & B (a = b) & " a#b " & B (a # b)
          & " e=e " & B (e = Big {}) & "\n");
  IF a = Big {12} THEN IO.Put ("if a=a T") ELSE IO.Put ("if a=a F") END;
  IF a = b THEN IO.Put (" if a=b T") ELSE IO.Put (" if a=b F") END;
  IF a # b THEN IO.Put (" if a#b T") ELSE IO.Put (" if a#b F") END;
  IF a # Big {12} THEN IO.Put (" if a#a T\n") ELSE IO.Put (" if a#a F\n") END;
END Main.
