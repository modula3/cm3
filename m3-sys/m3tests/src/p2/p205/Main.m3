MODULE Main;

IMPORT I, IO;

TYPE
  T = ARRAY [0..1] OF CHAR;

VAR
  one := 1;
  two := 2;
BEGIN
  VAR
    c: T := SUBARRAY(I.a, 1, 2);
    d: T := SUBARRAY(I.a, 1, two);
    (* bug? *)
    (*e: T := SUBARRAY(I.a, one, 2);*)
    (*f: T := SUBARRAY(I.a, one, two);*)
    e := I.b;
    f := I.b;
    g: T := SUBARRAY(I.b, 1, 2);
    h: T := SUBARRAY(I.b, 1, two);
    i: T := SUBARRAY(I.b, one, 2);
    j: T := SUBARRAY(I.b, one, two);
  BEGIN
    IO.PutChar(c[0]);
    IO.PutChar(d[0]);
    IO.PutChar(e[0]);
    IO.PutChar(f[0]);
    IO.PutChar(g[0]);
    IO.PutChar(h[0]);
    IO.PutChar(i[0]);
    IO.PutChar(j[0]);
    IO.Put("\nOK\n");
  END;
END Main.
