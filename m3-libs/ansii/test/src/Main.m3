MODULE Main;

IMPORT IO,Fmt;
IMPORT AnsiSeq AS A;

PROCEDURE W (t: TEXT) =
  BEGIN
    (* bell *)
    IO.Put("\X07");

    (* reverse video *)
    IO.Put(A.REVERSE & "REVERSE\n");

    (* print red, bright red then cyan *)
    IO.Put(A.RED & "hi there" & A.BRED & "BRED" & A.CYN & "CYN\n");
    (* print yellow then bright yellow *)
    IO.Put(A.YEL & "YELLOW" & A.BYEL & "BYELLOW\n");
    (* move cursor right 7 columns *)
    IO.Put(A.CUF(7));
    (* move cursor up 20 rows *)
    IO.Put(A.CPL(20));

    (* print red on white background *)
    IO.Put(A.SGR(ARRAY OF CARDINAL{1,91,107}));
    IO.Put("red on white");

    IO.Put(A.RESET);

  END W;

BEGIN

  W("test");

END Main.

