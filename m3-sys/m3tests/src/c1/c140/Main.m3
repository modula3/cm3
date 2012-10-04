MODULE Main;
IMPORT RTIO;

TYPE T1 = RECORD int := 100; END;
VAR int:T1;
VAR long:RECORD int := 200; END;
VAR g:= 1000;

PROCEDURE F1() =
VAR int := 300;
    PROCEDURE F2() = BEGIN
        RTIO.PutInt(int); RTIO.PutText("\n");
        INC(int, int DIV 100);
        RTIO.PutInt(int); RTIO.PutText("\n");
        INC(g, int);
    END F2;
BEGIN
    VAR int := 400;
    PROCEDURE F3() = BEGIN
        F2();
        RTIO.PutInt(int); RTIO.PutText("\n");
        INC(int, int DIV 100);
        RTIO.PutInt(int); RTIO.PutText("\n");
        INC(g, int);
        RTIO.PutInt(int); RTIO.PutText("\n");
    END F3;
    BEGIN
        RTIO.PutInt(int); RTIO.PutText("\n");
        INC(int);
        RTIO.PutInt(int); RTIO.PutText("\n");
        F3();
    END;
    F2();
END F1;

BEGIN
    RTIO.PutInt(int.int); RTIO.PutText("\n");
    F1();
    RTIO.Flush();
END Main.
