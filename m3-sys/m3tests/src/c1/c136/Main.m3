MODULE Main;

EXCEPTION E1;
EXCEPTION E2;

PROCEDURE F1() RAISES ANY =
VAR a := 1;
    PROCEDURE Nest1() =
    VAR b := 2;
    BEGIN
        TRY
            TRY
                VAR c := 3;
                BEGIN
                END;
            EXCEPT E1 =>
                a := 5;
            END;
        FINALLY
            VAR d := 4;
                PROCEDURE Nest2() =
                VAR e := 13;
                BEGIN
                    e := 14;
                    b := 15;
                    a := 16;
                END Nest2;
            BEGIN
                Nest1();
                Nest2();
            END;
        END;
    END Nest1;
BEGIN
    Nest1();
END F1;

PROCEDURE F2() RAISES ANY =
VAR f := 6;
    PROCEDURE Nest2() =
    VAR g := 7;
    BEGIN
        TRY
            TRY
                VAR h := 8;
                BEGIN
                END;
            FINALLY
                VAR i := 10;
                    PROCEDURE Nest4() =
                    VAR j := 11;
                    BEGIN
                        j := 12;
                    END Nest4;
                BEGIN
                    Nest2();
                    Nest4();
                END;
                g := 11;
                f := 12;
            END;
        EXCEPT E2 =>
            VAR k := 16;
                PROCEDURE Nest5() =
                VAR L := 17;
                BEGIN
                    L := 18;
                    k := 19;
                    Nest2();
                END Nest5;
            BEGIN
                Nest2();
                Nest5();
                k := 20;
            END;
            f := 9;
            g := 10;
        END;
    END Nest2;
BEGIN
END F2;

BEGIN
F1();
END Main.
