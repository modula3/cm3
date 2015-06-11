(* Test shift_right with a signed integer in the IR.
   Specifically m3front/src/misc/cg/Setrange.
   
   Be sure to hit the top bit.
   
   Also test setting of one bit in a set. This had regressed.
*)
UNSAFE MODULE Main;
IMPORT Dump;

PROCEDURE F31(start, count: INTEGER) =
(* Fit in word on all machines. *)
    CONST X = 31;
    TYPE T = SET OF[0..X];
    VAR end := start + count - 1;
    VAR a := T{ };
BEGIN
    IF end > X OR end < 0 OR start > X THEN RETURN END;
    (*Dump.Print1("start ", start);
    Dump.Print1("end ", end);*)
    a := T{start..end};
    Dump.Dump(X, start, count, BITSIZE(T), BYTESIZE(T), ADR(a));
END F31;

PROCEDURE F32(start, count: INTEGER) =
(* Just over a word on 32bit machines. *)
    CONST X = 32;
    TYPE T = SET OF[0..X];
    VAR end := start + count - 1;
    VAR a := T{ };
BEGIN
    IF end > X OR end < 0 OR start > X THEN RETURN END;
    a := T{start..end};
    Dump.Dump(X, start, count, BITSIZE(T), BYTESIZE(T), ADR(a));
END F32;

PROCEDURE F63(start, count: INTEGER) =
(* Fit in word on 64bit machines. *)
    CONST X = 63;
    TYPE T = SET OF[0..X];
    VAR end := start + count - 1;
    VAR a := T{ };
BEGIN
    IF end > X OR end < 0 OR start > X THEN RETURN END;
    a := T{start..end};
    Dump.Dump(X, start, count, BITSIZE(T), BYTESIZE(T), ADR(a));
END F63;

PROCEDURE F64(start, count: INTEGER) =
(* Fit in word on no machines. *)
(* Just over a word on 64bit machines. *)
    CONST X = 64;
    TYPE T = SET OF[0..X];
    VAR end := start + count - 1;
    VAR a := T{ };
BEGIN
    IF end > X OR end < 0 OR start > X THEN RETURN END;
    a := T{start..end};
    Dump.Dump(X, start, count, BITSIZE(T), BYTESIZE(T), ADR(a));
END F64;

CONST offset_bases = ARRAY OF INTEGER {0,32,64};
BEGIN
    FOR offset_base := FIRST(offset_bases) TO LAST(offset_bases) DO
        FOR offset_offset := -1 TO 1 DO
            WITH offset = offset_bases[offset_base] + offset_offset DO
                IF offset >= 0 THEN
                    FOR count := 0 TO 2 DO
                        F31(offset, count);
                        F32(offset, count);
                        F63(offset, count);
                        F64(offset, count);
                    END;
                END;
            END;
        END;
    END;
END Main.
