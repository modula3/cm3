GENERIC MODULE FilterBank(S, SM);

PROCEDURE ToPolyphase (READONLY x: TBody; scaling: S.ScalingType; ): SM.T =
  VAR z := NEW(SM.T, NUMBER(x), scaling);
  BEGIN
    FOR i := FIRST(x) TO LAST(x) DO z[i] := x[i].sliceRev(scaling)^; END;
    RETURN z;
  END ToPolyphase;

PROCEDURE FromPolyphase (READONLY x: SM.TBody; ): T =
  VAR z := NEW(T, NUMBER(x));
  BEGIN
    FOR i := FIRST(x) TO LAST(x) DO
      z[i] := NEW(S.T).interleaveRev(x[i]);
    END;
    RETURN z;
  END FromPolyphase;

BEGIN
END FilterBank.
