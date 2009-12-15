MODULE Main EXPORTS Main, finally;

PROCEDURE F2() RAISES {E1} =
  BEGIN
    RAISE E1;
  END F2;

BEGIN
  TRY
    F1();
  EXCEPT E1 =>
  END;
END Main.
