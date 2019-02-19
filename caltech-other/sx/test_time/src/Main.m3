(* $Id$ *)

MODULE Main;
IMPORT SXTime, SXSelect, Fmt, Debug, SX;

VAR
  sx := SXTime.New(10.0d0,3.33d0);
BEGIN
  LOOP
    SX.Lock1(sx);
    TRY
      SXSelect.Wait1(sx);
      Debug.Out("sx = " & Fmt.LongReal(sx.value()))
    FINALLY
      SX.Unlock1(sx)
    END
  END


END Main.
