MODULE Test20 EXPORTS Main;

IMPORT TextVBT, Trestle;

<*FATAL ANY*>

BEGIN
  WITH v = TextVBT.New("Top-Level Window 1"),
       w = TextVBT.New("Top-Level Window 2") DO
    Trestle.Install(v);
    Trestle.Install(w);
    Trestle.AwaitDelete(v);
  END;
END Test20.
