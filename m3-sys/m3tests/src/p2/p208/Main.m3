MODULE Main;

IMPORT RuntimeError, IO, Process;

CONST Tag = RuntimeError.Tag;

PROCEDURE CatchAssert() =
  BEGIN
    TRY
      <*ASSERT FALSE*>
    EXCEPT
      RuntimeError.E( t ) =>
      IO.Put( "OK: caught RuntimeError.Assert: " & Tag( t ) & "\n" );
    END;
  END CatchAssert;

PROCEDURE TestAll() =
  BEGIN
    CatchAssert();
  END TestAll;

BEGIN
  TRY
    TestAll();
  EXCEPT
  ELSE
    IO.Put( "ERROR: caught unexpected exception\n" );
    Process.Exit( 1 );
  END;
END Main.
