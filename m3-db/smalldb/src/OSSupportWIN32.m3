UNSAFE MODULE OSSupportWIN32 EXPORTS OSSupport;

IMPORT OSError, OSErrorWin32, File, FileWr, FileWin32, WrClass, WinNT, 
       OSSupportWIN32IF;

REVEAL
  T = FileWr.T BRANDED OBJECT
    handle: WinNT.HANDLE;
  OVERRIDES
    init := MyInit;
  END;

PROCEDURE MyInit(t: T; f: File.T; buffered: BOOLEAN := TRUE) : FileWr.T
    RAISES {OSError.E} =
  BEGIN
    t.handle := f.handle;
    RETURN FileWr.T.init(t, f, buffered);
  END MyInit;

PROCEDURE Sync(<*UNUSED*>wr: T) RAISES {} =
  BEGIN
    OSSupportWIN32IF.FlushFileBuffers();
  END Sync;

PROCEDURE Truncate(wr: T) RAISES {OSError.E} =
  BEGIN
    WrClass.Lock(wr);
    TRY
      IF OSSupportWIN32IF.SetEndOfFile(wr.handle, wr.cur) = 0 THEN
        OSErrorWin32.Raise();
      END;
    FINALLY
      WrClass.Unlock(wr);
    END;
  END Truncate;

BEGIN
END OSSupportWIN32.

