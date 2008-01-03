UNSAFE MODULE OSSupportWIN32 EXPORTS OSSupport;

IMPORT OSError, OSErrorWin32, File, FileWr, FileWin32, WrClass, WinNT, 
       OSSupportWIN32IF, WinBase;

CONST INVALID_SET_FILE_POINTER = -1; (* FIXME: is this correct? *)

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

PROCEDURE Sync(wr: T) RAISES {OSError.E} =
  BEGIN
    IF OSSupportWIN32IF.FlushFileBuffers(wr.handle) = 0 THEN
      OSErrorWin32.Raise();
    END;
  END Sync;

PROCEDURE Truncate(wr: T) RAISES {OSError.E} =
  BEGIN
    WrClass.Lock(wr);
    TRY
      IF WinBase.SetFilePointer(wr.handle, wr.cur, NIL,
                                WinBase.FILE_BEGIN) = 
                                INVALID_SET_FILE_POINTER THEN
        OSErrorWin32.Raise();
      END;
      IF OSSupportWIN32IF.SetEndOfFile(wr.handle) = 0 THEN
        OSErrorWin32.Raise();
      END;
    FINALLY
      WrClass.Unlock(wr);
    END;
  END Truncate;

BEGIN
END OSSupportWIN32.

