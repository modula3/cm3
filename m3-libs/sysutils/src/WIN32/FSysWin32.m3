(*--------------------------------------------------------------------------*)
UNSAFE MODULE FSysWin32 EXPORTS FSUtils;

IMPORT Pathname, WinNT, WinUser, M3toC, Text, OSError, File;
IMPORT PathRepr, (* FSFixed AS *) FS;

(*--------------------------------------------------------------------------*)
PROCEDURE IsReadable(fn : Pathname.T) : BOOLEAN =
  VAR f : File.T;
  BEGIN
    (* POSIX implementation:
      RETURN Unix.access(M3toC.TtoS(PathRepr.Native(fn)), Unix.R_OK) = 0;
    *)
    (* FIXME: write me! *)
    (* workaround inserted: just try to open it *)
    TRY
      f := FS.OpenFileReadonly(PathRepr.Native(fn));
      f.close();
      RETURN TRUE;
    EXCEPT
      OSError.E => RETURN FALSE
    END;
  END IsReadable;

(*--------------------------------------------------------------------------*)
PROCEDURE IsWritable(fn : Pathname.T) : BOOLEAN =
  VAR f : File.T;
  BEGIN
    (* POSIX implementation:
       RETURN Unix.access(M3toC.TtoS(PathRepr.Native(fn)), Unix.W_OK) = 0;
    *)
    (* FIXME: write me! *)
    (* workaround inserted: just try to open it *)
    TRY
      f := FS.OpenFile(PathRepr.Native(fn), FALSE);
      f.close();
      RETURN TRUE;
    EXCEPT
      OSError.E => RETURN FALSE
    END;
  END IsWritable;

(*--------------------------------------------------------------------------*)
PROCEDURE IsExecutable(fn : Pathname.T) : BOOLEAN =
  BEGIN
    (* POSIX implementation:
       RETURN Unix.access(M3toC.TtoS(PathRepr.Native(fn)), Unix.X_OK) = 0;
    *)
    (* FIXME: write me! *)
    WITH ext = Pathname.LastExt(fn) DO
      RETURN Text.Equal(ext, "exe") OR Text.Equal(ext, "com");
    END;
  END IsExecutable;

BEGIN
END FSysWin32.
