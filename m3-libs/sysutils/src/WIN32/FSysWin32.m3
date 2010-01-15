(*--------------------------------------------------------------------------*)
UNSAFE MODULE FSysWin32 EXPORTS FSUtils;

IMPORT Pathname, Text, OSError, File;
IMPORT PathRepr, (* FSFixed AS *) FS;
IMPORT FSUtilsUnsafe, M3toC;

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

(*--------------------------------------------------------------------------*)

PROCEDURE GetFileSize32(path:TEXT):INTEGER =
  VAR cpath := M3toC.SharedTtoS(path);
      res := FSUtilsUnsafe.GetFileSize32(cpath);
  BEGIN
    M3toC.FreeSharedS(path, cpath);
    RETURN res;
  END GetFileSize32;

BEGIN
END FSysWin32.
