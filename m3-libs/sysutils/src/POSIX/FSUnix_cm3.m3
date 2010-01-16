(*--------------------------------------------------------------------------*)
UNSAFE MODULE FSUnix_cm3 EXPORTS FSUtils;

IMPORT Pathname, Unix, M3toC;
IMPORT PathRepr, FSUtilsUnsafe;

(*--------------------------------------------------------------------------*)
PROCEDURE IsReadable(fn : Pathname.T) : BOOLEAN =
  VAR
    fna := PathRepr.Native(fn);
    fname := M3toC.SharedTtoS(fna);
    res := Unix.access(fname, Unix.R_OK) = 0;
  BEGIN
    M3toC.FreeSharedS(fna, fname);
    RETURN res;
  END IsReadable;

(*--------------------------------------------------------------------------*)
PROCEDURE IsWritable(fn : Pathname.T) : BOOLEAN =
  VAR
    fna := PathRepr.Native(fn);
    fname := M3toC.SharedTtoS(fna);
    res := Unix.access(fname, Unix.W_OK) = 0;
  BEGIN
    M3toC.FreeSharedS(fna, fname);
    RETURN res;
  END IsWritable;

(*--------------------------------------------------------------------------*)
PROCEDURE IsExecutable(fn : Pathname.T) : BOOLEAN =
  VAR
    fna := PathRepr.Native(fn);
    fname := M3toC.SharedTtoS(fna);
    res := Unix.access(fname, Unix.X_OK) = 0;
  BEGIN
    M3toC.FreeSharedS(fna, fname);
    RETURN res;
  END IsExecutable;

(*--------------------------------------------------------------------------*)

PROCEDURE GetFileSize32(path:TEXT):INTEGER =
  VAR cpath := M3toC.SharedTtoS(path);
      res := FSUtilsUnsafe.GetFileSize32(cpath);
  BEGIN
    M3toC.FreeSharedS(path, cpath);
    RETURN res;
  END GetFileSize32;

PROCEDURE GetFileSize(path:TEXT):INTEGER =
  VAR cpath := M3toC.SharedTtoS(path);
      res := FSUtilsUnsafe.GetFileSize(cpath);
  BEGIN
    M3toC.FreeSharedS(path, cpath);
    RETURN res;
  END GetFileSize;

BEGIN
END FSUnix_cm3.
