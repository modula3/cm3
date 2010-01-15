(*--------------------------------------------------------------------------*)
UNSAFE MODULE FSUnix_pm3 EXPORTS FSUtils;

IMPORT Pathname, Unix, M3toC;
IMPORT PathRepr;

(*--------------------------------------------------------------------------*)
PROCEDURE IsReadable(fn : Pathname.T) : BOOLEAN =
  BEGIN
    RETURN Unix.access(M3toC.TtoS(PathRepr.Native(fn)), Unix.R_OK) = 0;
  END IsReadable;

(*--------------------------------------------------------------------------*)
PROCEDURE IsWritable(fn : Pathname.T) : BOOLEAN =
  BEGIN
    RETURN Unix.access(M3toC.TtoS(PathRepr.Native(fn)), Unix.W_OK) = 0;
  END IsWritable;

(*--------------------------------------------------------------------------*)
PROCEDURE IsExecutable(fn : Pathname.T) : BOOLEAN =
  BEGIN
    RETURN Unix.access(M3toC.TtoS(PathRepr.Native(fn)), Unix.X_OK) = 0;
  END IsExecutable;

(*--------------------------------------------------------------------------*)

PROCEDURE GetFileSize32(path:TEXT):INTEGER =
  BEGIN
      RETURN FSUtilsUnsafe.GetFileSize32(M3toC.TtoS(path));
  END GetFileSize32;

BEGIN
END FSUnix_pm3.
