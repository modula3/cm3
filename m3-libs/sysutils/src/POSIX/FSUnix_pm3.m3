(*--------------------------------------------------------------------------*)
UNSAFE MODULE FSUnix_pm3 EXPORTS FSUtils;

IMPORT Pathname, M3toC;
IMPORT PathRepr;

(*--------------------------------------------------------------------------*)
PROCEDURE IsReadable(fn : Pathname.T) : BOOLEAN =
  BEGIN
    RETURN access(M3toC.TtoS(PathRepr.Native(fn)), R_OK) = 0;
  END IsReadable;

(*--------------------------------------------------------------------------*)
PROCEDURE IsWritable(fn : Pathname.T) : BOOLEAN =
  BEGIN
    RETURN access(M3toC.TtoS(PathRepr.Native(fn)), W_OK) = 0;
  END IsWritable;

(*--------------------------------------------------------------------------*)
PROCEDURE IsExecutable(fn : Pathname.T) : BOOLEAN =
  BEGIN
    RETURN access(M3toC.TtoS(PathRepr.Native(fn)), X_OK) = 0;
  END IsExecutable;

(*--------------------------------------------------------------------------*)

BEGIN
END FSUnix_pm3.
