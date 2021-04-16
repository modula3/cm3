(*--------------------------------------------------------------------------*)
UNSAFE MODULE FSUnix_cm3 EXPORTS FSUtils;

IMPORT Pathname, M3toC;
IMPORT PathRepr, Unix;

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

BEGIN
END FSUnix_cm3.
