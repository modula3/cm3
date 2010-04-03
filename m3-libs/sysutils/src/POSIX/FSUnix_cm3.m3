(*--------------------------------------------------------------------------*)
UNSAFE MODULE FSUnix_cm3 EXPORTS FSUtils;

IMPORT Pathname, M3toC;
IMPORT PathRepr;

(*--------------------------------------------------------------------------*)
PROCEDURE IsReadable(fn : Pathname.T) : BOOLEAN =
  VAR
    fna := PathRepr.Native(fn);
    fname := M3toC.SharedTtoS(fna);
    res := access(fname, R_OK) = 0;
  BEGIN
    M3toC.FreeSharedS(fna, fname);
    RETURN res;
  END IsReadable;

(*--------------------------------------------------------------------------*)
PROCEDURE IsWritable(fn : Pathname.T) : BOOLEAN =
  VAR
    fna := PathRepr.Native(fn);
    fname := M3toC.SharedTtoS(fna);
    res := access(fname, W_OK) = 0;
  BEGIN
    M3toC.FreeSharedS(fna, fname);
    RETURN res;
  END IsWritable;

(*--------------------------------------------------------------------------*)
PROCEDURE IsExecutable(fn : Pathname.T) : BOOLEAN =
  VAR
    fna := PathRepr.Native(fn);
    fname := M3toC.SharedTtoS(fna);
    res := access(fname, X_OK) = 0;
  BEGIN
    M3toC.FreeSharedS(fna, fname);
    RETURN res;
  END IsExecutable;

(*--------------------------------------------------------------------------*)

BEGIN
END FSUnix_cm3.
