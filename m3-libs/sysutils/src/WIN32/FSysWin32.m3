(*--------------------------------------------------------------------------*)
UNSAFE MODULE FSysWin32 EXPORTS FSUtils;

IMPORT Pathname, Text, OSError, File;
IMPORT PathRepr, (* FSFixed AS *) FS;
IMPORT ASCII, M3toC, Unix;

(*--------------------------------------------------------------------------*)
PROCEDURE PosixIsReadable(fn : Pathname.T) : BOOLEAN =
(* This is really just an existance check. *)
  VAR
    fna := PathRepr.Native(fn);
    fname := M3toC.SharedTtoS(fna);
    res := Unix.access(fname, Unix.R_OK) = 0;
  BEGIN
    M3toC.FreeSharedS(fna, fname);
    RETURN res;
  END PosixIsReadable;

(*--------------------------------------------------------------------------*)
PROCEDURE PosixIsWritable(fn : Pathname.T) : BOOLEAN =
(* This is really just an existance and read-only attribute check. *)
  VAR
    fna := PathRepr.Native(fn);
    fname := M3toC.SharedTtoS(fna);
    res := Unix.access(fname, Unix.W_OK) = 0;
  BEGIN
    M3toC.FreeSharedS(fna, fname);
    RETURN res;
  END PosixIsWritable;

(*--------------------------------------------------------------------------*)
PROCEDURE PosixIsExecutable(fn : Pathname.T) : BOOLEAN =
(* This is really just an existance check. *)
  VAR
    fna := PathRepr.Native(fn);
    fname := M3toC.SharedTtoS(fna);
    res := Unix.access(fname, Unix.F_OK) = 0;
  BEGIN
    M3toC.FreeSharedS(fna, fname);
    RETURN res;
  END PosixIsExecutable;

(*--------------------------------------------------------------------------*)
PROCEDURE IsReadable(fn : Pathname.T) : BOOLEAN =
  VAR f : File.T;
  BEGIN
    IF NOT PosixIsReadable(fn) THEN
      RETURN FALSE;
    END;
    (* and try to open it *)
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
    IF NOT PosixIsWritable(fn) THEN
      RETURN FALSE;
    END;
    (* and try to open it *)
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
  VAR ext: TEXT;
      len: CARDINAL;
      ext0, ext1, ext2: CHAR;
  BEGIN
    IF NOT PosixIsExecutable(fn) THEN
      RETURN FALSE;
    END;
    ext := Pathname.LastExt(fn);
    len := Text.Length(ext);
    IF len # 3 THEN
      RETURN FALSE;
    END;
    ext0 := ASCII.Upper[Text.GetChar(ext, 0)];
    ext1 := ASCII.Upper[Text.GetChar(ext, 1)];
    ext2 := ASCII.Upper[Text.GetChar(ext, 2)];
    RETURN (ext0 = 'E' AND ext1 = 'X' AND ext2 = 'E')
        OR (ext0 = 'C' AND ext1 = 'O' AND ext2 = 'M')
        OR (ext0 = 'B' AND ext1 = 'A' AND ext2 = 'T')
        OR (ext0 = 'C' AND ext1 = 'M' AND ext2 = 'D');
  END IsExecutable;

(*--------------------------------------------------------------------------*)

BEGIN
END FSysWin32.
