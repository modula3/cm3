(*--------------------------------------------------------------------------*)
MODULE FileDir;

IMPORT APN AS APN, APNSeq AS APNSeq.T;

REVEAL
  T = Public BRANDED "FileDir 0.0" OBJECT
    dirs  : APNSeq.T;
    files : APNSeq.T;
  OVERRIDES
    init(path : APN.T) : T;
    add(fn : APN.T);
    del(fn : APN.T);
    isFile(fn : APN.T) : BOOLEAN;
    isDir(fn : APN.T) : BOOLEAN;
    list() : APNSeq.T;
  END;

PROCEDURE Init(self : T; path : APN.T) : T;
PROCEDURE add(self : T; fn : APN.T);
PROCEDURE del(self : T; fn : APN.T);
PROCEDURE isFile(self : T; fn : APN.T) : BOOLEAN;
PROCEDURE isDir(self : T; fn : APN.T) : BOOLEAN;
PROCEDURE list(self : T) : APNSeq.T;
END FileDir.
