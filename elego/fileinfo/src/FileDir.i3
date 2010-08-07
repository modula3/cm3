(*--------------------------------------------------------------------------*)
INTERFACE FileDir;

IMPORT APN AS APN, APNSeq AS APNSeq.T;

TYPE
  T <: Public;

  Public = OBJECT
  METHODS
    init(path : APN.T) : T;
    addDir(fn : APN.T);
    addFile(fn : APN.T);
    delDir(fn : APN.T);
    delFile(fn : APN.T);
    isFile(fn : APN.T) : BOOLEAN;
    isDir(fn : APN.T) : BOOLEAN;
    list() : APNSeq.T;
  END;

END FileDir.
