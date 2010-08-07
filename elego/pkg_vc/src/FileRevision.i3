(*---------------------------------------------------------------------------*)
INTERFACE FileRevision;

IMPORT APN;

CONST Brand = "FileRevision Module 0.1";

(*---------------------------------------------------------------------------*)
TYPE T = OBJECT
  file     : APN.T;
  revision : TEXT;
END;

END FileRevision.
