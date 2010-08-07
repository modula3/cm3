(*---------------------------------------------------------------------------*)
INTERFACE CVSLockInfo;

CONST Brand = "Elego ComPact CVS LockInfo 0.0";

TYPE 
  T = OBJECT
    fn   : TEXT;
    user : TEXT;
    date : TEXT;
    host : TEXT;
    dir  : TEXT;
  END;

END CVSLockInfo.
