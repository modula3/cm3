
UNSAFE MODULE Lib;
IMPORT Ulib, M3toC;
FROM Ctypes IMPORT char_star;
FROM Cstdlib IMPORT malloc, free;

PROCEDURE GetCWD(): TEXT = 
  CONST
    size = 64;
  VAR
    c_str := malloc (size);
  BEGIN

    EVAL Ulib.getcwd(c_str,size);
    WITH result = M3toC.CopyStoT(c_str) DO
      free(c_str);
      RETURN result;
    END;

  END GetCWD;
    
BEGIN
END Lib.
    
    
