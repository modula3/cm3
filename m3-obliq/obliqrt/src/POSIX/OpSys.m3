(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

UNSAFE MODULE OpSys;

IMPORT M3toC, Unix;

PROCEDURE GetHostName (): TEXT RAISES {Error} =
  VAR
    buffer: ARRAY [0..255] OF CHAR;
  BEGIN
    WITH nbytes = Unix.gethostname (ADR (buffer), BYTESIZE (buffer) - 1) DO
      IF nbytes < 0 THEN 
        RAISE Error; 
      END;
    END;
    RETURN M3toC.CopyStoT (ADR (buffer));
  END GetHostName;


BEGIN
END OpSys.
