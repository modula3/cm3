(* $Id$ *)

UNSAFE MODULE SchemeUnixDeps;
FROM Ctypes IMPORT char_star;
IMPORT M3toC;
IMPORT Pathname;

PROCEDURE GetCurrentUser() : TEXT RAISES { Error } =
  (* string name of current user *)
  BEGIN
    WITH str = getCurrentUserWrapper() DO
      IF str = LOOPHOLE(0,char_star) THEN
        RAISE Error
      ELSE
        RETURN M3toC.CopyStoT(str)
      END
    END
  END GetCurrentUser;

PROCEDURE GetHomeDir(user : TEXT) : Pathname.T RAISES { Error } =
  (* home directory of specified user *)
  VAR res : char_star;
  BEGIN
    WITH str = M3toC.CopyTtoS(user) DO
      TRY
        res := getHomeDirWrapper(str)
      FINALLY
        M3toC.FreeCopiedS(str)
      END
    END;
    IF res = LOOPHOLE(0,char_star) THEN
      RAISE Error
    ELSE
      RETURN M3toC.CopyStoT(res)
    END
  END GetHomeDir;

BEGIN END SchemeUnixDeps.
