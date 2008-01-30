(*--------------------------------------------------------------------------*)
MODULE DirStack;

IMPORT TextSeq, Pathname, Process, OSError;

(*--------------------------------------------------------------------------*)
PROCEDURE PushDir(dir : Pathname.T) RAISES {Error} =
  BEGIN
    dirs.addhi(GetWorkingDir());
    SetWorkingDir(dir);
  END PushDir;

(*--------------------------------------------------------------------------*)
PROCEDURE PopDir() RAISES {Error} =
  BEGIN
    SetWorkingDir(dirs.remhi());
  END PopDir;

(*--------------------------------------------------------------------------*)
PROCEDURE GetWorkingDir() : Pathname.T RAISES {Error} =
  VAR wd : Pathname.T;
  BEGIN
    TRY
      wd := Process.GetWorkingDirectory();
    EXCEPT
      OSError.E => RAISE Error("cannot get working directory");
    END;
    RETURN wd;
  END GetWorkingDir;

(*--------------------------------------------------------------------------*)
PROCEDURE SetWorkingDir(dir : Pathname.T) RAISES {Error} =
  BEGIN
    TRY
      Process.SetWorkingDirectory(dir);
    EXCEPT
      OSError.E => RAISE Error("cannot change directory to " & dir);
    END;
  END SetWorkingDir;

(*--------------------------------------------------------------------------*)
VAR
  dirs := NEW(TextSeq.T).init();
BEGIN
END DirStack.
