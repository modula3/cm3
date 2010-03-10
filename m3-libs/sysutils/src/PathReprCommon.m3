(*--------------------------------------------------------------------------*)
MODULE PathReprCommon EXPORTS PathRepr;

IMPORT Pathname, Process, OSError;
IMPORT TextUtils;

(*--------------------------------------------------------------------------*)
PROCEDURE Posix(pn : TEXT) : TEXT =
  BEGIN
    RETURN TextUtils.SubstChar(pn, '\\', '/');
    (* Unconditional substitution actually performs better than this:
    IF Text.FindChar(pn, '\\') > -1  THEN
      RETURN TextUtils.SubstChar(pn, '\\', '/');
    ELSE
      RETURN pn;
    END;
    *)
  END Posix;

(*--------------------------------------------------------------------------*)
PROCEDURE Win32(pn : TEXT) : TEXT =
  BEGIN
    RETURN TextUtils.SubstChar(pn, '/', '\\');
    (* Unconditional substitution actually performs better than this:
    IF Text.FindChar(pn, '/') > -1  THEN
      RETURN TextUtils.SubstChar(pn, '/', '\\');
    ELSE
      RETURN pn;
    END;
    *)
  END Win32;

(*--------------------------------------------------------------------------*)
PROCEDURE Root(pn : TEXT) : TEXT =
  BEGIN
    TRY
      RETURN Pathname.Decompose(pn).getlo();
    EXCEPT
      Pathname.Invalid => RETURN NIL;
    END;
  END Root;

BEGIN
  TRY
    RootDir := Root(Process.GetWorkingDirectory());
  EXCEPT
    OSError.E => RootDir := Pathname.Current;
    (* ignore error and let other subsystems fail if they need this *)
  END;
END PathReprCommon.
