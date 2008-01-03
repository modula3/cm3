(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* OpSys.m3 -- OS dependent stuff -- Unix version *)
(* Last modified on Wed Feb  1 09:34:09 PST 1995 by kalsow *)
(*      modified on Fri May  7 11:08:35 PDT 1993 by wobber *)

UNSAFE MODULE OpSysPosix EXPORTS OpSys;

IMPORT M3toC, Unix, Upwd, Uugid, Ustat;
FROM Ctypes IMPORT int, char_star;

CONST
  rwMask = Ustat.S_GWRITE + Ustat.S_OWRITE;

PROCEDURE Init () =
  BEGIN
        (* set process umask *)
    EVAL Unix.umask(rwMask);
    (*
    sigvecrec.mask := AsyncMask;
    sigvecrec.flags := UxTypes.SVFlags{};
    sigvecrec.handler.h := UxTypes.SignalDefault;
    EVAL Unix.sigvec(SigQuit, sigvecrec, ovec);
    *)
  END Init;

PROCEDURE GetHostName (): TEXT RAISES {Error} =
  VAR
    buffer: ARRAY [0..255] OF CHAR;
    nbytes: int;
  BEGIN
    nbytes := Unix.gethostname(ADR(buffer), BYTESIZE(buffer)-1);
    IF nbytes < 0 THEN RAISE Error; END;
    RETURN M3toC.CopyStoT(ADR(buffer));
  END GetHostName;

PROCEDURE GetUser (): TEXT RAISES {Error} =
  VAR
    uid: int;
    pw: Upwd.struct_passwd_star;
  BEGIN
    uid := Uugid.getuid();
    pw := Upwd.getpwuid(uid);
    IF pw = NIL THEN RAISE Error; END;
    RETURN M3toC.CopyStoT(pw^.pw_name);
  END GetUser;

PROCEDURE SetUser (who: TEXT) RAISES {Error} =
  VAR
    p: char_star;
    uid: int;
    pw: Upwd.struct_passwd_star;
  BEGIN
    p := M3toC.TtoS(who);
    pw := Upwd.getpwnam(p);
    IF pw = NIL THEN RAISE Error; END;
    uid := Uugid.getuid();
    IF uid # pw^.pw_uid THEN
      IF Uugid.setreuid(pw^.pw_uid, -1) < 0 THEN RAISE Error; END;
    END;
    EVAL Uugid.setreuid(-1, pw^.pw_uid);
  END SetUser;

BEGIN
END OpSysPosix.

