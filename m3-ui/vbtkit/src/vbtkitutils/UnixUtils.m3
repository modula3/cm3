(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Feb 18 18:08:57 PST 1994 by kalsow               *)
(*      modified on Thu Aug  6 13:09:10 PDT 1992 by meehan               *)
(*      modified on Tue Jun 30 19:28:16 1992 by mhb                      *)
(*      modified on Mon Jun 22 17:20:26 PDT 1992 by muller                   *)
(*      modified on Fri Mar 27 02:29:44 1992 by steveg                       *)


UNSAFE MODULE UnixUtils;

IMPORT Cerrno, M3toC, TextList, Udir, Unix, Ustat, Word, Uerror;

PROCEDURE Directory (dirname: TEXT): TextList.T RAISES {Error} =
  VAR list: TextList.T := NIL;
  BEGIN
    WITH dir = M3toC.TtoS (dirname), dstream = Udir.opendir (dir) DO
      IF dstream = NIL THEN RaiseError () END;
      LOOP
        WITH entry = Udir.readdir (dstream) DO
          IF entry = NIL THEN
            EVAL Udir.closedir (dstream);
            RETURN TextList.ReverseD (list)
          END;
          TextList.Push (list, M3toC.CopyStoT (ADR (entry.d_name)))
        END                     (* WITH *)
      END                       (* LOOP *)
    END                         (* WITH *)
  END Directory;

PROCEDURE IsDirectory (name: TEXT): BOOLEAN =
  VAR
    string := M3toC.TtoS (name);
    ref    := NEW (Ustat.struct_stat_star);
    val    := Ustat.stat (string, ref);
    mode   := ref.st_mode;
  BEGIN
    DISPOSE (ref);
    RETURN val = 0 AND Word.And (mode, Ustat.S_IFMT) = Ustat.S_IFDIR
  END IsDirectory;

PROCEDURE ProbeFile (path: TEXT; error: BOOLEAN): BOOLEAN RAISES {Error} =
  VAR
    string := M3toC.TtoS (path);
    ref    := NEW (Ustat.struct_stat_star);
    val    := Ustat.stat (string, ref);
  BEGIN
    DISPOSE (ref);
    IF val = 0 THEN
      RETURN TRUE
    ELSIF error THEN
      RaiseError (); 
      <* ASSERT FALSE *>
    ELSE
      RETURN FALSE
    END
  END ProbeFile;

PROCEDURE FileModifyTime (path: TEXT): Seconds =
  VAR
    string := M3toC.TtoS (path);
    ref    := NEW (Ustat.struct_stat_star);
    val    := Ustat.stat (string, ref);
    mtime  := ref.st_mtime;
  BEGIN
    DISPOSE (ref);
    IF val = 0 THEN RETURN mtime ELSE RETURN 0 END
  END FileModifyTime;

PROCEDURE GetWD (): TEXT RAISES {Error} =
  VAR
    pathname := NEW (UNTRACED REF ARRAY [0 .. Unix.MaxPathLen] OF CHAR);
    result   := Unix.getwd (pathname);
  BEGIN
    IF result = NIL THEN
      RaiseError ();
      <* ASSERT FALSE *>
    ELSE
      RETURN M3toC.CopyStoT (pathname)
    END
  END GetWD;

PROCEDURE RaiseError () RAISES {Error} =
  BEGIN
    RAISE Error (M3toC.StoT (Uerror.GetFrom_sys_errlist (Cerrno.errno)))
  END RaiseError;

PROCEDURE Accessible (file: TEXT; modes := SET OF AccessMode {}): BOOLEAN =
  CONST
    R_OK: Word.T = 4;
    W_OK: Word.T = 2;
    X_OK: Word.T = 1;
    F_OK: Word.T = 0;
    bits         = ARRAY AccessMode OF Word.T {X_OK, W_OK, R_OK};
  VAR
    mode   := F_OK;
    string := M3toC.TtoS (file);
  BEGIN
    FOR i := FIRST (AccessMode) TO LAST (AccessMode) DO
      IF i IN modes THEN mode := Word.Or (mode, bits [i]) END
    END;
    RETURN access (string, mode) # -1;
  END Accessible;
  
BEGIN 
END UnixUtils.
