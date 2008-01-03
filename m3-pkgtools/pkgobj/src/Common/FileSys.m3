(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* FileSys.m3 *)
(* Last modified on Thu Sep  2 18:08:26 PDT 1993 by wobber *)

MODULE FileSys;

IMPORT OSError, TextList, Rd, Text, Thread, Time, Wr, WeakRef;
IMPORT FileRd, FileWr, FS, Pathname;

PROCEDURE OpenRead(fn: FN) : Rd.T RAISES {OSError.E} =
  VAR rd: Rd.T;
  BEGIN
    IF fn = NIL THEN fn := ""; END;
    rd := FileRd.Open(fn);
    EVAL WeakRef.FromRef(rd, CloseRd);
    RETURN rd;
  END OpenRead;
  
PROCEDURE OpenWrite(fn: FN) : Wr.T RAISES {OSError.E} =
  VAR wr: Wr.T;
  BEGIN
    IF fn = NIL THEN fn := ""; END;
    wr := FileWr.Open(fn);
    EVAL WeakRef.FromRef(wr, CloseWr);
    RETURN wr;
  END OpenWrite;

PROCEDURE OpenAppend(fn: FN) : Wr.T RAISES {OSError.E} =
  VAR wr: Wr.T;
  BEGIN
    IF fn = NIL THEN fn := ""; END;
    wr := FileWr.OpenAppend(fn);
    EVAL WeakRef.FromRef(wr, CloseWr);
    RETURN wr;
  END OpenAppend;

PROCEDURE CloseRd(<*UNUSED*> READONLY w: WeakRef.T; r: REFANY) =
    <* FATAL Thread.Alerted *>
  BEGIN
    TRY Rd.Close(r); EXCEPT Rd.Failure => END;
  END CloseRd;

PROCEDURE CloseWr(<*UNUSED*> READONLY w: WeakRef.T; r: REFANY) =
    <* FATAL Thread.Alerted *>
  BEGIN
    TRY Wr.Close(r); EXCEPT Wr.Failure => END;
  END CloseWr;

PROCEDURE Enumerate (fn: FN): Enumeration RAISES {OSError.E} =
  VAR
    res, tail, l: TextList.T := NIL;
    t: TEXT;
    i: FS.Iterator;
  BEGIN
    IF fn = NIL THEN fn := ""; END;
    TRY
      i := FS.Iterate(fn);
      WHILE i.next(t) DO
        l := TextList.Cons(t, NIL);
        IF res = NIL THEN
          res := l;
        ELSE
          tail.tail := l;
        END;
        tail := l;
      END;
    FINALLY
      i.close();
    END;
    RETURN res;
  END Enumerate;

PROCEDURE MakeDir (path: FN) RAISES {OSError.E} =
  BEGIN
    FS.CreateDirectory(path);
  END MakeDir;

PROCEDURE SetModifiedDate (path: FN; date: Time.T) RAISES {OSError.E} =
  BEGIN
    FS.SetModificationTime(path, date);
  END SetModifiedDate;

PROCEDURE Rename (source, dest: FN) RAISES {OSError.E} =
  BEGIN
    FS.Rename(source, dest);
  END Rename;

PROCEDURE Remove (path: FN; recursive: BOOLEAN := FALSE)
    RAISES {OSError.E} =
    (* iff recursive, we'll remove entire dir, otherwise the target
       must be a non-directory *)
  VAR waitCtr: CARDINAL;
  CONST WaitInterval = 10;
  PROCEDURE RemoveTreeInternal (thisPath: Text.T) RAISES {OSError.E} =
    VAR
      info: FileInfo;
      eList: TextList.T;
    BEGIN
      DEC (waitCtr);
      IF waitCtr = 0 THEN
        waitCtr := WaitInterval; Thread.Pause (5.0D-2);
      END;
      info := GetInfo(thisPath);
      IF recursive THEN
        IF info.type = FileType.Dir THEN
          EVAL CheckAccess(thisPath, TRUE, TRUE);
          eList := Enumerate(thisPath);
          WHILE eList # NIL DO
            RemoveTreeInternal (Pathname.Join(thisPath, eList.head, NIL));
            eList := eList.tail;
          END;
        END;
      END;
      IF info.type = FileType.Dir THEN
        FS.DeleteDirectory(thisPath);
      ELSE
        FS.DeleteFile(thisPath);
      END;
    END RemoveTreeInternal;
  BEGIN
    waitCtr := WaitInterval;
    IF path = NIL THEN path := ""; END;
    RemoveTreeInternal (path);
  END Remove;

BEGIN
END FileSys.
