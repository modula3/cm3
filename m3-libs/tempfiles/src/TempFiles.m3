(* Copyright (C) 1994, Digital Equipment Corporation          *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(* Last modified on Tue Nov  1 10:05:01 PST 1994 by kalsow    *)
(*      modified on Wed Sep  7 14:48:10 PDT 1994 by birrell   *)

MODULE TempFiles EXPORTS TempFiles;

IMPORT Fmt, FS, OSError, Pathname, Process, Stdio,
       Text, TextList, Thread, Time, Wr;

VAR mu := NEW(Thread.Mutex);


(* *)
(* temp file names *)
(* *)

VAR tempFilePrefix: Pathname.T := NIL;
  (* Prefix (i.e. directory) for temp file names *)

PROCEDURE DefaultPrefix(prefix: Pathname.T) =
  BEGIN
    LOCK mu DO tempFilePrefix := prefix END;
  END DefaultPrefix;

PROCEDURE Get(prefix: TEXT := NIL; part: TEXT := ""; ext: TEXT := NIL): TEXT =
  BEGIN
    LOCK mu DO
      IF prefix = NIL THEN
        IF tempFilePrefix = NIL THEN
          tempFilePrefix := Pathname.Prefix("/tmp/foo");
        END;
        prefix := tempFilePrefix;
      END;
      RETURN Pathname.Join(prefix,
                           Text.Cat(part, Fmt.LongReal(Time.Now())),
                           ext);
    END;
  END Get;


(* *)
(* Temp file list *)
(* *)

VAR tempFileList: TextList.T := NIL;
  (* List of files to be deleted at process exit *)

PROCEDURE InitTempFiles() =
  (* Ensure that we will clean up temp files on process exit *)
  BEGIN
    Process.RegisterExitor(Cleanup);
  END InitTempFiles;

PROCEDURE Note(t: TEXT) =
  (* Add "t" to the list of files to be deleted at process exit *)
  BEGIN
    LOCK mu DO
      tempFileList := NEW(TextList.T, head := t, tail := tempFileList);
    END;
  END Note;

PROCEDURE Forget(t: TEXT) =
  (* Remove "t" from the list of files to be deleted at process exit *)
  VAR
    this, prev: TextList.T := NIL;
  BEGIN
    LOCK mu DO
      this := tempFileList;
      WHILE this # NIL AND NOT Text.Equal(this.head, t) DO
         prev := this; this := this.tail;
     END;
      IF this # NIL THEN
        IF prev = NIL THEN
          tempFileList := this.tail
        ELSE
          prev.tail := this.tail;
        END;
      END;
    END;
  END Forget;

PROCEDURE Cleanup() =
  (* Called at process exit, from within "Process" module.  Deletes files
     recorded by "NoteTempFile". *)
  (* DON'T lock "mu" - it might be held!  Rely on atomic REF assignments. *)
  BEGIN
    WHILE tempFileList # NIL DO
      VAR t: TEXT := tempFileList.head;
      BEGIN
        TRY
          tempFileList := tempFileList.tail;
          FS.DeleteFile(t);
        EXCEPT OSError.E =>
          TRY
            Wr.PutText(Stdio.stderr,
                       "Warning: failed to delete \"" & t & "\"\n");
          EXCEPT Wr.Failure, Thread.Alerted =>
          END;
        END;
      END;
    END;
  END Cleanup;

BEGIN
  InitTempFiles();
END TempFiles.
