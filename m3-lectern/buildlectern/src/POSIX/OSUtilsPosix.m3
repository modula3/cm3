(* Copyright 1989 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* OSUtils.m3                                                  *)
(* Last modified on Mon May 20 13:04:46 PDT 1996 by mcjones    *)
(*      modified on Mon Jun  6 17:53:51 PDT 1994 by birrell    *)

UNSAFE MODULE OSUtilsPosix EXPORTS OSUtils;

IMPORT File, FilePosix, FileRd, FS, M3toC, OSError, OSErrorPosix, Unix;

(* *)
(* DupRd *)
(* *)

REVEAL DupRd = FileRd.T BRANDED OBJECT
  OVERRIDES
    init := MyFileRdInit;
  END;

PROCEDURE MyFileRdInit(rd: FileRd.T;
                       h: File.T): FileRd.T
                       RAISES { OSError.E } =
  BEGIN
    RETURN FileRd.T.init(rd, FilePosix.New(Unix.dup(h.fd), FilePosix.Read));
  END MyFileRdInit;


(* *)
(* Fifo *)
(* *)

VAR pFifo: TEXT := NIL;

PROCEDURE CreateFifo(p: TEXT) RAISES {OSError.E} =
  CONST Mode = Unix.fifo_special + Unix.MROWNER + Unix.MWOWNER;
  BEGIN
    <* ASSERT pFifo=NIL *>
    IF Unix.mknod(M3toC.TtoS(p), Mode, 0) < 0 THEN
      OSErrorPosix.Raise();
    END;
    pFifo := p;
  END CreateFifo;

PROCEDURE OpenFifo(): File.T RAISES {OSError.E} =
  BEGIN
    <* ASSERT pFifo#NIL *>
    RETURN FS.OpenFileReadonly(pFifo);
  END OpenFifo;

PROCEDURE DeleteFifo() RAISES {OSError.E} =
  BEGIN
    <* ASSERT pFifo#NIL *>
    FS.DeleteFile(pFifo);
    pFifo := NIL;
  END DeleteFifo;

BEGIN
END OSUtilsPosix.

(* *****

    Formerly, FromPS contained code like the following; now that
    we start Ghostscript with the "-dOpenOutputFile" option, it
    no longer seems to be necessary:

    (* Start "cat" process, to avoid blocking on open of fifo. *)
    VAR prCat: Process.T;
    VAR e: INTEGER;
    TRY Pipe.Open(pipe2R, pipe2W)
    EXCEPT OSError.E => RAISE Error("Can't create interprocess pipe")
    END;
    TRY
      t.prCat := Process.Create(
        cmd := "cat",
        params := ARRAY OF TEXT {t.fifoPath},
        stdout := pipe2W,
        stderr := stderr)
    EXCEPT OSError.E => RAISE Error("Can't create helper process")
    END;
    TRY pipe2W.close() EXCEPT OSError.E => END;
    ...
    e := Process.Wait(t.prCat);
    IF e # 0 THEN
      RAISE Error("cat exited with code of " & Fmt.Int(e))
    END;

***** *)
