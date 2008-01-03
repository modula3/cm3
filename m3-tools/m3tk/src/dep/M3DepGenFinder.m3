(************************************************************************
!		                                                        *
!*                                                                      *
!*         Copyright 1994 Sun Microsystems, Inc. All Rights Reserved.   *
!*                                                                      *
!*      Permission to use, copy, modify, and distribute this software   *
!*      and its documentation for any purpose and without fee is hereby *
!*      granted, provided that the above copyright notice appear in all *
!*      copies and that both that copyright notice and this permission  *
!*      notice appear in supporting documentation, and that the name of *
!*      Sun Microsystems, Inc. (SMI) not be used in advertising or      *
!*      publicity pertaining to distribution of the software without    *
!*      specific, written prior permission.                             *
!*                                                                      *
!*                                                                      *
!*      SMI DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,      *
!*      INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY,	        *
!*      FITNESS FOR A PARTICULAR PURPOSE OR NON-INFRINGEMENT.           *
!*      IN NO EVENT SHALL SMI BE LIABLE FOR ANY SPECIAL, INCIDENTAL,    *
!*	INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER     *
!*      RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN      *
!*      ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,        *
!*      ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE     *
!*      OF THIS SOFTWARE.                                               *
!*                                                                      *
!***********************************************************************)

MODULE M3DepGenFinder;
IMPORT File, Rd, Process, FS, FileRd, OSError, RdExtras, ASCII;

IMPORT M3Extension;
IMPORT M3DepFindFile;

(* For FATAL *)
IMPORT Thread;
<*FATAL Rd.EndOfFile, Thread.Alerted*>

PROCEDURE New(overrides := FALSE): M3DepFindFile.T=
  <*FATAL OSError.E*>
  VAR rd: Rd.T; stdout: File.T; 
      ix := 1;
      result: M3DepFindFile.T := NIL;
  CONST
    Arg2 = ARRAY OF TEXT{"srcmap", "-O"};
  BEGIN
    IF overrides THEN
      ix := 2;
    END;
    stdout := FS.OpenFile(".M3SRCMAP");
    TRY
      IF NOT RunProcess("m3build", SUBARRAY(Arg2, 0, ix),
                        xstdout := stdout) THEN RETURN NIL END;
    FINALLY
      stdout.close()
    END;

    TRY
      TRY
        rd := FileRd.Open(".M3SRCMAP");
        (* skip noise output by m3build *)
        EVAL RdExtras.GetText(rd, terminate := ASCII.Set{'@'});
        result := 
          NEW(M3DepFindFile.T).init(M3Extension.All, rd, NIL);
      FINALLY
        Rd.Close(rd);
      END;
    EXCEPT 
    | Rd.Failure =>
    END;
    RETURN result;
  END New;

PROCEDURE RunProcess(cmd: TEXT; READONLY args: ARRAY OF TEXT;
                     xstdin, xstdout, xstderr: File.T := NIL): BOOLEAN=
  VAR stdin, stdout, stderr: File.T;
      p: Process.T; 
  BEGIN
    Process.GetStandardFileHandles(stdin, stdout, stderr);
    IF xstdin # NIL THEN stdin := xstdin END;
    IF xstdout # NIL THEN stdout := xstdout END;
    IF xstderr # NIL THEN stderr := xstderr END;
    TRY
      p := Process.Create(cmd, args, NIL, NIL,
                     stdin, stdout, stderr);
      IF Process.Wait(p) # 0 THEN END;
    EXCEPT
    | OSError.E =>
        RETURN FALSE
    END;
    RETURN TRUE;
  END RunProcess;

BEGIN
END M3DepGenFinder.
