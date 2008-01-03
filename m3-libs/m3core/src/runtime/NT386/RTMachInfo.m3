(* Copyright 1996-2000, Critical Mass, Inc.   All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

UNSAFE MODULE RTMachInfo;

IMPORT RTHooks, RTIO, RTError, RTLinker, RTProcedure, RTProcedureSRC;
IMPORT RTException, WinBase, WinDef;

TYPE
  Frame = UNTRACED REF RECORD saved_fp, saved_pc: ADDRESS; END;

TYPE
  PrintLine = RECORD
    pc   : ADDRESS             := NIL;
    fp   : ADDRESS             := NIL;
    proc : RTProcedure.Proc    := NIL;
    file : RTProcedureSRC.Name := NIL;
    name : RTProcedureSRC.Name := NIL;
  END;

VAR dumping := FALSE;

PROCEDURE DumpStack (pc, xfp: ADDRESS) =
  VAR
    fp        : Frame    := xfp;
    n_skipped : CARDINAL := 0;
    n_found   : CARDINAL := 0;
    hit_bottom: BOOLEAN  := FALSE;
    lines     : ARRAY [0..31] OF PrintLine;
  BEGIN
    IF dumping THEN RETURN; END;
    dumping := TRUE;

    (* scan for the frames that are to be printed *)
    WHILE (n_found < NUMBER (lines)) DO
      WITH z = lines [n_found] DO
        z.pc := pc;
        z.fp := fp;
        IF WinBase.IsBadCodePtr (LOOPHOLE (pc-1, WinDef.FARPROC)) # 0 THEN
          (* ouch.  Bail out *)
          INC (n_found); EXIT;
        END;
        RTProcedureSRC.FromPC (pc-1, z.proc, z.file, z.name);

        IF (z.proc = LOOPHOLE (RTLinker.RunMainBody, ADDRESS)) THEN
          (* bail out, and don't bother reporting this frame. *)
          hit_bottom := TRUE;  EXIT;
        ELSIF (z.proc = LOOPHOLE (RTLinker.InitRuntime, ADDRESS)) THEN
          (* bail out, and don't bother reporting this frame. *)
          hit_bottom := TRUE;  EXIT;
        ELSIF (z.proc = LOOPHOLE (RTHooks.ReportFault, ADDRESS)) THEN
          (* skip this frame and its caller, a compiler-generated stub *)
          n_skipped := n_found+2;
        ELSIF (z.proc = LOOPHOLE (RTHooks.AssertFailed, ADDRESS)) THEN
          n_skipped := n_found+1;    (* skip this frame *)
        ELSIF (z.proc = LOOPHOLE (RTHooks.Raise, ADDRESS)) THEN
          n_skipped := n_found+1;    (* skip this frame *)
        ELSIF (z.proc = LOOPHOLE (RTHooks.ResumeRaise, ADDRESS)) THEN
          n_skipped := n_found+1;    (* skip this frame *)
        ELSIF (z.proc = LOOPHOLE (RTException.Raise, ADDRESS)) THEN
          n_skipped := n_found+1;    (* skip this frame *)
        ELSIF (z.proc = LOOPHOLE (RTException.ResumeRaise, ADDRESS)) THEN
          n_skipped := n_found+1;    (* skip this frame *)
        ELSIF (z.proc = LOOPHOLE (RTError.Msg, ADDRESS)) THEN
          n_skipped := n_found+1;    (* skip this frame *)
        ELSIF (z.proc = LOOPHOLE (RTError.MsgS, ADDRESS)) THEN
          n_skipped := n_found+1;    (* skip this frame *)
        ELSIF (z.proc = LOOPHOLE (RTError.MsgI, ADDRESS)) THEN
          n_skipped := n_found+1;    (* skip this frame *)
        ELSIF (z.proc = LOOPHOLE (RTError.MsgPC, ADDRESS)) THEN
          n_skipped := n_found+1;    (* skip this frame *)
        ELSIF (z.proc = LOOPHOLE (RTError.ReportPC, ADDRESS)) THEN
          n_skipped := n_found+1;    (* skip this frame *)
        END;
      END;
      pc := fp.saved_pc;
      IF WinBase.IsBadReadPtr (fp.saved_fp, BYTESIZE (fp^)) # 0 THEN EXIT; END;
      IF (fp.saved_fp <= fp) OR (fp + 8000 < fp.saved_fp) THEN EXIT; END;
      fp := fp.saved_fp;
      INC (n_found);
    END;

    (* print the frames *)
    IF (n_found > n_skipped) THEN
      RTIO.PutText ("Stack trace:\r\n");
      RTIO.PutText ("   FP         PC      Procedure\r\n");
      RTIO.PutText ("---------  ---------  -------------------------------\r\n");
      IF (n_found > n_skipped+10) THEN
        n_found := n_skipped + 10;
        hit_bottom := FALSE;
      END;
      FOR i := n_skipped TO n_found-1 DO
        DumpFrame (lines[i]);
      END;
      IF NOT hit_bottom THEN
        RTIO.PutText (".........  .........  ... more frames ...\r\n");
      END;
      RTIO.Flush ();
    END;

    dumping := FALSE;
  END DumpStack;

PROCEDURE DumpFrame (READONLY z: PrintLine) =
  VAR offset := LOOPHOLE (z.pc, INTEGER) - LOOPHOLE (z.proc, INTEGER);
  BEGIN
    RTIO.PutAddr (z.fp, 9);  RTIO.PutText ("  ");
    RTIO.PutAddr (z.pc, 9);  RTIO.PutText ("  ");
    IF (0 <= offset) AND (offset < 8192) THEN
      IF (z.name # NIL) THEN
        RTIO.PutString (z.name);
        IF (offset # 0) THEN
          RTIO.PutText (" + ");
          RTIO.PutHex  (offset);
          IF (offset > 4096) THEN RTIO.PutText ("(!)"); END;
        END;
      END;
      IF (z.file # NIL) THEN
        RTIO.PutText (" in ");
        RTIO.PutString (z.file);
      END;
    ELSE
      RTIO.PutText ("<???>");
    END;
    RTIO.PutText ("\r\n");
  END DumpFrame;

(**********
PROCEDURE DumpStack (pc, xfp: ADDRESS) =
  VAR fp: Frame := xfp;  cnt := 0;
  BEGIN
    IF dumping THEN RETURN; END;
    dumping := TRUE;
    RTIO.PutText ("Stack trace:\r\n");
    RTIO.PutText ("   FP         PC      Procedure\r\n");
    RTIO.PutText ("---------  ---------\r\n");
    WHILE DumpFrame (pc, fp) DO
      INC (cnt);
      IF (cnt >= 25) THEN
        RTIO.PutText ("... and then some more ...\r\n");
        EXIT;
      END;
      pc := fp.saved_pc;
      IF WinBase.IsBadReadPtr (fp.saved_fp, BYTESIZE (fp^)) # 0 THEN EXIT; END;
      IF (fp.saved_fp <= fp) OR (fp + 2000 < fp.saved_fp) THEN EXIT; END;
      fp := fp.saved_fp;
    END;
    RTIO.Flush ();
    dumping := FALSE;
  END DumpStack;

PROCEDURE DumpFrame (pc, fp: ADDRESS): BOOLEAN =
  VAR
    proc: RTProcedure.Proc;
    file, name: RTProcedureSRC.Name;
    offset: INTEGER;
  BEGIN
    RTIO.PutAddr (fp, 9);  RTIO.PutText ("  ");
    RTIO.PutAddr (pc, 9);  RTIO.PutText ("  ");
    RTProcedureSRC.FromPC (pc, proc, file, name);
    offset := LOOPHOLE (pc, INTEGER) - LOOPHOLE (proc, INTEGER);
    IF (0 <= offset) AND (offset < 2048) THEN
      IF (name # NIL) THEN
        RTIO.PutString (name);
        IF (offset # 0) THEN
          RTIO.PutText (" + ");
          RTIO.PutHex  (offset);
        END;
      END;
      IF (file # NIL) THEN
        RTIO.PutText (" in ");
        RTIO.PutString (file);
      END;
    ELSE
      RTIO.PutText ("<???>");
    END;
    RTIO.PutText ("\r\n");
    RTIO.Flush ();
    RETURN (proc # LOOPHOLE (RTLinker.RunProgram, ADDRESS));
  END DumpFrame;
*********)

BEGIN
END RTMachInfo.
