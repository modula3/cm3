(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Tue Dec 15 13:50:16 PST 1998 by heydon     *)
(*      modified on Tue Dec 15 12:00:00 EST 1998 by rcoleburn  *)
(*      modified on Mon Nov  4 13:10:08 PST 1996 by najork     *)
(*      modified on Mon Jan 30 15:15:24 PST 1995 by kalsow     *)
(*      modified on Sun Aug 28 10:13:28 PDT 1994 by mhb        *)
(*      modified on Fri Jul  9 11:01:44 PDT 1993 by wobber     *)
(*      modified on Fri Jun 11 21:09:52 PDT 1993 by meehan     *)
(*      modified on Tue Jun 16 13:08:05 PDT 1992 by muller     *)
(*      modified on Thu Apr  4 15:55:03 PST 1991 by brooks     *)
(*      modified on Thu May 17  9:54:12 PDT 1990 by mcjones    *)
<* PRAGMA LL *>

(* 12/15/1998:  Summary of Patches by R. C. Coleburn

   I was having trouble with calls to ClearHistory getting a checked
   runtime error on the DEC() call.  After looking at the code, it
   seems that the typeinStart and outputEnd fields are not being 
   properly protected to ensure they are updated/used in sync with
   one another in a multithreaded environment.  I patched the code
   to move lines that modified typeinStart to be inside the lock of
   v.mu that is being held for modifications of the outputEnd field.
*)

MODULE TypescriptVBT;

(** Here's how the typescript works:

    The vtext holds the underlying text.
    0 <= outputEnd <= tp.typeinStart <= len(vtext).

    vtext[0 .. outputEnd-1] is the "history".  It is accessible to
    neither the reader nor the writer.  ClearHistory "erases" this,
    i.e., deletes that section of the vtext and decrements outputEnd
    and tp.typeinStart accordingly.

    Wr.Flush inserts characters at outputEnd, in the "middle" of the
    vtext.  After the insertion, outputEnd and tp.typeinStart are
    incremented by the number of inserted characters.

    vtext[outputEnd .. tp.typeinStart-1] is the section that's accessible
    to the reader.  RSeek copies characters from this part of the vtext.
    If outputEnd = tp.typeinStart (i.e., if there are no characters
    available) and dontBlock is false, then RSeek calls Wr.Flush and
    waits for inputReady to be signaled.

    vtext[typeinStart .. len(vtext) - 1] contains typed-in characters.
    That is, keyboard input is appended to the end of the vtext.  This
    segment is editable.  When Return is typed, a Newline is appended,
    tp.typeinStart is set to len(vtext), and inputReady is signaled, thus
    making the input line accessible to the reader.

**)

IMPORT RdClass, Text, TextEditVBT, TextPort, TextPortClass, Thread, VBT,
       Wr, WrClass;

REVEAL
  T = Public BRANDED OBJECT
        mu: MUTEX;
        <* LL = mu *>
        rd        : Reader;
        wr        : Writer;
        lastReader: Thread.T;          (* whom to alert on ^C *)
        inputReady: Thread.Condition;
        terminated: BOOLEAN;
        outputEnd : CARDINAL;
      OVERRIDES
        init            := Init;
        interrupt       := Interrupt;
        handleInterrupt := HandleInterrupt;
        setThread       := SetThread;
        terminate       := Terminate;
      END;
  Reader = PublicReader BRANDED "Typescript.Reader" OBJECT
             v: T
           OVERRIDES
             seek       := RSeek;
             typescript := RdTypescript
           END;
  Writer = PublicWriter BRANDED OBJECT
             v: T
           OVERRIDES
             seek       := WSeek;
             flush      := WFlush;
             typescript := WrTypescript
           END;

REVEAL
  Port = TextPort.T BRANDED OBJECT
           v: T
         OVERRIDES
           returnAction := ReturnAction;
           setReadOnly  := SetReadOnly
         END;

EXCEPTION Error;

PROCEDURE Init (v: T; scrollable := TRUE): T =
  <* FATAL Error *>
  CONST
    TerminalReaderBuffSize = 4096;
    TerminalWriterBuffSize = 4096;
  BEGIN
    v := TextEditVBT.T.init (v, scrollable);
    v.inputReady := NEW (Thread.Condition);
    v.rd := NEW (Reader, v := v, lo := 0, cur := 0, hi := 0, st := 0,
                 buff := NEW (REF ARRAY OF CHAR, TerminalReaderBuffSize),
                 closed := FALSE, seekable := FALSE, intermittent := TRUE);
    v.wr :=
      NEW (Writer, v := v, lo := 0, cur := 0, hi := TerminalWriterBuffSize,
           st := 0, buff := NEW (REF ARRAY OF CHAR, TerminalWriterBuffSize),
           closed := FALSE, seekable := FALSE, buffered := TRUE);
    v.lastReader := NIL;
    v.terminated := FALSE;
    v.outputEnd := 0;
    v.mu := NEW (MUTEX);
    TYPECASE v.tp OF | NULL => | Port (p) => p.v := v; RETURN v ELSE END;
    RAISE Error
  END Init;

(***********************  Typescript-specific code  ***********************)

PROCEDURE WSeek (wr: Writer; <* UNUSED *> n: CARDINAL)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    wr.flush ()
  END WSeek;

PROCEDURE WFlush (wr: Writer) RAISES {Thread.Alerted} =
  VAR
    v      := wr.v; tp := v.tp;
    normP  := TextPort.IsVisible (v.tp, TextPort.Index (tp));
    nchars := wr.cur - wr.lo;
  BEGIN
    LOCK v.mu DO
      TextPort.Replace (tp, v.outputEnd, v.outputEnd,
                        Text.FromChars (SUBARRAY (wr.buff^, 0, nchars)));
      INC (v.outputEnd, nchars);
      INC (tp.typeinStart, nchars); (* moved this line to be inside the LOCK -RCC *)
    END;
    wr.lo := wr.cur;
    wr.hi := wr.lo + NUMBER (wr.buff^);
    IF normP THEN TextPort.Normalize (tp) ELSE VBT.Mark (tp) END;
    IF Thread.TestAlert () THEN RAISE Thread.Alerted END
  END WFlush;

PROCEDURE RSeek (rd: Reader; <*UNUSED*> n: CARDINAL;
                 dontBlock: BOOLEAN): RdClass.SeekResult
  RAISES {Thread.Alerted} =
  VAR
    nchars: CARDINAL;
    v                := rd.v;
  BEGIN
    LOCK v.mu DO
      v.lastReader := Thread.Self ();
      nchars := v.tp.typeinStart - v.outputEnd;
      IF nchars > 0 THEN
      ELSIF v.terminated THEN
        rd.buff := NIL;
        RETURN RdClass.SeekResult.Eof
      ELSIF dontBlock THEN
        RETURN RdClass.SeekResult.WouldBlock
      ELSE
        REPEAT
          Thread.Release (v.mu);
          TRY
            TRY Wr.Flush (v.wr) EXCEPT Wr.Failure => END
          FINALLY
            Thread.Acquire (v.mu)
          END;
          Thread.AlertWait (v.mu, v.inputReady);
          nchars := v.tp.typeinStart - v.outputEnd
        UNTIL nchars > 0
      END;
      WITH n   = MIN (nchars, NUMBER (rd.buff^)),
           txt = TextPort.GetText (v.tp, v.outputEnd, v.outputEnd + n) DO
        Text.SetChars (rd.buff^, txt);
        INC (v.outputEnd, n);
        rd.lo := rd.cur;
        rd.hi := rd.lo + n;      (* NOT v.outputEnd! *)
        RETURN RdClass.SeekResult.Ready
      END                        (* WITH n *)
    END                          (* LOCK *)
  END RSeek;

PROCEDURE ReturnAction (tp: Port; READONLY event: VBT.KeyRec) =
  (* Input action, called when the user presses Return in the input area.
     Unblocks RSeek if it was blocked. *)
  BEGIN
    IF event.modifiers = VBT.Modifiers {} AND NOT tp.getReadOnly () THEN
      TextPort.Seek (tp, TextPort.Length (tp));
      TextPort.Insert (tp, Wr.EOL);
      LOCK tp.v.mu DO
        (* added LOCK -RCC *)
        tp.typeinStart := TextPort.Length (tp)
      END;
      (* activate the reading client *)
      Thread.Signal (tp.v.inputReady);
      TextPort.Normalize (tp)
    END
  END ReturnAction;

PROCEDURE SetReadOnly (<* UNUSED *> tp: Port; <* UNUSED *> flag: BOOLEAN) =
  BEGIN
  END SetReadOnly;

PROCEDURE Interrupt (v: T; time: VBT.TimeStamp) =
  (* Interrupt.  It flushes (ignores) all pending typein, then calls the
     interrupt handler. *)
  VAR length := TextPort.Length (v.tp);
  BEGIN
    TextPort.Seek (v.tp, length);
    TextPort.Insert (v.tp, "^C");
    LOCK v.mu DO (* flush all pending typein *) 
      v.outputEnd := length + 2; 
      v.tp.typeinStart := length + 2; (* moved this line to be inside the LOCK -RCC *)
    END;
    v.handleInterrupt (time)
  END Interrupt;

PROCEDURE HandleInterrupt (v: T; <* UNUSED *> time: VBT.TimeStamp) =
  BEGIN
    LOCK v.mu DO
      IF v.lastReader # NIL THEN Thread.Alert (v.lastReader) END
    END
  END HandleInterrupt;

PROCEDURE GetRd (v: T): Reader =
  BEGIN
    RETURN v.rd
  END GetRd;

PROCEDURE GetWr (v: T): Writer =
  BEGIN
    RETURN v.wr
  END GetWr;

PROCEDURE RdTypescript (r: Reader): T =
  BEGIN 
    RETURN r.v
  END RdTypescript;

PROCEDURE WrTypescript (r: Writer): T =
  BEGIN 
    RETURN r.v
  END WrTypescript;

PROCEDURE GetHistory (v: T): TEXT =
  BEGIN
    LOCK v.mu DO RETURN TextPort.GetText (v.tp, 0, v.outputEnd) END
  END GetHistory;

PROCEDURE ClearHistory (v: T) =
  BEGIN
    LOCK v.mu DO
      TextPort.Replace (v.tp, 0, v.outputEnd, "");
      DEC (v.tp.typeinStart, v.outputEnd);
      v.outputEnd := 0
    END;
    VBT.Mark (v.tp)
  END ClearHistory;


(**************************  Special controls  **************************)

PROCEDURE SetThread (v: T; thread: Thread.T := NIL) =
  BEGIN
    LOCK v.mu DO
      IF thread = NIL THEN
        v.lastReader := Thread.Self ()
      ELSE
        v.lastReader := thread
      END
    END
  END SetThread;

PROCEDURE Terminate (v: T) =
  BEGIN
    LOCK v.mu DO v.terminated := TRUE END;
    v.tp.setReadOnly (TRUE);
    Thread.Signal (v.inputReady)
  END Terminate;

BEGIN
END TypescriptVBT.
