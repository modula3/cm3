(* Copyright (C) 1992, Digital Equipment Corporation          *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(* Last modified on Tue Jan 31 10:40:56 PST 1995 by kalsow    *)
(*      modified on Mon Dec 14 22:42:02 PST 1992 by jdd       *)
(*      modified on Thu Apr 02 19:17:19 PST 1992 by muller    *)

UNSAFE MODULE Main;

IMPORT Thread, FileRd, Params, Stdio;
IMPORT Text, Process, PerfTool;
IMPORT RTHeapEvent, Wr, Rd, Fmt;

IMPORT VBT, TextVBT, ButtonVBT, (*RigidVBT, *) HVSplit;
IMPORT Trestle, (* Region, PaintOp, *) Axis, Split;

FROM Stdio IMPORT stdout;

<*FATAL ANY*>

(*---------------------------------------------------------- various VBTs ---*)

(*
TYPE 
  PatchVBT = VBT.Leaf OBJECT
               color: PaintOp.T;
             OVERRIDES
               repaint := RepaintPatch; END;

PROCEDURE RepaintPatch (self: PatchVBT; READONLY rgn: Region.T) = 
  BEGIN
    VBT.PaintTint (self, rgn.r, self.color);
  END RepaintPatch;

PROCEDURE NewPatchVBT (color: PaintOp.T): VBT.T =
  BEGIN
    RETURN RigidVBT.New (NEW (PatchVBT, color := color),
                         RigidVBT.Shape { 
                            RigidVBT.SizeRange {lo := 5.0, pref := 5.0, 
                                                hi := 5.0},
                            RigidVBT.SizeRange {lo := 0.0,  pref := 2.0,
                                                  hi := 1.0e6}});
  END NewPatchVBT;

PROCEDURE ColorLegendVBT (name: Text.T; 
                          c1, c2, c3, c4: PaintOp.T; value: VBT.T): VBT.T =
  BEGIN
    RETURN HVSplit.Cons (Axis.T.Hor,
             NewPatchVBT (c1), NewPatchVBT (c2), 
             NewPatchVBT (c3), NewPatchVBT (c4), 
             TextVBT.New (name, 0.0), value);
  END ColorLegendVBT;


PROCEDURE ShowValueVBT (name: Text.T; value: VBT.T): VBT.T =
  BEGIN
    RETURN HVSplit.Cons (Axis.T.Hor,
                         TextVBT.New (name, 0.0),
                         value);
  END ShowValueVBT;


PROCEDURE ControlValueVBT (name: Text.T; valu: VBT.T; 
                           less, more: ButtonVBT.Proc; 
                           right: Text.T := NIL): VBT.T =
  VAR res: VBT.T;
  BEGIN
    res := HVSplit.Cons (Axis.T.Hor,
                         ButtonVBT.New (TextVBT.New ("-"), less, valu),
                         TextVBT.New (name, 0.0),
                         valu);
    IF right # NIL THEN
      Split.AddChild (res, TextVBT.New (right)); END;
    Split.AddChild (res, ButtonVBT.New (TextVBT.New ("+"), more, valu));
    RETURN res;
  END ControlValueVBT;
*)

TYPE
  A = REF RECORD p: PROCEDURE (); END;

PROCEDURE ActionVBT (name: Text.T; action: PROCEDURE ()): VBT.T = 
  BEGIN
    RETURN ButtonVBT.New (TextVBT.New (name, halign := 0.0), 
                          DoActionVBT, NEW (A, p := action));
  END ActionVBT;

PROCEDURE DoActionVBT (self: ButtonVBT.T;
                           <*UNUSED*> READONLY cd: VBT.MouseRec) =
  BEGIN
    NARROW (VBT.GetProp (self, TYPECODE (A)), A).p ();
  END DoActionVBT;

(*---------------------------------------------------------------------------*)

TYPE
  Evt = RTHeapEvent.T;

CONST
  EvtSize = (BITSIZE (Evt) + BITSIZE (CHAR) - 1) DIV BITSIZE (CHAR);

TYPE 
  EvtChars = ARRAY [0..EvtSize-1] OF CHAR;

PROCEDURE PutEvent (wr: Wr.T;  READONLY e: Evt) =
  BEGIN
    Wr.PutString (wr, LOOPHOLE (e, EvtChars));
  END PutEvent;

PROCEDURE GetEvent (rd: Rd.T): Evt =
  VAR e: Evt;
  BEGIN
    EVAL Rd.GetSub (rd, LOOPHOLE (e, EvtChars));
    RETURN e;
  END GetEvent;

(*-------------------------------------------------------------- controls ---*)

VAR 
  root: VBT.T;
  trestleThread: Thread.T;

PROCEDURE QuitAction () =
  BEGIN
    Trestle.Delete (root);
    VAR e := RTHeapEvent.T {kind := RTHeapEvent.Kind.Bye}; BEGIN
      PutEvent (wr, e);
      Wr.Flush (wr); END;
    Process.Exit (0);
  END QuitAction;


VAR 
  runUntilList: VBT.T;
  stopOnGrowVBT: VBT.T;

PROCEDURE SetupVBT () =
  BEGIN
    root := HVSplit.New (Axis.T.Ver);
    runUntilList := HVSplit.Cons (Axis.T.Ver,
                      ActionVBT ("just after next page change", AfterChange),
                      ActionVBT ("just after next grow", AfterGrow),
                      ActionVBT ("just after next gc start", AfterGCStart),
                      ActionVBT ("just after next roots scanning", AfterRoots),
                      ActionVBT ("just before next flip", BeforeFlip),
                      ActionVBT ("just after next gc end", AfterGCEnd),
                      ActionVBT ("end", UntilEnd));
    Split.AddChild (root,
                    HVSplit.Cons (Axis.T.Hor,
                                  TextVBT.New ("Run until:"),
                                  runUntilList));



    stopOnGrow := FALSE;
    stopOnGrowVBT := TextVBT.New ("no");
    Split.AddChild (root,
                    HVSplit.Cons (Axis.T.Hor,
                                  ActionVBT ("but stop before grow: ",
                                             ToggleStopOnGrow),
                                  stopOnGrowVBT));
                    
    Split.AddChild (root,
                    ActionVBT ("quit", QuitAction));

    Trestle.Install (root);
  
    trestleThread := Thread.Fork (NEW (Thread.SizedClosure, 
                                  stackSize := 100000,
                                  apply := AwaitDelete));
  END SetupVBT;

PROCEDURE AwaitDelete (<*UNUSED*> self: Thread.Closure): REFANY RAISES {} = 
  BEGIN
    Trestle.AwaitDelete (root);
    RETURN NIL;
  END AwaitDelete;

(*---------------------------------------------------------------------------*)

PROCEDURE AfterChange () =
  BEGIN
    stopBefore := RTHeapEvent.Kind.Bye;
    stopAfter  := RTHeapEvent.Kind.Change;
    Thread.Signal (go);
  END AfterChange;

PROCEDURE AfterGrow () =
  BEGIN
    stopBefore := RTHeapEvent.Kind.Bye;
    stopAfter := RTHeapEvent.Kind.Grow;
    Thread.Signal (go);
  END AfterGrow;

PROCEDURE AfterGCStart () =
  BEGIN
    stopBefore := RTHeapEvent.Kind.Bye;
    stopAfter := RTHeapEvent.Kind.Begin;
    Thread.Signal (go);
  END AfterGCStart;

PROCEDURE AfterRoots () =
  BEGIN
    stopBefore := RTHeapEvent.Kind.Bye;
    stopAfter := RTHeapEvent.Kind.Roots;
    Thread.Signal (go);
  END AfterRoots;

PROCEDURE BeforeFlip () =
  BEGIN
    stopBefore := RTHeapEvent.Kind.Flip;
    stopAfter := RTHeapEvent.Kind.Bye;
    Thread.Signal (go);
  END BeforeFlip;

PROCEDURE AfterGCEnd () =
  BEGIN
    stopBefore := RTHeapEvent.Kind.Bye;
    stopAfter := RTHeapEvent.Kind.End;
    Thread.Signal (go);
  END AfterGCEnd;

PROCEDURE UntilEnd () =
  BEGIN
    stopBefore := RTHeapEvent.Kind.Bye;
    stopAfter := RTHeapEvent.Kind.Bye;
    Thread.Signal (go);
  END UntilEnd;

PROCEDURE ToggleStopOnGrow () =
  CONST
    v = ARRAY BOOLEAN OF Text.T { "no", "yes"};
  BEGIN
    stopOnGrow := NOT stopOnGrow;
    TextVBT.Put (stopOnGrowVBT, v [stopOnGrow]);
  END ToggleStopOnGrow;

(*---------------------------------------------------------------------------*)

PROCEDURE Print (e: RTHeapEvent.T) =
  CONST
    names = ARRAY RTHeapEvent.Kind OF Text.T {
                "gc begin", "promote roots", "flip",
                "gc end", "grow", "change", "bye",
                "prohibited", "now", "enable", "disable"};

  BEGIN
    Wr.PutText (stdout, Fmt.F ("%s: %s, %s\n",
                          names [e.kind], Fmt.Int (e.first), Fmt.Int (e.nb)));
    Wr.Flush (stdout);
  END Print;

PROCEDURE Usage () =
  BEGIN
    Wr.PutText (Stdio.stderr, "usage: replayheap [-showheap prog] datafile\n");
    Process.Exit (1);
  END Usage;


VAR mu := NEW (Thread.Mutex);
    go := NEW (Thread.Condition);
    stopBefore, stopAfter: RTHeapEvent.Kind;
    stopOnGrow: BOOLEAN;
    from: Rd.T;
    showheap: Text.T;
    rd: Rd.T;
    wr: Wr.T;
    debug := FALSE;

BEGIN
  SetupVBT ();

  showheap := "showheap";
  from := NIL;

  VAR i := 1;  arg: TEXT;  BEGIN
    WHILE (i < Params.Count) DO
      arg := Params.Get (i);  INC (i);
      IF Text.Equal (arg, "-showheap") THEN
        IF (i < Params.Count)
          THEN showheap := Params.Get (i);  INC (i);
          ELSE Usage ();
        END;
      ELSIF (from = NIL) THEN
        from := FileRd.Open (arg);
      ELSE
        Usage ();
      END;
    END;
  END;
  IF (from = NIL) THEN Usage () END;

  VAR b := PerfTool.Start (showheap, rd, wr); BEGIN
    <* ASSERT b *> END;

  LOCK mu DO
  
    Thread.Wait (mu, go);

    LOOP
      VAR e := GetEvent (from);
      BEGIN
        IF debug THEN Print (e); END;

        IF e.kind = stopBefore 
          OR e.kind = RTHeapEvent.Kind.Grow AND stopOnGrow THEN
          Thread.Wait (mu, go);
        END;

        IF e.kind # RTHeapEvent.Kind.Bye THEN
          PutEvent (wr, e);
          Wr.Flush (wr);
        END;

        IF e.kind = stopAfter THEN
          Thread.Wait (mu, go);
        END;
      END;
    END;
  END;

  (* EVAL Thread.Join (trestleThread); *)
END Main.



