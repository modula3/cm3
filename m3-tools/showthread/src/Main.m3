(* Copyright (C) 1992, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* Last modified on Tue Jan 31 10:48:26 PST 1995 by kalsow   *)
(*      modified on Mon Mar 09 14:12:19 PST 1992 by muller   *)

UNSAFE MODULE Main;

IMPORT PaintOp, Rect, VBT, Trestle, Region, Axis, ButtonVBT, HVSplit, Split;
IMPORT TextVBT, RigidVBT;
IMPORT Process, ThreadF, Text, Stdio, Thread, Rd, ThreadEvent, Wr;

<*FATAL ANY*>

(*------------------------------------------------------------------- VBT ---*)

TYPE 
  PatchVBT = VBT.Leaf OBJECT
               color: PaintOp.T;
             OVERRIDES
               repaint := RepaintPatch; END;

PROCEDURE RepaintPatch (self: PatchVBT; READONLY rgn: Region.T) = 
  BEGIN
    VBT.PaintTint (self, rgn.r, self.color);
  END RepaintPatch;

CONST 
  names = ARRAY ThreadF.State OF Text.T {
            "alive", "waiting", "locking", "pausing", "blocking",
            "dying", "dead" };

VAR roo, control: VBT.T;
    map: ThreadMapVBT;
    mapRec: Rect.T;

VAR
  tints: ARRAY ThreadF.State OF PaintOp.T;
  backgroundTint := PaintOp.FromRGB (0.9, 0.9, 0.9);

  light := PaintOp.FromRGB (1.0, 1.0, 1.0);

  green :=     PaintOp.FromRGB (  0.0/255.0, 250.0/255.0,   0.0/255.0);
  slateblue := PaintOp.FromRGB (  0.0/255.0, 127.0/255.0, 255.0/255.0);
  magenta   := PaintOp.FromRGB (255.0/255.0,   0.0/255.0, 255.0/255.0);

  lightgreen := PaintOp.FromRGB (138.0/255.0, 255.0/255.0, 138.0/255.5);
  lightblue  := PaintOp.FromRGB (191.0/255.0, 216.0/255.0, 230.0/255.0);
  plum  :=      PaintOp.FromRGB (234.0/255.0, 173.0/255.0, 234.0/255.0);

  red  :=      PaintOp.FromRGB (255.0/255.0,    0.0/255.0,   0.0/255.0);


TYPE 
  ThreadMapVBT = VBT.Leaf OBJECT
               OVERRIDES
                 repaint := RepaintThreadMap;
                 reshape := ReshapeThreadMap;
                 shape   := ShapeThreadMap; END;

PROCEDURE RepaintThreadMap (<*UNUSED*> self: ThreadMapVBT; 
			  <*UNUSED*> READONLY rgn: Region.T) = 
  BEGIN
    VBT.PaintTint (map, mapRec, backgroundTint);
  END RepaintThreadMap;

PROCEDURE ReshapeThreadMap (<*UNUSED*> self: ThreadMapVBT; 
			  READONLY cd: VBT.ReshapeRec) = 
  BEGIN
    mapRec := cd.new;
    VBT.PaintTint (map, mapRec, backgroundTint);
  END ReshapeThreadMap;


PROCEDURE ShapeThreadMap (<*UNUSED*> self: ThreadMapVBT; 
                        ax: Axis.T;
                        <*UNUSED*> n: CARDINAL): 
    VBT.SizeRange =
  BEGIN
    IF ax = Axis.T.Hor THEN
      RETURN (VBT.SizeRange {lo := 200, pref := 300, hi := 100*1000});
    ELSE
      RETURN (VBT.SizeRange {lo := 400, pref := 600, hi := 100*1000}); END;
  END ShapeThreadMap;


PROCEDURE AwaitDelete (<*UNUSED*> self: Thread.Closure): REFANY RAISES {} = 
  BEGIN
    Trestle.AwaitDelete (roo);
    RETURN NIL;
  END AwaitDelete;

VAR
  trestleThread: Thread.T;

PROCEDURE StartAction (<*UNUSED*> self: ButtonVBT.T; 
		      <*UNUSED*> READONLY cd: VBT.MouseRec) =
  BEGIN
    (* Tell the program that we are ready to accept things *)
    Wr.PutChar (Stdio.stdout, 'g');
    Wr.Flush (Stdio.stdout);
    ShowEvent ();
  END StartAction;


PROCEDURE QuitAction (<*UNUSED*> self: ButtonVBT.T; 
		      <*UNUSED*> READONLY cd: VBT.MouseRec) =
  BEGIN
    Trestle.Delete (roo);
    Process.Exit (0);
  END QuitAction;


PROCEDURE LegendVBT (name: Text.T; color: PaintOp.T): VBT.T =
  BEGIN
    RETURN HVSplit.Cons (Axis.T.Hor,
             RigidVBT.New (NEW (PatchVBT, color := color),
                           RigidVBT.Shape { 
                              RigidVBT.SizeRange {lo := 10.0, pref := 10.0, 
                                                  hi := 10.0},
                              RigidVBT.SizeRange {lo := 0.0,  pref := 2.0,
                                                  hi := 4.0}}),
             TextVBT.New (name, 0.0));
  END LegendVBT;

PROCEDURE SetupVBT () =
  BEGIN
    tints := ARRAY ThreadF.State OF PaintOp.T {
               green, slateblue, magenta, lightgreen,
               lightblue, plum, light};

    map := NEW (ThreadMapVBT);

    control := HVSplit.New (Axis.T.Ver);
    Split.AddChild (control,
                    ButtonVBT.New (TextVBT.New ("start"), StartAction),
                    ButtonVBT.New (TextVBT.New ("quit"), QuitAction));

    Split.AddChild (control, LegendVBT ("running", red));
    FOR i := FIRST (ThreadF.State) TO LAST (ThreadF.State) DO
      Split.AddChild (control, LegendVBT (names [i], tints [i])); END;
   
    roo := HVSplit.New (Axis.T.Hor);
    Split.AddChild (roo, control, map);

    Trestle.Install (roo);

    trestleThread := Thread.Fork (NEW (Thread.SizedClosure, 
                         stackSize := 10000,
                         apply := AwaitDelete));

  END SetupVBT;



CONST 
  StepSize = 10;
  ThreadSep = 10;
  ThreadSize = 5;

VAR
  top := 0;

  slot2state := NEW (REF ARRAY OF ThreadF.State, 1);
  thread2slot := NEW (REF ARRAY OF INTEGER, 1);
 
PROCEDURE ShowEvent () =
  VAR t: PaintOp.T;
  BEGIN
    (* erase the line below the one we are going to paint *)
    VBT.PaintTint (map, Rect.T {mapRec.west,
                                mapRec.east, 
                                top + StepSize + mapRec.north, 
                                top + 2 * StepSize + mapRec.north},
                   light);

    (* paint the line for that round *)
    FOR i := FIRST (slot2state^) TO LAST (slot2state^) DO 
      IF i = active THEN 
        t := red; 
      ELSE
        t := tints [slot2state [i]]; END;
      VBT.PaintTint (map, Rect.T {i * ThreadSep + mapRec.west, 
                                  i * ThreadSep + ThreadSize + mapRec.west, 
                                  top + mapRec.north,
                                  top + StepSize + mapRec.north}, t); END;

    INC (top, StepSize);
    IF top + 2 * StepSize > mapRec.south - mapRec.north THEN 
      top := 0; END;
  
  END ShowEvent;


(*---------------------------------------------------------------------------*)

TYPE
  Evt = ThreadEvent.T;

CONST
  EvtSize = (BITSIZE (Evt) + BITSIZE (CHAR) - 1) DIV BITSIZE (CHAR);

TYPE 
  EvtChars = ARRAY [0..EvtSize-1] OF CHAR;

PROCEDURE GetEvent (): Evt =
  VAR e: Evt;
  BEGIN
    EVAL Rd.GetSub (Stdio.stdin, LOOPHOLE (e, EvtChars));
    RETURN e;
  END GetEvent;

(*---------------------------------------------------------------------------*)

VAR 
    e: ThreadEvent.T;
    active: INTEGER;
    allocated : BOOLEAN;

BEGIN
  SetupVBT ();

  thread2slot [0] := 0;
  slot2state [0] := ThreadF.State.alive;

  LOOP 
    e := GetEvent ();
 
    (* have we seen that thread yet ? *)
    IF e.id # -1 AND (e.id > LAST(thread2slot^) OR thread2slot[e.id] = -1) THEN
      (* no. make space FOR it IN the translation table *)
      WHILE e.id > LAST (thread2slot^) DO
        VAR newThread2slot := 
            NEW (REF ARRAY OF INTEGER, NUMBER (thread2slot^)*2);
        BEGIN
          SUBARRAY (newThread2slot^, 0, NUMBER (thread2slot^)) := thread2slot^;
          FOR i := LAST (thread2slot^) + 1 TO LAST (newThread2slot^) DO
            newThread2slot [i] := -1;
          END;
          thread2slot := newThread2slot;
        END;
      END;
      
      (* find a slot for it *)
      allocated := FALSE;
      FOR i := FIRST (slot2state^) TO LAST (slot2state^) DO
        IF slot2state [i] = ThreadF.State.dead THEN
          (* here is one *)
          thread2slot [e.id] := i; 
          allocated := TRUE; 
          EXIT;
        END;
      END;
      IF NOT allocated THEN
        VAR newSlot2state :=
            NEW (REF ARRAY OF ThreadF.State, 2 * NUMBER (slot2state^));
        BEGIN
          SUBARRAY (newSlot2state^, 0, NUMBER (slot2state^)) := slot2state^;
          FOR i := LAST (slot2state^) + 1 TO LAST (newSlot2state^) DO
            newSlot2state [i] := ThreadF.State.dead;
          END;
          thread2slot [e.id] := LAST (slot2state^) + 1;
          slot2state := newSlot2state;
        END;
      END;
    END;

    CASE e.kind OF
    | ThreadEvent.Kind.Changed =>
           slot2state [thread2slot [e.id]] := e.state;
    | ThreadEvent.Kind.Running =>
           IF e.id = -1
             THEN active := -1;
             ELSE active := thread2slot [e.id];
           END;
           ShowEvent();
    | ThreadEvent.Kind.Deleted =>
           slot2state [thread2slot [e.id]] := ThreadF.State.dead;
    END;
    
  END; (*LOOP*)

  (* EVAL Thread.Join (trestleThread); *)
END Main.

