
INTERFACE AnimateVBT;
IMPORT VBT, Time, PaintOp;
IMPORT IntervalTimer;

(* "AnimateVBT" cycles through its children VBTs, 
    waiting for "duration" seconds each time. 

    Animated VBTs belong to a ``group'', so that
    all of the animations can change in a single
    lockstep. 
    
    Each animation must include at least one snapshot. *)

TYPE
  T <: Public;
  Private <: VBT.Leaf;
  Public =  Private OBJECT METHODS 
    init(READONLY snapshots: ARRAY OF VBT.T;
                  group: Group := NIL;
                  bg: PaintOp.T := PaintOp.Bg): T;
    reset(position: CARDINAL := 0);
  END;
  (* The call "init" initializes an animated VBT 
     given a set of snapshots, the animation
     group and the background. The call "reset"
     can be used to reset the animated vbt
     back to a particular location. If "group"
     is "NIL" then the animation gets its own
     unique group. *)
  
  Group <: GroupPublic; 
  GroupPublic = IntervalTimer.T OBJECT METHODS 
    init (duration: Time.T): Group;
    reset (position: CARDINAL := 0);
  END;
  (* Specialized groups may be created so that
     the animations of more than one animated VBT  
     are synchronized. *)
    
PROCEDURE New(READONLY snapshots: ARRAY OF VBT.T; 
              duration: Time.T := DefaultDuration): T;
(* Creates a new animated VBT, each frame of which will
   stay on the screen for at least the specified
   duration. *)

PROCEDURE Blink (ch: VBT.T; bg: PaintOp.T := PaintOp.Bg): T;
(* Blinks a VBT by screen by animating the set containing
   "ch" and a "bg" background. *)

PROCEDURE Stop(v: T);
PROCEDURE Go(v: T);
(* "Stop" and "Go" cause the animation to halt or continue. *)

CONST 
  DefaultDuration = 1.0D0;

END AnimateVBT.
