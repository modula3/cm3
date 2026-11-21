MODULE TwoWayStack;

TYPE
  Frame = ARRAY OF REFANY;

  Procedure = PROCEDURE(VAR parent_frame : Frame;
                        VAR args         : Frame) : Object RAISES { E };

  Closure = REF RECORD
    F            : Procedure;
    parent_frame : REF Frame;
  END;

  FrameCopier = PROCEDURE() : REF Frame;

  FrameGetter = PROCEDURE(up, pos : CARDINAL) : Object;

  FrameSetter = PROCEDURE(up, pos : CARDINAL; val : Object);

(* 
   (define (g z)
     (define (f x)
        (lambda (y) (+ x y z))
      )
      
      (let ((res f z))   ;; this actually introduces a new scope...
        (set! z 3)
        res
      )
      
    )

*)


(* Question: is it *impossible* to change a value in the current frame
   after a depending value has been constructed? *)

(* 
   (define (g z)
     (define f 
       (lambda(x)
         (lambda (y) (+ x y z)))
      )
      
      (f z)
    )

*)

(* frames copied only on making a closure? *)

PROCEDURE Proc_g(VAR args         : Frame;
                 fc               : FrameCopier) : Object RAISES { E } =

  PROCEDURE FC() : REF Frame =
    BEGIN
      frameRef := NEW(REF Frame, NUMBER(args));
      frameRef^ := args;
      frameRef[FramePos] := fc()
    END FC;
    
  VAR
    frameRef : REF Frame := NIL;
    z := args[0];
  BEGIN
    f := NEW(Closure, F := Proc_f, parent_frame := FC() );

    RETURN CallClosure(f, Frame { z }) (* really T-R *)
  END Proc_g;

PROCEDURE Proc_f(VAR args        : Frame;
                 fc              : FrameCopier) : Object RAISES { E } =

  PROCEDURE FC() : REF Frame =
    BEGIN
      frameRef := NEW(REF Frame, NUMBER(args));
      frameRef^ := args;
      frameRef[FramePos] := fc()
    END FC;

  VAR
    frameRef : REF Frame := NIL;
    x := args[0];
  BEGIN
    e := NEW(Closure, F := Proc_e, parent_frame := FC() );
    RETURN e
  END Proc_f;
    
PROCEDURE Proc_e(VAR args       : Frame;
                 fc, fg, fs             ) : Object RAISES { E } =

  PROCEDURE FrameGet(level, pos : CARDINAL) : Object =
    BEGIN
      IF    level > 0 THEN 
        RETURN fg(level - 1, pos) 
      ELSIF frameRef # NIL THEN  
        (* what about the closure case? *)
        (* necessary? *)
        RETURN frameRef[pos]
      ELSE
        RETURN args[pos]
      END
    END FrameGet;
    
  VAR
    x := FrameGet(1,0);
    z := FrameGet(2,0);
    y := args[0];
  BEGIN
    RETURN PrimPlus(Frame { x, y, z })
  END Proc_e;

BEGIN END TwoWayStack.
