MODULE Test EXPORTS Main;

IMPORT EventNumber, EventNumberF, Fmt, IO, EventCounter, WorkerPool,
       Work, Text, Time; 

TYPE Action = {acquire, wait, set, enqueue};

TYPE
  MyWork = Work.T OBJECT
             time   : Time.T;
             action : Action;
             value  : EventNumber.T;
             msg    : Text.T;
           OVERRIDES
             handle := MyJobber;
           END;
  
VAR
  ec    : EventCounter.T;
  wp    : WorkerPool.T;

TYPE
  Handler = EventCounter.Handler OBJECT 
    work: MyWork;
    arrive: Time.T;
  OVERRIDES
    handle := HandlerMeth
  END;
    
PROCEDURE HandlerMeth (self: Handler) =
  VAR
    acquire: Time.T := Time.Now();
  BEGIN
    IO.Put(Fmt.LongReal(acquire, prec := 10) 
             & " Execute Enqueued Event " & self.work.value.fmt() & " ("
             & Fmt.LongReal(self.arrive - self.work.time, prec := 2) & " + "
             & Fmt.LongReal(acquire - self.arrive, prec := 2) & " = "
             & Fmt.LongReal(acquire - self.work.time, prec := 2) & "): "
             & self.work.msg & "\n");
  END HandlerMeth;

PROCEDURE MyJobber (work: MyWork) =
  VAR
    arrive : Time.T := Time.Now();
    acquire: Time.T;

  PROCEDURE Print(msg: Text.T) =
    BEGIN
      IO.Put(Fmt.LongReal(acquire, prec := 10) 
             & " " & msg & work.value.fmt() & " ("
             & Fmt.LongReal(arrive - work.time, prec := 2) & " + "
             & Fmt.LongReal(acquire - arrive, prec := 2) & " = "
             & Fmt.LongReal(acquire - work.time, prec := 2) & "): "
             & work.msg & "\n");
    END Print;      

  BEGIN
    CASE work.action OF
    | Action.acquire =>
      TRY
        ec.acquire(work.value);
        acquire := Time.Now();
        Print("Worker Acquired ");
        ec.release();
      EXCEPT
      | EventCounter.Duplicate =>
          acquire := Time.Now();
          Print("** Worker Acquire attempt RAISED Duplicate for ");
      END;
    | Action.wait =>
      ec.wait(work.value);
      acquire := Time.Now();
      Print("Worker Wait ");
    | Action.enqueue =>
      acquire := Time.Now();
      ec.enqueueAction(work.value,NEW(Handler, work := work, 
                                      arrive := arrive));
      Print("Worker Enqueue ");
    | Action.set =>
      TRY
        ec.set(work.value);
        acquire := Time.Now();
        Print("Worker Set ");
      EXCEPT
      | EventCounter.Invalid =>
          acquire := Time.Now();
          Print("** Worker Set attempt RAISED Invalid for ");
      END;
    END;
  END MyJobber;

VAR
  work := ARRAY [0 .. 18] OF
            MyWork{
            NEW(MyWork, action := Action.enqueue, 
                value := NEW(EventNumber.T, lo := 9, hi := 0), 
                msg := "Woopie."), 
            NEW(MyWork, action := Action.wait, 
                value := NEW(EventNumber.T, lo := 3, hi := 0), 
                msg := "Wham."), 
            NEW(MyWork, action := Action.wait, 
                value := NEW(EventNumber.T, lo := 1, hi := 0),
                msg := "Wham."),
            NEW(MyWork, action := Action.wait, 
                value := NEW(EventNumber.T, lo := 1, hi := 0),
                msg := "Bam."),
            NEW(MyWork, action := Action.enqueue, 
                value := NEW(EventNumber.T, lo := 2, hi := 0),
                msg := "lovely."),
            NEW(MyWork, action := Action.acquire, 
                value := NEW(EventNumber.T, lo := 1, hi := 0),
                msg := "Wham."),
            NEW(MyWork, action := Action.acquire, 
                value := NEW(EventNumber.T, lo := 7, hi := 0),
                msg := "Eeee."),
            NEW(MyWork, action := Action.acquire, 
                value := NEW(EventNumber.T, lo := 5, hi := 0),
                msg := "Fun."),
            NEW(MyWork, action := Action.acquire, 
                value := NEW(EventNumber.T, lo := 3, hi := 0),
                msg := "mam."),
            NEW(MyWork, action := Action.acquire, 
                value := NEW(EventNumber.T, lo := 5, hi := 0),
                msg := "Fun."),
            NEW(MyWork, action := Action.acquire, 
                value := NEW(EventNumber.T, lo := 5, hi := 0),
                msg := "Fun."),
            NEW(MyWork, action := Action.set, 
                value := NEW(EventNumber.T, lo := 5, hi := 0),
                msg := "Wow."),
            NEW(MyWork, action := Action.wait, 
                value := NEW(EventNumber.T, lo := 7, hi := 0),
                msg := "Eeee."),
            NEW(MyWork, action := Action.wait, 
                value := NEW(EventNumber.T, lo := 6, hi := 0),
                msg := "Eeee."),
            NEW(MyWork, action := Action.wait, 
                value := NEW(EventNumber.T, lo := 8, hi := 0),
                msg := "Eeee."),
            NEW(MyWork, action := Action.set, 
                value := NEW(EventNumber.T, lo := 8, hi := 0),
                msg := "Wow."),
            NEW(MyWork, action := Action.acquire, 
                value := NEW(EventNumber.T, lo := 4, hi := 0),
                msg := "Wow."),
            NEW(MyWork, action := Action.acquire, 
                value := NEW(EventNumber.T, lo := 10, hi := 0),
                msg := "Wow."),
            NEW(MyWork, action := Action.acquire, 
                value := NEW(EventNumber.T, lo := 8, hi := 0),
                msg := "Wow.")};

BEGIN
  ec := NEW(EventCounter.T).init(NEW(EventNumber.T, lo := 1, hi := 0));
  wp := NEW(WorkerPool.T).init();

  FOR i := FIRST(work) TO LAST(work) DO
    (* IO.Put("Adding Work: " & Fmt.Bool(work[i].acquire) & ", " &
       Fmt.Int(work[i].value) & ", " & work[i].msg & "\n"); *)
    work[i].time := Time.Now();
    wp.add(work[i]);
  END;
  ec.acquire(NEW(EventNumber.T, lo := 11, hi := 0));
  ec.release();
  wp.finish();
END Test.
