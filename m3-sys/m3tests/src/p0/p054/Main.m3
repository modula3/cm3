(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

(*
    Title: 	Martin Richards' benchmark program transcribed from BCPL
    LastEdit:	"Fri Nov 16 11:41:35 1984"
    Author: 	pr
		Cambridge University Computer Laboratory
*)


IMPORT Fmt, Wr, Stdio, Word, Test;
<*FATAL ANY*>

CONST   CharsPerLine = 50;
 
VAR
    Tracing: BOOLEAN;
    Layout: INTEGER;
 
PROCEDURE Trace (Ch: CHAR) =
BEGIN
    DEC (Layout);
    IF Layout <= 0 THEN
        Wr.PutText(Stdio.stdout, "\n");
        Layout := CharsPerLine
    END;
    Wr.PutText(Stdio.stdout, Fmt.Char(Ch));
END Trace;
 

CONST
    Idler = 1;
    Worker = 2;
    HandlerA = 3;
    HandlerB = 4;
    DeviceA = 5;
    DeviceB = 6;
 

TYPE    WorkQueue = REF Packet;
 
TYPE    Task = REF TaskControlBlock;
 
CONST   NumberOfTasks = 10;
TYPE    TaskIdentity = [1 .. NumberOfTasks];
 
TYPE    TaskPriority = CARDINAL;
 
TYPE    TaskProperties = {PacketPending, TaskWaiting, TaskHolding};
        TaskState = SET OF TaskProperties;

CONST
    Running = TaskState {};
    Waiting = TaskState {TaskProperties.TaskWaiting};
    WaitingWithPacket = TaskState {TaskProperties.PacketPending,
                                   TaskProperties.TaskWaiting};

TYPE    TaskData = OBJECT
                   METHODS
                     operation(wq: WorkQueue): Task;
                   END;

(* TYPE TaskOperation = PROCEDURE (td: TaskData; wq: WorkQueue): Task; *)
 
TYPE    TaskControlBlock = RECORD
        Link: Task;
        Identity: TaskIdentity;
        Priority: TaskPriority;
        Input: WorkQueue;
        State: TaskState;
(* superceded        Function: TaskOperation; *)
        Handle: TaskData;
    END;

CONST   NoTask = NIL;
 
TYPE    PacketKind = {Device, Work};
 
CONST   PacketBufferSize = 3;
 
TYPE    Packet = RECORD
        Link: WorkQueue;
        Identity: TaskIdentity;
        Kind: PacketKind;
        Datum: Word.T;
        Data: ARRAY [0 .. PacketBufferSize] OF Word.T;
    END;
 
CONST   NoWork = NIL;
 
VAR     QueuePacketCount: CARDINAL;
    HoldCount: CARDINAL;

VAR     TaskTable:          ARRAY TaskIdentity OF Task;
    TaskList, CurrentTask:  Task;
    CurrentTaskIdentity:    TaskIdentity;
 
 
PROCEDURE CreateTask (identity: TaskIdentity;
                      priority: TaskPriority;
                      InitialWorkQueue: WorkQueue;
                      InitialState: TaskState;
(* superceded                      function: TaskOperation; *)
                      PrivateData: TaskData) =
    VAR t := NEW (Task);
BEGIN
    TaskTable [identity] := t;
        t^.Link := TaskList;
        t^.Identity := identity;
        t^.Priority := priority;
        t^.Input := InitialWorkQueue;
        t^.State := InitialState;
(* superceded        t^.Function := function; *)
        t^.Handle := PrivateData;
    TaskList := t;
END CreateTask;
 
 
PROCEDURE CreatePacket (link: WorkQueue; identity: TaskIdentity;
        kind: PacketKind): WorkQueue =
    VAR     p :=  NEW (WorkQueue);
BEGIN
        p^.Link := link;
        p^.Identity := identity;
        p^.Kind := kind;
        p^.Datum := (0);
        FOR i := 0 TO PacketBufferSize DO
            p^.Data [i] := (0)
        END;
    RETURN p;
END CreatePacket;
 
 
PROCEDURE QueuePacket (packet: WorkQueue): Task =
    VAR t: Task;
BEGIN
    t := FindTask (packet^.Identity);
    IF t = NoTask THEN
        RETURN NoTask
    END;
    INC (QueuePacketCount);
    packet^.Link := NoWork;
    packet^.Identity := CurrentTaskIdentity;
    IF t^.Input = NoWork THEN
        t^.Input := packet;
        t^.State := t^.State + TaskState{TaskProperties.PacketPending};
        IF t^.Priority > CurrentTask^.Priority THEN
            RETURN t
        END
    ELSE
        Append (packet, t^.Input)
    END;
    RETURN CurrentTask
END QueuePacket;
 
 
PROCEDURE Append (packet: WorkQueue; VAR head: WorkQueue) =
    VAR mouse: WorkQueue;
BEGIN
    packet^.Link := NoWork;
    IF head = NoWork THEN
        head := packet;
        RETURN
    END;
    mouse := head;
    WHILE mouse^.Link # NoWork DO
        mouse := mouse^.Link
    END;
    mouse^.Link := packet
END Append;
 
 
PROCEDURE Schedule() =
    VAR     Message: WorkQueue;
BEGIN
    CurrentTask := TaskList;
    WHILE CurrentTask # NoTask DO
      Message := NoWork;
    (***
        Print.F2 ("CurrentTask = %U   State = %U\n", Identity, State);
    ***)
      IF (TaskProperties.TaskHolding IN CurrentTask^.State) OR
         CurrentTask^.State = Waiting THEN
        CurrentTask := CurrentTask^.Link;
      ELSE
        IF CurrentTask^.State = WaitingWithPacket THEN
            Message := CurrentTask^.Input;
            CurrentTask^.Input := Message^.Link;
            IF CurrentTask^.Input = NoWork THEN
                CurrentTask^.State := Running
            ELSE
                CurrentTask^.State := TaskState {TaskProperties.PacketPending};
            END
        END;
        CurrentTaskIdentity := CurrentTask^.Identity;
        IF Tracing THEN
            Trace (VAL(ORD ('0') + CurrentTaskIdentity, CHAR));
        END;
        CurrentTask := CurrentTask^.Handle.operation(Message);
        END;
    END;
END Schedule;
 
 
PROCEDURE Wait (): Task =
BEGIN
    CurrentTask^.State := CurrentTask^.State + 
     TaskState{TaskProperties.TaskWaiting};
    RETURN CurrentTask
END Wait;
 
 
PROCEDURE HoldSelf (): Task =
BEGIN
    INC (HoldCount);
    CurrentTask^.State := CurrentTask^.State + 
     TaskState{TaskProperties.TaskHolding};
    RETURN CurrentTask^.Link
END HoldSelf;
 
 
PROCEDURE Release (identity: TaskIdentity): Task =
    VAR t: Task;
BEGIN
    t := FindTask (identity);
    IF t = NoTask THEN
        RETURN NoTask
    END;
    t^.State := t^.State - TaskState{TaskProperties.TaskHolding};
    IF t^.Priority > CurrentTask^.Priority THEN
        RETURN t
    END;
    RETURN CurrentTask
END Release;
 
 
PROCEDURE FindTask (identity: TaskIdentity): Task =
    VAR t: Task;
BEGIN
    t := NoTask;
    IF identity IN SET OF [1..NumberOfTasks]{1 .. NumberOfTasks} THEN
        t := TaskTable [identity]
    END;
    IF t = NoTask THEN
        Wr.PutText (Stdio.stdout, "Bad task identity\n");
    END;
    RETURN t
END FindTask;

TYPE
    BITSET = Word.T; (* bug!!! *)
    IdleData = TaskData OBJECT
            Control: BITSET;
            Count: CARDINAL
        OVERRIDES
            operation := IdleFunction;
        END;

CONST
(* bug!    InitialControl = {LEASTSIGBIT}; *)
    HashValue = 16_D008;
    InitialControl = 1;

VAR
    HashControl: BITSET;
 
PROCEDURE CreateIdler (identity: TaskIdentity; priority: TaskPriority;
        work: WorkQueue; state: TaskState) =
    VAR     data := NEW (IdleData);
BEGIN
    data.Control := InitialControl;
    data.Count := 10000;
    CreateTask (identity, priority, work, state, data)
END CreateIdler;
 
PROCEDURE IdleFunction (Data: IdleData;  <*UNUSED*>work: WorkQueue): Task =
BEGIN
    DEC (Data.Count);
    IF Data.Count = 0 THEN
        RETURN HoldSelf ()
    END;
    IF Data.Control MOD 2 = 0 THEN
        Data.Control := (Data.Control) DIV 2;
        RETURN Release (DeviceA)
    ELSE
        Data.Control := Word.Xor(Data.Control DIV 2, HashControl);
        RETURN Release (DeviceB);
    END;
END IdleFunction;

TYPE
    DeviceData = TaskData OBJECT
               pending:    WorkQueue
        OVERRIDES
               operation := DeviceFunction;
        END;
 
PROCEDURE CreateDevice (identity: TaskIdentity; priority: TaskPriority;
        work: WorkQueue; state: TaskState) =
    VAR data := NEW (DeviceData);
BEGIN
    data.pending := NoWork;
    CreateTask (identity, priority, work, state, data)
END CreateDevice;
 
PROCEDURE DeviceFunction (data: DeviceData; work: WorkQueue): Task =
BEGIN
    IF work = NoWork THEN
        IF data.pending = NoWork THEN
            RETURN Wait ()
        END;
        work := data.pending;
        data.pending := NoWork;
        RETURN QueuePacket (work)
    ELSE
        data.pending := work;
        IF Tracing THEN
            Trace (VAL(work^.Datum, CHAR));
        END;
        RETURN HoldSelf ();
    END
END DeviceFunction;

TYPE
    HandlerData = TaskData OBJECT
            WorkIn, 
            DeviceIn:   WorkQueue;
        OVERRIDES
            operation := HandlerFunction;
        END;
 
PROCEDURE CreateHandler (identity: TaskIdentity; priority: TaskPriority;
        work: WorkQueue; state: TaskState) =
    VAR     data := NEW (HandlerData);
BEGIN
    data.WorkIn := NoWork;
    data.DeviceIn := NoWork;
    CreateTask (identity, priority, work, state, data);
END CreateHandler;
 
PROCEDURE HandlerFunction (data: HandlerData; work: WorkQueue): Task =
    VAR     workpacket, devicepacket: WorkQueue;
        count: INTEGER;
BEGIN
    IF work # NoWork THEN
        IF work^.Kind = PacketKind.Work THEN
            Append (work, data.WorkIn)
        ELSE
            Append (work, data.DeviceIn)
        END
    END;
    IF data.WorkIn # NoWork THEN
        workpacket := data.WorkIn;
        count := workpacket^.Datum;
        IF count > PacketBufferSize THEN
            data.WorkIn := data.WorkIn^.Link;
            RETURN QueuePacket (workpacket);
        END;
        IF data.DeviceIn # NoWork THEN
            devicepacket := data.DeviceIn;
            data.DeviceIn := data.DeviceIn^.Link;
            devicepacket^.Datum := workpacket^.Data [count];
            workpacket^.Datum := (count + 1);
            RETURN QueuePacket (devicepacket);
        END;
    END;
    RETURN Wait ();
END HandlerFunction;

TYPE
    WorkerData = TaskData OBJECT
            Destination:    TaskIdentity;
            Count:          CARDINAL;
        OVERRIDES
            operation := WorkFunction;
        END;
 
PROCEDURE CreateWorker (identity: TaskIdentity; priority: TaskPriority;
        work: WorkQueue; state: TaskState) =
    VAR     data := NEW (WorkerData);
BEGIN
    data.Destination := HandlerA;
    data.Count := 0;
    CreateTask (identity, priority, work, state, data);
END CreateWorker;
 
PROCEDURE WorkFunction (data: WorkerData; work: WorkQueue): Task =
    VAR Ch: CHAR;
BEGIN
    IF work = NoWork THEN
        RETURN Wait ()
    ELSE
            data.Destination := HandlerA + HandlerB - data.Destination;
            work^.Identity := data.Destination;
            work^.Datum := (0);
            FOR i := 0 TO PacketBufferSize DO
                INC (data.Count);
                IF data.Count > 26 THEN
                    data.Count := 1
                END;
                Ch := VAL(ORD ('A') - 1 + data.Count, CHAR);
                work^.Data [i] := ORD(Ch);
            END;
            RETURN QueuePacket (work);
    END;
END WorkFunction;

 
VAR     WorkQ: WorkQueue;

BEGIN
FOR i := 1 TO 100 DO
    Tracing := FALSE;
    Layout := 0;

    FOR TaskNumber := 1 TO NumberOfTasks DO
        TaskTable [TaskNumber] := NoTask;
    END;
    TaskList := NoTask;
    QueuePacketCount := 0;
    HoldCount := 0;

(*
    HashControl := BITSET (HashValue);
*)
    HashControl := HashValue;
 
    Wr.PutText (Stdio.stdout, "Bench mark starting\n");
 
    CreateIdler (Idler, 0, NoWork, Running);
 
    WorkQ := CreatePacket (NoWork, Worker, PacketKind.Work);
    WorkQ := CreatePacket (WorkQ, Worker, PacketKind.Work);
    CreateWorker (Worker, 1000, WorkQ, WaitingWithPacket);
 
    WorkQ := CreatePacket (NoWork, DeviceA, PacketKind.Device);
    WorkQ := CreatePacket (WorkQ, DeviceA, PacketKind.Device);
    WorkQ := CreatePacket (WorkQ, DeviceA, PacketKind.Device);
    CreateHandler (HandlerA, 2000, WorkQ, WaitingWithPacket);
 
    WorkQ := CreatePacket (NoWork, DeviceB, PacketKind.Device);
    WorkQ := CreatePacket (WorkQ, DeviceB, PacketKind.Device);
    WorkQ := CreatePacket (WorkQ, DeviceB, PacketKind.Device);
    CreateHandler (HandlerB, 3000, WorkQ, WaitingWithPacket);
 
    CreateDevice (DeviceA, 4000, NoWork, Waiting);
    CreateDevice (DeviceB, 5000, NoWork, Waiting);
 
    Wr.PutText (Stdio.stdout, "Starting\n");
    Tracing := FALSE;
    Schedule();
    Wr.PutText (Stdio.stdout, "Finished\n");
 
    Wr.PutText (Stdio.stdout, 
     "QueuePacket count = " & Fmt.Int(QueuePacketCount) & 
     "     HoldCount = " & Fmt.Int(HoldCount) & "\n");

    Wr.PutText (Stdio.stdout, "These results are ");
    IF (QueuePacketCount = 23246) AND (HoldCount = 9297) THEN
        Wr.PutText (Stdio.stdout, "correct");
    ELSE
        Wr.PutText (Stdio.stdout, "wrong");
    END;
 
    Wr.PutText (Stdio.stdout, "\nEnd of run\n");
END;
    Test.done ();
END Main.




