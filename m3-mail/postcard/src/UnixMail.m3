(* Copyright 1989 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(*                                                             *)
(* Last modified on Wed Feb 21 16:23:24 PST 1996 by mhb        *)
(*      modified on Wed Feb  1 12:22:45 PST 1995 by kalsow     *)
(*      modified on Wed Apr 27 15:33:02 PDT 1994 by birrell    *)
(*      modified on Mon Jul  6 17:52:32 PDT 1992 by meehan     *)
(*      modified on Wed Jan 15 23:17:01 PST 1992 by jerian     *)
(*      modified on Thu Jul 12 11:36:08 PDT 1990 by swart      *)
(*      modified on Fri Oct 13 15:53:56 PDT 1989 by mcjones    *)
(*      modified on Mon Sep 18 16:58:05 PDT 1989 by denning    *)
(*      modified on Tue Jan  5 11:38:45 PST 1988 by discolo    *)

MODULE UnixMail;

IMPORT Fmt;
IMPORT Env, MailUtilities, MiscUtils, OSUtils,
       Process, Rd, Stdio, TextRefTbl, Thread, Text, TextList, TextRd,
       TextWr, Time, Wr;

<*FATAL Wr.Failure*>
<*FATAL Thread.Alerted*>
<*FATAL Rd.Failure*>

(******** A note on synchronization and concurrency. ********

    There are two optimizations that make this module somewhat complex:
      1) Many actions (e.g. move, copy, delete) do no work synchronously,
         and instead add requests to the background queue: see InvokeMHCommand
         and AppendToBackgroundQueue.  There is a single daemon thread that
         process requests from the queue.
      2) There is a daemon thread "ScanDaemon" that tries to run llscan
         at times when the user won't notice the cost.  This requires care to
         avoid running llscan at the same time as other actions that access
         the folder's files: synchronous client calls and client actions
         being processed asynchronously by the dequeueing daemon thread.
    In general, this module assumes the caller guarantees single-threaded
    invokation of the main entry points.  The exceptions are those entry
    points intended for client daemons, as noted in the .def comments.  More
    precisely, this module assumes that its client is careful that the calls it
    makes are consistent with the deferred requests in the background queue.
    The client can call ChangeFolders to ensure this.  However, this module
    itself ensures that calls that modify folder message numbering wait for
    background requests that read the messages to complete.
    The locking strategies are as follows.
      a) AcquireShared, ReleaseShared, AcquireExclusive and ReleaseExclusive
         provide reader/writer synchronization to protect the table of folders
         and the folder list.
      b) Reading particular message bodies (e.g. Print or Save) are
         unsynchronized, and rely on the client to prevent confusing its user.
      c) "scanMutex" protects folder fields and globals that are accessed by
         multiple threads - internal daemons or client daemons.
      d) "backgroundQueueMutex" protects the queue of asynchronous requests.
      e) "EnqueueAccessFolders" notes that a request which will update
         the folder(s) is being added to the queue; it does not block
         for other actions; it is called only inside
         "AppendToBackgroundQueue".  "DequeueAccessFolders" notes
         a request being removed from the queue, and blocks until
         there is no other access to the folder(s) (apart from queued
         ones).  "AccessFolder" permits access to the folder contents,
         excluding other access except for actions under item (b); it defers
         until enqueued actions have completed.
      f) "incMutex" provides mutual exclusion between scanning and the
         NewMail polling daemon, so that we can get an accurate count of
         new messages.
      g) The two major daemons "ScanDaemon" and "CheckpointThread" block
         on semaphores when they're idle.
*)

CONST
  NewMH = TRUE; (* => use MHSortCmd for sort command *)
  FromCmd = "/usr/ucb/from"; (*hardwired because we depend on a fixed format. *)
  MHSendCmd = "send";
  MHFileCmd = "refile";
  MHRmmCmd = "rmm";
  MHFolderCmd = "folder";
  MHSortCmd = "sortm";
  LLScanCmd = "llscan";
  RmCmd = "/bin/rm";
  MaxMHArgs = 35; (* Actually 40, but include slop for other args *)

TYPE
  AT = ARRAY OF TEXT;
  Folder = RECORD
                 (* Fields protected by AcquireShared and AcquireExclusive: *)
      prev, next: FolderPtr;
                 (* Read-only fields (after initialization): *)
      name: Text.T;          (* e.g., "inbox" or "src.mail" *)
      fullPathName: Text.T;  (* e.g., "/udir/discolo/Mail/inbox" *)
      bBoard: BOOLEAN;       (* TRUE if folder is bulletin board *)
      newsDirName: Text.T;   (* path of form /usr/spool/news/... *)
                 (* Fields protected by scanMutex. *)
      curMsg: TEXT;
      checkpoint: BOOLEAN;   (* TRUE if cur file needs checkpointing *)
      mustScan: BOOLEAN;     (* => we know llscan is needed *)
      autoScan: BOOLEAN;     (* permits ScanDaemon; cleared on error *)
             (* NOTE: altering these two fields affects "scanNeeded" *)
      hasNews: BOOLEAN;      (* TRUE if bboard has news waiting *)
      doPurge: BOOLEAN;      (* TRUE if bboard has been reorganized *)
      mtime: Time.T;         (* of fullPathName as of last scan *)
      newsDirMTime: Time.T;  (* last mtime of newsDirName *)
                 (* Fields protected by backgroundQueueMutex *)
      inUse: BOOLEAN;        (* TRUE if we're accessing folder files *)
      queueActions: INTEGER; (* count of update on the queue *)
    END;

REVEAL FolderPtr = BRANDED REF Folder;

TYPE (* Queue of requests for background processing *)
  BackgroundQueueElement = REF BackgroundQueueRecord;
  BackgroundQueueRecord = RECORD
      next: BackgroundQueueElement;
      argv: REF AT;
      f1, f2: FolderPtr;
    END;


VAR (* The following are protected by AcquireShared and AcquireExclusive: *)
  folderMutex := NEW(MUTEX);        (* Internal to AcquireShared, etc. *)
  folderLock: INTEGER;              (* State of AcquireShared, etc. *)
  folderCond := NEW(Thread.Condition); (* broadcast from ReleaseShared, etc. *)
  folderHead: FolderPtr;            (* Folder list *)
  folderTail: FolderPtr;            (* Folder list *)
  folderTable: TextRefTbl.T;        (* All folders, indexed by name *)


VAR (* The following are protected by scanMutex: *)
  scanMutex := NEW(MUTEX);          (* Protect misc. stuff - see above *)
  newsCount: CARDINAL;              (* Number of folders having news *)
  scanEnableCV := NEW(Thread.Condition);
  scanDaemonCV := NEW(Thread.Condition);
  runScanDaemon: BOOLEAN;           (* enables scan daemon *)
  scanNeeded: BOOLEAN;              (* a folder has (mustScan AND autoScan) *)
  checkpointCV := NEW(Thread.Condition);
  checkpointNeeded: INTEGER;        (* INC when cur file needs checkpointing *)


VAR
  incMutex := NEW(MUTEX);           (* Protect transfer of msgs from spool *)


VAR (* The following are read-only after initialization: *)
  newsSpoolName, mailboxName, mailDirName, draftDirName: TEXT;
  checkpointThread, scanDaemonThread: Thread.T;


VAR (* The following are protected by backgroundQueueMutex: *)
  backgroundQueueMutex := NEW(MUTEX);
  backgroundQueueEmpty := NEW(Thread.Condition);
  accessCond := NEW(Thread.Condition); (* broadcast when f^.inUse := FALSE *)
  backgroundQueueHead, backgroundQueueTail: BackgroundQueueElement;
  tempFileCount: CARDINAL; (* For RunFilterText *)

VAR (* The following are read-only after initialization: *)
  backgroundQueueThread: Thread.T;     (* for debugging *)
  backgroundQueueHandler: ErrorHandler;


(* *)
(* Locking machinery *)
(* *)

PROCEDURE AcquireExclusive () =
  BEGIN
    LOCK folderMutex DO
      WHILE folderLock # 0 DO Thread.Wait (folderMutex, folderCond);  END;
      folderLock :=  -1;
    END
  END AcquireExclusive;

PROCEDURE ReleaseExclusive () =
  BEGIN
    LOCK folderMutex DO ASSERT (folderLock =  -1); folderLock := 0;  END;
    Thread.Broadcast (folderCond);
  END ReleaseExclusive;

PROCEDURE AcquireShared () =
  BEGIN
    LOCK folderMutex DO
      WHILE folderLock < 0 DO Thread.Wait (folderMutex, folderCond);  END;
      INC (folderLock);
    END;
  END AcquireShared;

PROCEDURE ReleaseShared () =
  BEGIN
    LOCK folderMutex DO ASSERT (folderLock > 0); DEC (folderLock);  END;
    Thread.Broadcast (folderCond);
  END ReleaseShared;

PROCEDURE EnqueueAccessFolders (f1, f2: FolderPtr := NIL) =
    (* Used just before enqueuing a background update for f1, f2 *)
    (* LL = backgroundQueueMutex *)
  BEGIN
    IF f1 # NIL THEN INC (f1.queueActions) END;
    IF f2 # NIL THEN INC (f2.queueActions) END;
  END EnqueueAccessFolders;

PROCEDURE DequeueAccessFolders (f1, f2: FolderPtr := NIL) =
    (* Note that an async folder access is about to actually happen *)
    (* LL = backgroundQueueMutex *)
  BEGIN
    WHILE ((f1 # NIL) AND f1.inUse) OR ((f2 # NIL) AND f2.inUse) DO
      Thread.Wait (backgroundQueueMutex, accessCond);
    END;
    IF f1 # NIL THEN f1.inUse := TRUE; DEC (f1.queueActions) END;
    IF f2 # NIL THEN f2.inUse := TRUE; DEC (f2.queueActions) END;
  END DequeueAccessFolders;

PROCEDURE AccessFolder (f: FolderPtr) =
    (* Acquires the right to modify files in the folder; used before
       non-queued MH commands, such as inside DoLLScan; defers to queue actions *)
  BEGIN
    LOCK backgroundQueueMutex DO
      WHILE (f.inUse) OR (f.queueActions > 0) DO
        Thread.Wait (backgroundQueueMutex, accessCond);
      END;
      f.inUse := TRUE;
    END;
  END AccessFolder;

PROCEDURE FinishAccessFolders (f1, f2: FolderPtr := NIL) =
  BEGIN
    LOCK backgroundQueueMutex DO
      IF f1 # NIL THEN f1.inUse := FALSE END;
      IF f2 # NIL THEN f2.inUse := FALSE END;
    END;
    Thread.Broadcast (accessCond);
  END FinishAccessFolders;


(***********************************)
(* Miscellaneous System Procedures *)
(***********************************)

PROCEDURE GetInfo(fileName: TEXT; VAR (*OUT*) mtime: Time.T): OSUtils.FileType
                  RAISES{Error} =
  BEGIN
    TRY
      RETURN OSUtils.GetInfo(fileName, mtime);
    EXCEPT
      | OSUtils.FileNotFound =>
          RAISE Error("\"" & fileName & "\" doesn't exist");
      | OSUtils.FileError(why) =>
          RAISE Error("\"" & fileName & "\": " & why);
    END;
  END GetInfo;

PROCEDURE OpenRead(fileName: TEXT): Rd.T RAISES {Error} =
  BEGIN
    TRY
      OSUtils.CheckFile(fileName);
      RETURN OSUtils.OpenRead(fileName);
    EXCEPT
      | OSUtils.FileError(why) =>
          RAISE Error("Can't open \"" & fileName & "\": " & why);
      | OSUtils.FileNotFound =>
          RAISE Error("\"" & fileName & "\" doesn't exist");
    END;
  END OpenRead;

PROCEDURE OpenWrite(fileName: TEXT; append: BOOLEAN): Wr.T RAISES {Error} =
  BEGIN
    TRY
      RETURN OSUtils.OpenWrite(fileName, append);
    EXCEPT
      | OSUtils.FileError(why) =>
          RAISE Error("Can't write \"" & fileName & "\": " & why);
    END;
  END OpenWrite;

  
(***************************)
(* Exec Process Procedures *)
(***************************)

PROCEDURE RunProcess(READONLY argv: AT): OSUtils.Filter
                     RAISES {Error} =
    (* Start a child argv[0], using argv *)
  BEGIN
    TRY
      RETURN OSUtils.RunProcess(argv)
    EXCEPT OSUtils.FileError(t) =>
      RAISE Error("Couldn't run \"" & argv[0] & "\": " & t);
    END;
  END RunProcess;

PROCEDURE ExecProcess(READONLY argv: AT): TEXT RAISES {Error} =
  VAR
    child: OSUtils.Filter;
    output: TEXT;
    status: INTEGER;
    errText: Text.T;
  BEGIN
    child := RunProcess(argv);
    Wr.Close(child.stdin);
    output := Rd.GetText(child.stdout, LAST(CARDINAL)); (* includes stderr *)
    Rd.Close(child.stdout);
    status := OSUtils.WaitProcess(child.handle);
    errText := "\"" & argv[0] & "\": ";
    IF status # 0 THEN
      IF Text.Empty(output) THEN
        RAISE Error(errText & "ended with status " & Fmt.Int(status))
      ELSE
        RAISE Error(errText & output)
      END;
    END;
    RETURN output
  END ExecProcess;

PROCEDURE SetupFilterArgs(VAR numTokens: CARDINAL; reserve: INTEGER;
                          filter: TEXT): REF AT =
  VAR
    rd: Rd.T;
    c: CHAR;
    argv: REF AT;
    wr: Wr.T;
  BEGIN
        (* Count tokens in filter. *)
    rd := TextRd.New(filter);
    numTokens := 0;
    TRY
      c := Rd.GetChar(rd);
      LOOP
        WHILE c IN MailUtilities.DefaultWS DO c := Rd.GetChar(rd) END;
        INC(numTokens);
        WHILE NOT(c IN MailUtilities.DefaultWS) DO c := Rd.GetChar(rd) END;
      END;
    EXCEPT Rd.EndOfFile =>
    END;
    Rd.Close (rd);
    argv :=  NEW(REF AT, numTokens + reserve);
    IF numTokens > 0 THEN
      rd := TextRd.New(filter);
      <*FATAL Rd.EndOfFile*> BEGIN
        c := Rd.GetChar(rd);
        FOR i := 0 TO numTokens - 1 DO
          WHILE c IN MailUtilities.DefaultWS DO c := Rd.GetChar(rd) END;
          wr := TextWr.New();
          LOOP
            IF c IN MailUtilities.DefaultWS THEN EXIT END;
            Wr.PutChar(wr, c);
            IF Rd.EOF(rd) THEN EXIT END;
            c := Rd.GetChar(rd);
          END;
          argv[i] := TextWr.ToText(wr);
        END;
      END;
      Rd.Close (rd);
    END;
    RETURN argv
  END SetupFilterArgs;

PROCEDURE RunFilterFile(pathName: TEXT; filter: TEXT) RAISES { Error } =
  VAR
    numWords: CARDINAL;
    argv := SetupFilterArgs(numWords, 1, filter);
  BEGIN
    argv[numWords] := pathName;
    EVAL ExecProcess(argv^);
  END RunFilterFile;

PROCEDURE RunFilterText(t: TEXT; from: INTEGER; filter: TEXT) RAISES { Error } =
  VAR mine: TEXT; wr: Wr.T;
  BEGIN
    mine := "/tmp/postcard." & Fmt.LongReal(Time.Now());
    TRY
      wr := OpenWrite(mine, FALSE);
      TRY
        MiscUtils.PutTextSub(wr, t, from);
      FINALLY
        Wr.Close(wr);
      END;
    EXCEPT Wr.Failure =>
        RAISE Error("Error while writing text into \"" & mine & "\"");
    END;
    TRY
      RunFilterFile(mine, filter)
    FINALLY
      TRY
        OSUtils.Delete(mine)
      EXCEPT OSUtils.FileError => (* ignore *)
      END;
    END;
  END RunFilterText;


(********************)
(* Background queue *)
(********************)

(* Invariants (variables names understood to begin with "backgroundQueue"):
        I1. Either Head = Tail = NIL, or
            Head = e[0],
            Tail = e[N-1], N > 0,
            e[i]^.next = e[i+1], 0 <= i < N-1,
            e[N-1]^.next = NIL.
        I2. If Head # NIL, Head^ contains the arguments to the oldest unprocessed
            call of AppendToBackgroundQueue.
        I3. Thread # NIL iff Head # NIL (and then exactly one thread
            is executing BackgroundQueueThread).
*)

PROCEDURE InitBackgroundQueue(handler: ErrorHandler) =
  BEGIN
    backgroundQueueHead := NIL; (* establish I2 *)
    backgroundQueueTail := NIL; (* establish I1 *)
    backgroundQueueThread := NIL; (* establish I3 *)
    backgroundQueueHandler := handler;
    tempFileCount := 0;
  END InitBackgroundQueue;

PROCEDURE AppendToBackgroundQueue(argv: REF AT; f1, f2: FolderPtr;
                                  isUpdate: BOOLEAN) =
  VAR element:= NEW(BackgroundQueueElement,
                    next := NIL,
                    argv := argv,
                    f1 := f1,
                    f2 := f2);
  BEGIN
    LOCK backgroundQueueMutex DO
      EnqueueAccessFolders(f1, f2);
      IF backgroundQueueHead = NIL THEN (* queue empty *)
        backgroundQueueHead := element;
        backgroundQueueThread := Thread.Fork(
            NEW(Thread.Closure, apply := BackgroundQueueThread));
      ELSE (* queue nonempty *)
        backgroundQueueTail.next := element;
      END;
      backgroundQueueTail := element;
      IF isUpdate THEN
        LOCK scanMutex DO
          IF f1 # NIL THEN SetMustScan(f1, TRUE, f1.autoScan) END;
          IF f2 # NIL THEN SetMustScan(f2, TRUE, f2.autoScan) END;
        END;
      END;
    END;
  END AppendToBackgroundQueue;

PROCEDURE DrainBackgroundQueue() RAISES {} =
  BEGIN
    LOCK backgroundQueueMutex DO
      WHILE backgroundQueueTail # NIL DO
        Thread.Wait (backgroundQueueMutex, backgroundQueueEmpty);
      END;
    END;
  END DrainBackgroundQueue;

PROCEDURE BackgroundQueueThread(<*UNUSED*> self: Thread.Closure): REFANY =
  VAR
    element: BackgroundQueueElement;
  BEGIN
    LOOP
      (* Find next element. *)
      LOCK backgroundQueueMutex DO
        ASSERT (backgroundQueueTail # NIL);
        element := backgroundQueueHead;
        DequeueAccessFolders (element.f1, element.f2);
      END;
      (* Process it *)
      TRY
        EVAL ExecProcess(element.argv^);
      EXCEPT Error(t) =>
        backgroundQueueHandler.AsyncErrorHandler(t);
      END;
      FinishAccessFolders(element.f1, element.f2);
            (* Advance queue. *)
      LOCK backgroundQueueMutex DO
        ASSERT (backgroundQueueHead = element);
        backgroundQueueHead := backgroundQueueHead.next;
        IF backgroundQueueHead = NIL THEN
          backgroundQueueTail := NIL;
          Thread.Broadcast (backgroundQueueEmpty);
          backgroundQueueThread := NIL;
          EXIT;
        END;
      END;
    END; (* LOOP *)
    RETURN NIL;
  END BackgroundQueueThread;

PROCEDURE InvokeMHCommand(f1, f2: FolderPtr;
                          l: TextList.T; (* possibly NIL *)
                          fromNI: BOOLEAN; (* msg ID is file name in spool *)
                          READONLY argv: AT) =
  VAR
    t: TextList.T;
    n, nBatch: INTEGER;
    newArgv: REF AT;
    argvPos: INTEGER;
    fileFactor: INTEGER;
  BEGIN
    ASSERT (NUMBER(argv) <= 5, "too many args for MH command line");
    t := l;
    n := TextList.Length(t);
    IF fromNI THEN fileFactor := 2 ELSE fileFactor := 1 END;
    REPEAT
      nBatch := MIN(n, MaxMHArgs DIV fileFactor);
      newArgv := NEW(REF AT, NUMBER(argv) + nBatch * fileFactor);
      SUBARRAY(newArgv^, 0, NUMBER(argv)) := argv;
      argvPos := NUMBER(argv);
      FOR i := 0 TO nBatch - 1 DO
        IF fromNI THEN
          newArgv[argvPos] := "-file";
          INC(argvPos);
          newArgv[argvPos] := MsgPath(NIFolderName, t.head);
        ELSE
          newArgv[argvPos] := MailUtilities.LTrim(t.head);
        END;
        INC(argvPos);
        t := t.tail;
      END;
      AppendToBackgroundQueue(newArgv, f1, f2, TRUE);
      DEC (n, nBatch);
    UNTIL n = 0;
  END InvokeMHCommand;

PROCEDURE QueueFilterFile(pathName: TEXT; filter: TEXT) =
  VAR
    numWords: CARDINAL;
    argv := SetupFilterArgs(numWords, 1, filter);
  BEGIN
    argv[numWords] := pathName;
    AppendToBackgroundQueue(argv, NIL, NIL, FALSE);
  END QueueFilterFile;


(**************************)
(* Folder List Procedures *)
(**************************)

PROCEDURE BBoardToPath (b: Text.T): Text.T =
    (* Substitute slash for period. *)
  VAR len: CARDINAL; ch: CHAR; newb: Text.T;
  BEGIN
    len := Text.Length (b);
    newb := "";
    FOR i := 0 TO len - 1 DO
      ch := Text.GetChar (b, i);
      IF ch = '.' THEN ch := '/';  END;
      newb := Text.Cat (newb, Text.FromChar (ch));
    END;
    RETURN newb;
  END BBoardToPath;

PROCEDURE FolderSetup (f: FolderPtr; folder: Text.T) =
  VAR rd: Rd.T;
  BEGIN
    f.name := folder;
    f.fullPathName := mailDirName & "/" & folder;
    f.curMsg := "";
    f.checkpoint := FALSE;
    (* A folder is a bboard if: (1) its name contains a period or if its
       folder directory contains a file named ".bblink" AND (2) an
       appropriate news spool file actually exists. *)
    f.bBoard := Text.FindChar(folder, '.', 0) >= 0;
    f.newsDirName := "";
    (* The contents of a ".bblink" file provide an alias. *)
    TRY
      rd := OpenRead(f.fullPathName & "/.bblink");
      f.bBoard := TRUE; (* since ".bblink" file exists *)
      TRY
        folder := Rd.GetLine(rd); (* discards newline *)
      FINALLY
        Rd.Close(rd);
      END;
    EXCEPT Error, Rd.EndOfFile, Rd.Failure => (* ignore *)
    END;
    IF f.bBoard THEN
      IF Text.GetChar(folder, 0) # '/' THEN
        f.newsDirName := newsSpoolName & "/" & BBoardToPath(folder);
      ELSE
        f.newsDirName := folder;
      END;
    END;
    f.newsDirMTime := 0.0d0;
    LOCK scanMutex DO
      SetMustScan(f, TRUE, TRUE);
      f.doPurge := FALSE;
      f.hasNews := FALSE;
    END;
    Thread.Signal(scanDaemonCV);
    LOCK backgroundQueueMutex DO
      f.inUse := FALSE;
      f.queueActions := 0;
    END;
  END FolderSetup;

PROCEDURE FolderInit(<*UNUSED*> self: Thread.Closure): REFANY =
  VAR
    contents: TextList.T;
    elem, folderPath: Text.T;
    f: FolderPtr;
    mtime: Time.T;
  BEGIN
    TRY
      OSUtils.CheckFile(mailDirName);
      contents := OSUtils.Enumerate(mailDirName);
    EXCEPT
    | OSUtils.FileNotFound =>
        Wr.PutText(Stdio.stderr, "\"" & mailDirName & "\" doesn't exist\n");
        Process.Exit(104);
    | OSUtils.FileError(t) =>
        Wr.PutText(Stdio.stderr, "Can't enumerate " & mailDirName &
                   ": " & t & "\n");
        Process.Exit(104);
    END;
    WHILE contents # NIL DO
      elem := NARROW(contents.head, TEXT);
      folderPath := mailDirName & "/" & elem;
      TRY
        IF GetInfo(folderPath, mtime) = OSUtils.FileType.Dir THEN
          f :=  NEW(FolderPtr);
          FolderSetup(f, elem);
          (* We need not scan the folder unless the mtime of the directory
             is more than an epsilon greater than the mtime of its
             ".inodecache" file.  The epsilon is needed because
             llscan isn't careful to update ".inodescan" last. *)
          LOCK scanMutex DO f.mtime := mtime END;
          TRY
            EVAL GetInfo(folderPath & "/.inodecache", mtime);
            LOCK scanMutex DO
              SetMustScan(f,
                          f.mtime > mtime + 1.0d0,
                          f.autoScan);
            END;
          EXCEPT Error => (* ignore *)
          END;
          FolderInsert (f);
        END;
      EXCEPT Error => (*ignore *)
      END;
      contents := contents.tail;
    END;
    RETURN NIL
  END FolderInit;

PROCEDURE FolderInsert (f: FolderPtr) =
  VAR tf: FolderPtr;
  BEGIN
    AcquireExclusive ();
    TRY
            (* Make sure there is no other folder with f^.name. *)
      IF folderTable.put(f.name, f) THEN
        ASSERT(FALSE, "duplicate folder name");
      END;
      tf := folderHead;
      IF tf = NIL THEN
        folderHead := f;
        folderTail := f;
        f.prev := NIL;
        f.next := NIL;
      ELSE
        LOOP
          IF Text.Compare (tf.name, f.name) > 0 THEN
            IF tf.prev = NIL THEN
              f.next := folderHead;
              f.prev := NIL;
              folderHead.prev := f;
              folderHead := f;
            ELSE
              tf.prev.next := f;
              f.next := tf;
              f.prev := tf.prev;
              tf.prev := f;
            END;
            RETURN ;
          END;
          tf := tf.next;
          IF tf = NIL THEN EXIT;  END;
        END;
        f.next := NIL;
        f.prev := folderTail;
        folderTail.next := f;
        folderTail := f;
      END;
    FINALLY
      ReleaseExclusive ();
    END;
  END FolderInsert;

PROCEDURE FolderDelete (f: FolderPtr) =
    (* Called inside AcquireExclusive *)
  VAR ref: REFANY;
  BEGIN
    IF NOT folderTable.delete(f.name, ref) THEN
      ASSERT (FALSE, "folder not found when removing from table");
    END;
    IF f.next # NIL THEN f.next.prev := f.prev;  END;
    IF f.prev # NIL THEN f.prev.next := f.next;  END;
    IF f = folderHead THEN folderHead := folderHead.next;  END;
    IF f = folderTail THEN folderTail := folderTail.prev;  END;
    f.next := NIL;
    f.prev := NIL;
  END FolderDelete;


(********************)
(*  Folder Routines *)
(********************)

PROCEDURE FindFolder(folder: Text.T): FolderPtr RAISES{Error} =
    VAR f: REFANY;
  BEGIN
    AcquireShared ();
    TRY
      IF NOT folderTable.get(folder, f) THEN
        RAISE Error("Folder \"" & folder & "\" not found")
      END;
    FINALLY
      ReleaseShared();
    END;
    RETURN f
  END FindFolder;

PROCEDURE GetNextFolder(folderPtr: FolderPtr; forward: BOOLEAN;
                        VAR candidateFolder: Text.T; VAR
   candidateBBoard: BOOLEAN): FolderPtr =
  VAR f: FolderPtr;
  BEGIN
    AcquireShared ();
    TRY
      IF folderPtr = NIL THEN
        IF forward THEN f := folderHead ELSE f := folderTail END;
      ELSIF forward THEN
        f := folderPtr.next
      ELSE
        f := folderPtr.prev
      END;
      IF f # NIL THEN
        candidateFolder := f.name;
        candidateBBoard := f.bBoard;
      END;
    FINALLY
      ReleaseShared ();
    END;
    RETURN f
  END GetNextFolder;

PROCEDURE CreateFolder(folder: Text.T) RAISES {Error} =
  VAR
    f: FolderPtr;
    dirName, newsDirName: Text.T;
    mtime: Time.T;
  BEGIN
    IF Text.FindChar (folder, '.', 0) >= 0 THEN
      newsDirName := newsSpoolName & "/" & BBoardToPath(folder);
      TRY
        EVAL GetInfo(newsDirName, mtime);
      EXCEPT Error =>
        newsDirName := "";
        RAISE Error("A folder name may not contain a period unless it names a bboard.  If you want a bboard, check for misspelling.");
      END;
    END;
    dirName := mailDirName & "/" & folder;
    TRY
      OSUtils.MakeDir(dirName);
    EXCEPT OSUtils.FileError => RAISE Error("Can't create " & dirName);
    END;
    f :=  NEW(FolderPtr);
    FolderSetup(f, folder);
    FolderInsert(f);
  END CreateFolder;

PROCEDURE ChangeFolders() RAISES {} =
  BEGIN
    DrainBackgroundQueue ();
  END ChangeFolders;

PROCEDURE RescanFolder(folder: Text.T) RAISES { Error } =
  VAR f: FolderPtr; scanFileName: Text.T;
  BEGIN
    f := FindFolder(folder);
    AccessFolder(f);
    TRY
      scanFileName := f.fullPathName & "/.inodecache";
      TRY
        OSUtils.Delete(scanFileName); (* delete .inodecache file *)
      EXCEPT OSUtils.FileError(t) =>
        RAISE Error("Can't delete " & scanFileName & ": " & t);
      END;
    FINALLY
      FinishAccessFolders(f);
    END;
  END RescanFolder;

PROCEDURE SortFolder(folder: Text.T) RAISES {Error} =
  VAR f: FolderPtr;
  BEGIN
    DrainBackgroundQueue();
    f := FindFolder(folder);
    AccessFolder(f);
    TRY
      LOCK scanMutex DO SetMustScan(f, TRUE, f.autoScan) END;
      IF NewMH THEN
        EVAL ExecProcess(AT{MHSortCmd, "+" & folder});
        EVAL ExecProcess(AT{MHFolderCmd, "-pack", "+" & folder});
      ELSE
        EVAL ExecProcess(AT{MHFolderCmd, "-sort", "-pack", "+" & folder});
      END;
      LOCK scanMutex DO
        TRY
          OSUtils.Delete(f.fullPathName & "/cur")
        EXCEPT OSUtils.FileError => (* ignore *)
        END;
        f.curMsg := "";
      END;
    FINALLY
      FinishAccessFolders (f);
    END;
  END SortFolder;

PROCEDURE RemoveFolder(folder: Text.T): FolderPtr RAISES {Error} =
  VAR f: FolderPtr; ra: REFANY; argv := NEW(REF AT, 3);
  BEGIN
        (* Remove the folder from the folder table. *)
    AcquireExclusive();
    TRY
      IF NOT folderTable.get(folder, ra) THEN
        RAISE Error("Folder \"" & folder & "\" not found")
      END;
      f := ra;
      LOCK scanMutex DO
        IF f.hasNews THEN DEC(newsCount) END;
        SetMustScan(f, FALSE, f.autoScan);
      END;
      FolderDelete(f);
    FINALLY
      ReleaseExclusive();
    END;
        (* Delete the directory and its contents. *)
    argv^ := AT{RmCmd, "-r", f.fullPathName};
    AppendToBackgroundQueue(argv, f, NIL, TRUE);
    RETURN f;
  END RemoveFolder;

PROCEDURE FolderExists (folder: Text.T): BOOLEAN =
  VAR ref: REFANY;
  BEGIN
    AcquireShared ();
    TRY
      RETURN folderTable.get(folder, ref);
    FINALLY
      ReleaseShared ();
    END;
  END FolderExists;

PROCEDURE FolderList (): TextList.T =
  VAR f: FolderPtr; l: TextList.T;
  BEGIN
    AcquireShared ();
    TRY
      f := folderHead;
      l := NIL;
      WHILE f # NIL DO
        l := TextList.AppendD(l, TextList.Cons(f.name,NIL));
        f := f.next;
      END;
      RETURN l
    FINALLY
      ReleaseShared ();
    END;
  END FolderList;

PROCEDURE IsBBoard (folder: Text.T): BOOLEAN =
  VAR f: FolderPtr;
  BEGIN
    IF Text.Equal (folder, NIFolderName) THEN
      RETURN FALSE
    ELSIF Text.Equal (folder, InboxName) THEN
      RETURN FALSE
    ELSE
      TRY
        f := FindFolder(folder);
        RETURN f.bBoard;
      EXCEPT Error =>
        RETURN FALSE
      END;
    END;
  END IsBBoard;

PROCEDURE Incoming (folder: Text.T): BOOLEAN =
  VAR f: FolderPtr;
  BEGIN
    IF Text.Equal (folder, NIFolderName) THEN
      RETURN FALSE
    ELSIF Text.Equal (folder, InboxName) THEN
      RETURN TRUE
    ELSE
      TRY
        f := FindFolder (folder);
        RETURN f.bBoard
      EXCEPT Error =>
        RETURN FALSE
      END;
    END;
  END Incoming;


(********************)
(* Mailbox Routines *)
(********************)

PROCEDURE NewMailPoll (): INTEGER RAISES {Error} =
  VAR
    index: INTEGER;
    mailCount: INTEGER;
    f: FolderPtr;
    result: TEXT;
  BEGIN
    mailCount := 0;
    f := FindFolder(InboxName);
    LOCK incMutex DO
      result := ExecProcess(AT{FromCmd});
    END;
    index :=  -1;
    LOOP
      index := Text.FindChar(result, '\n', index + 1);
      IF index = -1 THEN EXIT END;
      INC(mailCount);
    END;
    RETURN mailCount
  END NewMailPoll;


(*****************************)
(* Bulletin Board Procedures *)
(*****************************)

PROCEDURE HasUnseen(f: FolderPtr): BOOLEAN RAISES {} =
    (* Check for messages in folder beyond "cur" *)
    (* LL = AccessFolder *)
  VAR curFromFile: INTEGER;
  BEGIN
    LOCK scanMutex DO
      EnsureCurFileRead (f);
      TRY
        curFromFile := MiscUtils.ToInt(f.curMsg);
      EXCEPT MiscUtils.BadFormat => RETURN FALSE;
      END;
    END;
    TRY
      OSUtils.CheckFile(f.fullPathName & "/" & Fmt.Int(curFromFile+1));
      RETURN TRUE
    EXCEPT OSUtils.FileNotFound, OSUtils.FileError => RETURN FALSE
    END;
  END HasUnseen;

PROCEDURE HasNews (f: FolderPtr): BOOLEAN RAISES {} =
    (* If we have never considered news for this folder, check the folder itself
       (because messages might have been left there by ScanDaemon in an earlier
       run of Postcard, for example.  Otherwise, just check whether there is
       news for "f" in the spool. *)
    (* LL = 0 *)
  VAR
    newsDirMTime, purgeMTime: Time.T;
    has: BOOLEAN;
    rd: Rd.T;
    lastlinkNum: INTEGER;
    contents: TextList.T;
  BEGIN
    has := FALSE;
    AccessFolder(f);
    TRY
      IF HasUnseen(f) THEN has := TRUE END;
    FINALLY
      FinishAccessFolders(f);
    END;
    (* Look at the news spool.  If it's not accessible, "has" is our best guess.
       Otherwise, if the news spool has changed since we last considered the
       matter (or if llscan hasn't yet processed it and not "has"), then we
       must examine the spool to see if there are new messages in it. *)
    TRY
      EVAL GetInfo(f.newsDirName, newsDirMTime);
    EXCEPT Error => RETURN has; (* can't look in spool *)
    END;
    LOCK scanMutex DO
      IF (newsDirMTime <= f.newsDirMTime) AND (NOT f.mustScan OR has) THEN
        RETURN has
      END;
      (* If the news directory contains a file named ".purge" that was modified
         more recently than the folder, then it is a SRC-style bulletin board
         that has been reorganized. Set flag that will cause a purge to occur
         the next time the folder is opened. *)
      TRY
        EVAL GetInfo(f.newsDirName & "/.purge", purgeMTime);
        IF purgeMTime > f.mtime THEN
          f.doPurge := TRUE;
        END;
      EXCEPT Error => (* ignore *)
      END;
    END;
    (* Say yes if there is a news message numbered higher than the value saved
       in the ".lastlink" file; otherwise the modification must have been an
       expiration of old messages. (If no ".lastlink" file exists, say yes so
       a scan will occur. *)
    TRY
      rd := OSUtils.OpenRead(f.fullPathName & "/.lastlink");
    EXCEPT
      | OSUtils.FileError => RETURN has;
      | OSUtils.FileNotFound => RETURN TRUE;
    END;
    TRY
      TRY
        lastlinkNum := MiscUtils.ToInt(Rd.GetText(rd, 25));
      FINALLY
        Rd.Close(rd)
      END;
    EXCEPT MiscUtils.BadFormat => RETURN TRUE; (* need to scan *)
    END;
    TRY
      contents := OSUtils.Enumerate(f.newsDirName);
    EXCEPT OSUtils.FileError => RETURN has; (* try again some other day *)
    END;
    WHILE contents # NIL DO
      TRY
        IF MiscUtils.ToInt(contents.head) > lastlinkNum THEN
          RETURN TRUE;
        END;
      EXCEPT MiscUtils.BadFormat => (* ignore garbage, including ".." *)
      END;
      contents := contents.tail;
    END;
    LOCK scanMutex DO
      f.newsDirMTime := newsDirMTime;
    END;
    RETURN has
  END HasNews;

PROCEDURE GetNextBBoard(b: FolderPtr; checkNewsDir: BOOLEAN;
                        VAR (* out *) folder: Text.T;
                        VAR (* out *) hasNews: BOOLEAN): FolderPtr RAISES {} =
  VAR f: FolderPtr;
  BEGIN
    f := b;
    LOOP
      AcquireShared ();
      TRY
        IF f = NIL THEN
          f := folderHead;
        ELSE
          f := f.next
        END;
      FINALLY
        ReleaseShared ();
      END;
      IF f = NIL THEN
        folder := NIL;
        hasNews := FALSE;
        RETURN NIL
      ELSIF f.bBoard THEN
        IF checkNewsDir THEN
          hasNews := HasNews(f);
          LOCK scanMutex DO
            IF hasNews THEN
              IF NOT f.hasNews THEN
                f.hasNews := TRUE;
                INC(newsCount);
              END;
              SetMustScan(f, TRUE, f.autoScan);
            ELSIF f.hasNews THEN
              f.hasNews := FALSE;
              DEC(newsCount);
            END;
          END;
          Thread.Pause(1.0d0); (* only used in background thread *)
        ELSE
          LOCK scanMutex DO hasNews := f.hasNews END;
        END;
        folder := f.name;
        RETURN f
      END;
    END; (* LOOP *)
  END GetNextBBoard;

PROCEDURE GetNewsCount (): CARDINAL =
  BEGIN
    LOCK scanMutex DO RETURN newsCount;  END;
  END GetNewsCount;


(***********************)
(* Cur File Procedures *)
(***********************)

PROCEDURE EnsureCurFileRead (f: FolderPtr) =
    (* LL = scanMutex *)
  VAR rd: Rd.T;
  BEGIN
    IF (Text.Length(f.curMsg) = 0) OR (NOT f.checkpoint) THEN
      TRY
        rd := OpenRead(f.fullPathName & "/cur");
        TRY
          f.curMsg := Rd.GetLine(rd);
        FINALLY
          Rd.Close (rd);
        END;
      EXCEPT Error, Rd.Failure, Rd.EndOfFile => (* ignore *)
      END;
    END;
  END EnsureCurFileRead;

PROCEDURE WriteCurFile(f: FolderPtr) =
    (* LL = scanMutex *)
  VAR wr: Wr.T;
  BEGIN
    TRY
      wr := OpenWrite(f.fullPathName & "/cur", FALSE);
      TRY
        Wr.PutText (wr, f.curMsg & "\n");
      FINALLY
        Wr.Close (wr);
      END;
    EXCEPT Error, Wr.Failure => (* ignore *)
    END;
  END WriteCurFile;

PROCEDURE CheckpointCurFiles () =
  VAR f: FolderPtr; checkPoint: BOOLEAN;
  BEGIN
    AcquireShared ();
    TRY
      f := folderHead;
      WHILE f # NIL DO
        LOCK scanMutex DO
          IF f.checkpoint THEN
            checkPoint := TRUE;
            f.checkpoint := FALSE;
            DEC(checkpointNeeded);
            WriteCurFile(f);
          ELSE
            checkPoint := FALSE;
          END;
        END;
        IF checkPoint THEN Thread.Pause (0.010d0) END;
        f := f.next;
      END;
    FINALLY
      ReleaseShared ();
    END;
  END CheckpointCurFiles;

PROCEDURE SetCurrent(folder, msgID: Text.T) RAISES {} =
  VAR f: FolderPtr;
  BEGIN
    IF NOT Text.Equal(folder, NIFolderName) THEN
      TRY
        f := FindFolder(folder);
        LOCK scanMutex DO
          f.curMsg := msgID;
          IF Text.Equal(folder, InboxName) OR f.bBoard THEN
            WriteCurFile(f);
          ELSE
            f.checkpoint := TRUE;
            INC(checkpointNeeded);
            Thread.Signal(checkpointCV);
          END;
        END;
      EXCEPT Error =>
        (* no such folder; ignore *)
      END;
    END;
  END SetCurrent;

PROCEDURE CheckpointThread(<*UNUSED*> self: Thread.Closure): REFANY =
  BEGIN
    LOOP
      LOCK scanMutex DO
        WHILE checkpointNeeded = 0 DO
          Thread.Wait(scanMutex, checkpointCV);
        END;
      END;
      AcquireShared();
      TRY CheckpointCurFiles() FINALLY ReleaseShared() END;
      Thread.Pause(10.0d0);
    END;
  END CheckpointThread;


(**************************)
(* Folder Scan Procedures *)
(**************************)

PROCEDURE SetMustScan(f: FolderPtr; must, doAuto: BOOLEAN) RAISES {} =
    (* LL >= scanMutex *)
    (* Set f.mustScan and f.autoScan; adjust scanNeeded as appropriate. *)
  BEGIN
    f.mustScan := must;
    f.autoScan := doAuto;
    IF must AND doAuto THEN
      scanNeeded := TRUE;
      Thread.Signal(scanDaemonCV);
    END;
  END SetMustScan;
  
PROCEDURE FolderTwiddled (f: FolderPtr): BOOLEAN RAISES {Error} =
    (* Do we need to call llscan? *)
    (* LL = AccessFolder *)
  VAR mtime: Time.T;
  BEGIN
    LOCK scanMutex DO
      IF f.bBoard AND f.doPurge THEN RETURN TRUE END;
      IF NOT f.mustScan THEN
        EVAL GetInfo(f.fullPathName, mtime);
        IF mtime > f.mtime THEN
          SetMustScan(f, TRUE, f.autoScan);
        END;
      END;
      RETURN f.mustScan
    END;
  END FolderTwiddled;

PROCEDURE DoPurge (f: FolderPtr) RAISES { Error } =
    (* LL = AccessFolder *)
    (* LLScanCmd -p; MHFolderCmd -pack *)
  BEGIN
    EVAL ExecProcess(AT{LLScanCmd, "-p", "+" & f.name});
    EVAL ExecProcess(AT{MHFolderCmd, "-pack", "+" & f.name});
  END DoPurge;

PROCEDURE DoLLScan (f: FolderPtr; withInc: BOOLEAN) RAISES {Error} =
    (* Invoke llscan. *)
    (* LL = AccessFolder *)
  VAR
    doPurge: BOOLEAN;
    folderMTime, purgeMTime: Time.T;
  BEGIN
        (* Be sure we have "cur" file *)
    LOCK scanMutex DO EnsureCurFileRead (f) END;
        (* Enforce ".purge" mechanism *)
    IF f.bBoard THEN
      LOCK scanMutex DO doPurge := f.doPurge; f.doPurge := FALSE;  END;
      IF doPurge THEN
        (* We must first do an "llscan -p" to remove links to nonexistent
           files and a "folder +pack" to renumber the remaining links. *)
        DoPurge (f);
        (* The code below will then do another "llscan" to rebuild the scan
           file. *)
      END;
    END;
    LOCK scanMutex DO
      SetMustScan(f, FALSE, f.autoScan);
    END;
    TRY
      IF withInc THEN
        LOCK incMutex DO
          EVAL ExecProcess(AT{LLScanCmd, "-i", "+" & f.name});
        END;
      ELSE
        EVAL ExecProcess(AT{LLScanCmd, "+" & f.name});
      END;
    EXCEPT Error(t) =>
      LOCK scanMutex DO SetMustScan(f, TRUE, f.autoScan) END;
      RAISE Error(t);
    END;
        (* llscan unfortunately changed "cur" ... *)
    LOCK scanMutex DO
      IF Text.Length(f.curMsg) # 0 THEN WriteCurFile(f) END;
    END;
    TRY
      EVAL GetInfo(f.fullPathName, folderMTime);
      LOCK scanMutex DO
        IF f.bBoard THEN
          TRY
            EVAL GetInfo(f.newsDirName & "/.purge", purgeMTime);
            IF purgeMTime > f.mtime THEN
              f.doPurge := TRUE;
            END;
          EXCEPT Error => (* assume lookup or remote file error *)
          END;
        END;
        f.mtime := folderMTime;
      END;
    EXCEPT Error => (* ignore *)
    END;
  END DoLLScan;

PROCEDURE GetMsgList(folder: Text.T; forceScan, withInc: BOOLEAN;
                     VAR curMsgId: TEXT): Rd.T
                     RAISES {Error} =
  VAR f: FolderPtr; check: BOOLEAN; scanFileName: Text.T; rd: Rd.T;
  BEGIN
    f := FindFolder(folder);
    LOCK scanMutex DO check := NOT(f.hasNews) END;
    IF f.bBoard AND check AND HasNews(f) THEN
      LOCK scanMutex DO SetMustScan(f, TRUE, f.autoScan) END;
    END;
    AccessFolder(f);
    TRY
      LOCK scanMutex DO
        EnsureCurFileRead(f);
        curMsgId := f.curMsg;
        IF f.bBoard THEN
          IF f.hasNews THEN f.hasNews := FALSE; DEC(newsCount); END;
        END;
      END;
      IF forceScan OR withInc OR FolderTwiddled(f) THEN
        DoLLScan(f, withInc);
      END;
        (* get the actual message list *)
      scanFileName := Text.Cat (f.fullPathName, "/.inodecache");
      rd := OpenRead(scanFileName);
      LOCK scanMutex DO
        SetMustScan(f, f.mustScan, TRUE);
      END;
      RETURN rd
    FINALLY
      FinishAccessFolders (f);
    END;
  END GetMsgList;

PROCEDURE GetInodecache (folder: Text.T): Rd.T RAISES {Error} =
  VAR f: FolderPtr; scanFileName: Text.T;
  BEGIN
    f := FindFolder (folder);
    AccessFolder (f);
    TRY
      IF ( NOT f.bBoard) AND ( NOT Text.Equal (folder, InboxName))
        AND FolderTwiddled (f) THEN
        DoLLScan (f, FALSE);
      END;
      scanFileName := Text.Cat (f.fullPathName, "/.inodecache");
      RETURN OpenRead(scanFileName);
    FINALLY
      FinishAccessFolders (f);
    END;
  END GetInodecache;

PROCEDURE PurgeFolder (folder: Text.T; lastToDelete: TEXT) RAISES {Error} =
  VAR
    f: FolderPtr;
  BEGIN
    DrainBackgroundQueue ();
    f := FindFolder (folder);
    AccessFolder (f);
    TRY
      LOCK scanMutex DO SetMustScan(f, TRUE, f.autoScan) END;
      Thread.Signal(scanDaemonCV);
      EVAL ExecProcess(AT{MHRmmCmd, "+" & folder,
                          "1-" & MailUtilities.LTrim(lastToDelete)});
      (* Do an llscan -p in case this is a bboard, then pack the messages. *)
      DoPurge (f);
      LOCK scanMutex DO
        TRY
          OSUtils.Delete(f.fullPathName & "/cur");
        EXCEPT OSUtils.FileError => (* ignore *)
        END;
        f.curMsg := "";
      END;
    FINALLY
      FinishAccessFolders (f);
    END;
  END PurgeFolder;

PROCEDURE BackgroundScanning (on: BOOLEAN) =
  BEGIN
    LOCK scanMutex DO runScanDaemon := on; Thread.Signal(scanEnableCV);  END;
  END BackgroundScanning;

PROCEDURE ScanDaemon(<*UNUSED*> self: Thread.Closure): REFANY =
    (* Background thread to invok llscan while user isn't looking *)
  VAR f: FolderPtr; mustScan: BOOLEAN;
  BEGIN
    f := NIL;
    LOOP
      LOCK scanMutex DO
        IF f = NIL THEN
          (* start-up, or we've considered all the folders *)
          WHILE NOT scanNeeded DO Thread.Wait(scanMutex, scanDaemonCV) END;
          scanNeeded := FALSE;
        END;
        WHILE NOT runScanDaemon DO Thread.Wait(scanMutex, scanEnableCV) END;
      END;
      IF f = NIL THEN
        AcquireShared ();
        f := folderHead;
        ReleaseShared ();
      ELSE
        AcquireShared ();
        f := f.next;
        ReleaseShared ();
      END;
      IF f # NIL THEN
        LOCK scanMutex DO
          mustScan := (f.mustScan AND f.autoScan);
        END;
        IF mustScan THEN
          AccessFolder (f);
          TRY
            TRY
              DoLLScan(f, FALSE);
            EXCEPT Error =>
              LOCK scanMutex DO
                SetMustScan(f, f.mustScan, FALSE);
              END;
              (* disables auto scan until next successful open *)
            END;
          FINALLY
            FinishAccessFolders (f);
          END;
          Thread.Pause(1.0d0); (* We're background, remember *)
        END;
      END;
    END;
  END ScanDaemon;


(*****************)
(* NI procedures *)
(*****************)

TYPE NIPrivate = NI OBJECT
    child: OSUtils.SubProcess;
  OVERRIDES
    getCount := GetNICount;
    kill := KillNI;
  END;

PROCEDURE StartNI(): NI RAISES {Error} =
    (* starts up "ni" sub-program; delivers rd/wr piped to "ni" *)
  VAR
    ni := NEW(NIPrivate);
    filter := RunProcess(AT{"ni"});
  BEGIN
    ni.child := filter.handle;
    ni.wr := filter.stdin;
    ni.rd := filter.stdout; (* includes stderr *)
    RETURN ni
  END StartNI;

PROCEDURE KillNI(ni: NIPrivate) =
  BEGIN
    TRY Wr.Close(ni.wr) EXCEPT Wr.Failure => END;
    TRY Rd.Close(ni.rd) EXCEPT Rd.Failure => END;
    EVAL OSUtils.WaitProcess(ni.child);
  END KillNI;

PROCEDURE GetNICount(ni: NIPrivate): INTEGER RAISES {Error} =
    (* Read and parse a message count response from NI *)
  VAR line: Text.T;
  VAR nMsgs: INTEGER;
  BEGIN
    TRY
      line := Rd.GetLine(ni.rd);
    EXCEPT
      | Rd.Failure =>
          RAISE Error("Broken pipe from ni during GetNICount");
      | Rd.EndOfFile =>
          RAISE Error("EOF on pipe from ni during GetNICount");
    END;
    IF Text.GetChar(line, 0) = '?' THEN
      RAISE Error(Text.Sub(line, 1, LAST(CARDINAL)))
    ELSE
      TRY
        nMsgs := MiscUtils.ToInt(line);
      EXCEPT MiscUtils.BadFormat =>
        RAISE Error("Expected integer from NI process, received:\n" & line);
      END;
      RETURN nMsgs
    END;
  END GetNICount;


(******************)
(* Message Access *)
(******************)

PROCEDURE MsgPath (folder, msgID: Text.T): Text.T =
  VAR f: FolderPtr;
  BEGIN
    IF Text.Equal (folder, NIFolderName) THEN
      RETURN Text.Cat (newsSpoolName & "/", msgID)
    ELSE
      <*FATAL Error*> BEGIN f := FindFolder (folder); END;
      RETURN f.fullPathName & "/" & MailUtilities.LTrim(msgID)
    END;
  END MsgPath;

PROCEDURE ReadFileText(fileName: TEXT): TEXT RAISES {Error} =
  VAR rd := OpenRead(fileName); t: TEXT;
  BEGIN
    TRY
      TRY
        t := Rd.GetText(rd, LAST(CARDINAL))
      FINALLY
        Rd.Close(rd);
      END;
    EXCEPT Rd.Failure => RAISE Error("Can't read \"" & fileName & "\"");
    END;
    RETURN t
  END ReadFileText;

VAR headerFilter: TextList.T;
  (* Fields included in the brief header returned by GetMsg *)

PROCEDURE GetMsg(folder, msgID: TEXT;
                 VAR (*OUT*) header, brief, body: TEXT) RAISES {Error} =
  VAR rd: Rd.T; fileName := MsgPath(folder, msgID);
  BEGIN
    TRY
      OSUtils.CheckFile(fileName);
      rd := OSUtils.OpenRead(fileName);
      TRY
        header := MailUtilities.GetHeader(rd);
        body := Rd.GetText(rd, LAST(CARDINAL));
      FINALLY
        Rd.Close(rd);
      END;
      brief := MailUtilities.FilterHeader(header, headerFilter);
    EXCEPT
      | OSUtils.FileError(why) =>
        RAISE Error("Can't open \"" & fileName & "\": " & why);
      | OSUtils.FileNotFound =>
        header := "\n";
        brief := "\n";
        body := "********----********\n\n" &
                "The message \"" & fileName & "\" doesn't exist.\n\n" &
                "Probably the message has been deleted.  If this is a news " &
                "group, the message might have expired or been cancelled.\n\n" &
                "********----********\n";
      | Rd.Failure =>
        RAISE Error("Can't read \"" & fileName & "\"");
    END;
  END GetMsg;

PROCEDURE GetHeader(folder, msgID: TEXT): TEXT RAISES { Error } =
  VAR rd: Rd.T; fileName := MsgPath(folder, msgID);
  BEGIN
    rd := OpenRead(fileName);
    TRY
      TRY
        RETURN MailUtilities.GetHeader(rd);
      FINALLY
        Rd.Close(rd);
      END;
    EXCEPT Rd.Failure => RAISE Error("Can't read \"" & fileName & "\"");
    END;
  END GetHeader;

PROCEDURE CopyMsg(srcFolder, toFolder: Text.T; msgList: TextList.T)
                  RAISES {Error} =
  VAR to: FolderPtr;
  BEGIN
    to := FindFolder(toFolder);
    IF Text.Equal(srcFolder, NIFolderName) THEN
      InvokeMHCommand(to, NIL, msgList, TRUE,
               AT{MHFileCmd, "-link", "+" & toFolder});
    ELSE
      InvokeMHCommand(to, NIL, msgList, FALSE,
               AT{MHFileCmd, "-link", "-src", "+" & srcFolder, "+" & toFolder});
    END;
  END CopyMsg;

PROCEDURE DeleteMsg(folder: Text.T; msgList: TextList.T) RAISES {Error} =
  VAR f: FolderPtr;
  BEGIN
    f := FindFolder(folder);
    InvokeMHCommand(f, NIL, msgList, FALSE,
                    AT{MHRmmCmd, "+" & folder});
  END DeleteMsg;

PROCEDURE MoveMsg(srcFolder, toFolder: Text.T; msgList: TextList.T)
                  RAISES {Error} =
  VAR from, to: FolderPtr;
  BEGIN
    from := FindFolder(srcFolder);
    to := FindFolder(toFolder);
    InvokeMHCommand(to, from, msgList,  FALSE,
                    AT{MHFileCmd, "-src", "+" & srcFolder, "+" & toFolder});
  END MoveMsg;

PROCEDURE PrintMsg(folder: Text.T; msgList: TextList.T; printFilter: Text.T)
                   RAISES {Error} =
  VAR
    numWords: CARDINAL;
    argv: REF AT;
  BEGIN
    argv := SetupFilterArgs(numWords, TextList.Length(msgList), printFilter);
        (* Copy message names to remainder of argument list. *)
    WHILE msgList # NIL DO
      argv[numWords] := MsgPath(folder, msgList.head);
      INC(numWords);
      msgList := msgList.tail;
    END;
    IF Text.Equal (folder, NIFolderName) THEN
      AppendToBackgroundQueue(argv, NIL, NIL, FALSE);
    ELSE
      AppendToBackgroundQueue(argv, FindFolder(folder), NIL, FALSE);
    END;
  END PrintMsg;

PROCEDURE SaveMsg(folder: Text.T; msgList: TextList.T; toFileName: Text.T)
                  RAISES {Error} =
  VAR
    rd: Rd.T; wr: Wr.T;
    len: CARDINAL;
    buff: ARRAY [0..1023] OF CHAR;
  BEGIN
    wr := OpenWrite(toFileName, TRUE);
    TRY
      TRY
        WHILE msgList # NIL DO
          rd := OpenRead(MsgPath(folder, msgList.head));
          TRY
            TRY
              WHILE NOT Rd.EOF(rd) DO
                len := Rd.GetSub(rd, buff);
                Wr.PutString(wr, SUBARRAY(buff, 0, len));
              END;
            FINALLY
              Rd.Close(rd);
            END;
          EXCEPT Rd.Failure =>
            RAISE Error("Can't read \"" & msgList.head & "\"");
          END;
          msgList := msgList.tail;
        END;
      FINALLY
        Wr.Close(wr)
      END;
    EXCEPT Wr.Failure =>
      RAISE Error("Error while saving into \"" & toFileName & "\"");
    END;
  END SaveMsg;

PROCEDURE MsgSubject (folder, msgID: Text.T): Text.T RAISES {Error} =
  BEGIN
    RETURN MailUtilities.LTrim(
             MailUtilities.GetFieldValue(GetHeader(folder, msgID), "Subject"))
  END MsgSubject;

PROCEDURE ReplyMsg(folder, msgID: Text.T;
                   replyToSender, ccToMe: BOOLEAN;
                   includeMessageInDraft: BOOLEAN; includeReplyString: Text.T;
                   VAR (* out *) to, cc, subject, inReplyTo, body: Text.T)
                   RAISES {Error} =
  VAR
    user, msgHeader, sender: Text.T;
    toField, replyToField, fromField, ccField, subjField: Text.T;
    msgidField, dateField: Text.T;
  PROCEDURE GetValue(name: TEXT): TEXT =
    BEGIN
      RETURN MailUtilities.LTrim(MailUtilities.GetFieldValue(msgHeader, name))
    END GetValue;
  BEGIN
    to := "";
    cc := "";
    subject := "";
    inReplyTo := "";
    body := "";
    user := Env.Get("USER");
    IF user = NIL THEN user := "" END;
    msgHeader := GetHeader(folder, msgID);
    replyToField := GetValue("Reply-To");
    fromField := GetValue("From");
    toField := GetValue("FollowUp-To");
    IF Text.Empty(toField) THEN toField := GetValue("To") END;
    IF Text.Empty(toField) THEN toField := GetValue("NewsGroups") END;
    ccField := GetValue("Cc");
    msgidField := GetValue("Message-Id");
    subjField := GetValue("Subject");
    dateField := GetValue("Date");
    IF NOT Text.Empty(replyToField) THEN
      sender := replyToField;
    ELSE
      sender := fromField;
    END;
    IF replyToSender THEN
      to := sender;
      IF ccToMe THEN cc := user END;
    ELSE (* reply to all or to bboard only  *)
      IF Text.Equal(folder, NIFolderName) THEN
        to := toField;
        cc := ccField;
          (* omit sender because sender presumably reads this bboard *)
        IF ccToMe THEN cc := MailUtilities.ConcatText(cc, user, ", ");  END;
      ELSIF IsBBoard(folder) THEN
        to := toField;
        cc := ccField;
          (* omit sender and ccToMe because they presumably read this bboard *)
      ELSE
        to := sender;
        cc := MailUtilities.ConcatText(toField, ccField, ", ");
          (* Ignore ccToMe since we assume we are already included. *)
      END;
    END;
    IF (MiscUtils.Find(subjField, 0, "Re:", TRUE) < 0) THEN
      subject := "Re: ";
    END;
    subject := subject & subjField;
    inReplyTo := "Message of " & dateField & "\n    from " & sender;
    inReplyTo := inReplyTo & "\n    " & msgidField;
    IF includeMessageInDraft THEN
      body := IncludeMsg(folder, msgID, includeReplyString);
    END;
  END ReplyMsg;

PROCEDURE ForwardMsg(folder: Text.T; msgList: TextList.T; ccToMe: BOOLEAN;
                     VAR (* out *) cc, body: Text.T) RAISES {Error} =
  VAR n: INTEGER; plural: Text.T;
  BEGIN
    IF ccToMe THEN
      cc := Env.Get("USER");
      IF cc = NIL THEN cc := "" END;
    ELSE
      cc := "";
    END;
    body := "";
    n := TextList.Length(msgList);
    IF n = 0 THEN RETURN ;  END;
    IF n > 1 THEN plural := "s";  ELSE plural := "";  END;
    FOR i := 1 TO n DO
      IF i = 1 THEN
        body := "\n\n------- Forwarded Message" & plural & "\n\n";
      ELSE
        body := body & "\n\n------- Message " & Fmt.Int(i) & "\n\n";
      END;
      body := body & IncludeMsg(folder, msgList.head, "");
      msgList := msgList.tail;
    END;
    body := body & "\n------- End of Forwarded Message" & plural & "\n";
  END ForwardMsg;

VAR forwardFilter: TextList.T; (* fields for forwarded headeer *)

PROCEDURE IncludeMsg (folder, msgID, prefix: Text.T): TEXT RAISES {Error} =
  VAR fileName := MsgPath(folder, msgID);
  VAR rd, rd2: Rd.T; header, line: Text.T; wr: Wr.T;
  BEGIN
    rd := OpenRead(fileName);
    wr := TextWr.New();
    TRY
      TRY
        <*FATAL Rd.EndOfFile*> BEGIN
          header := MailUtilities.FilterHeader(MailUtilities.GetHeader(rd),
                                               forwardFilter);
          rd2 := TextRd.New(header);
          WHILE NOT Rd.EOF(rd2) DO
            line := Rd.GetLine(rd2);
            Wr.PutText(wr, prefix & line & "\n");
          END;
          Rd.Close(rd2);
          WHILE NOT Rd.EOF(rd) DO
            line := Rd.GetLine(rd);
            Wr.PutText(wr, prefix & line & "\n");
          END;
        END;
      FINALLY
        Rd.Close (rd);
      END;
    EXCEPT Rd.Failure =>
      RAISE Error("Error while reading \"" & fileName & "\"");
    END;
    RETURN TextWr.ToText(wr)
  END IncludeMsg;


(***********)
(* Sending *)
(***********)

PROCEDURE DraftFileName(draftFile: CARDINAL; prefix: TEXT := ""): TEXT =
  (* Returns full path name of draft file *)
  BEGIN
    RETURN draftDirName & "/" & prefix & Fmt.Int(draftFile)
  END DraftFileName;
  
PROCEDURE GetOldDrafts(): REF ARRAY OF CARDINAL RAISES { Error } =
    VAR names, this: TextList.T; count: CARDINAL; res: REF ARRAY OF CARDINAL;
  BEGIN
    TRY
      OSUtils.CheckFile(draftDirName);
      names := OSUtils.Enumerate(draftDirName);
    EXCEPT
      | OSUtils.FileNotFound =>
          RETURN NEW(REF ARRAY OF CARDINAL,0);
      | OSUtils.FileError(t) =>
          RAISE Error("Can't enumerate " & draftDirName & ": " & t);
    END;
    count := 0;
    this := names;
    WHILE this # NIL DO
      TRY
        EVAL MiscUtils.ToInt(this.head);
        INC(count);
      EXCEPT MiscUtils.BadFormat =>
      END;
      this := this.tail;
    END;
    res := NEW(REF ARRAY OF CARDINAL, count);
    count := 0;
    this := names;
    WHILE this # NIL DO
      TRY
        res[count] := MiscUtils.ToInt(this.head);
        INC(count);
      EXCEPT MiscUtils.BadFormat =>
      END;
      this := this.tail;
    END;
    RETURN res
  END GetOldDrafts;

PROCEDURE DraftFileTime(draftFile: CARDINAL): Time.T RAISES { Error } =
  VAR mtime: Time.T;
  BEGIN
    EVAL GetInfo(DraftFileName(draftFile), mtime);
    RETURN mtime
  END DraftFileTime;

PROCEDURE ReadDraftFile(draftFile: CARDINAL): TEXT RAISES { Error } =
  BEGIN
    RETURN ReadFileText(DraftFileName(draftFile))
  END ReadDraftFile;

PROCEDURE WriteDraftFile(msg: TEXT; draftFile: CARDINAL) RAISES {Error} =
  VAR
    wr: Wr.T;
  BEGIN
    (* create draftDirName if it doesn't exist. *)
    TRY
      TRY
        OSUtils.CheckFile(draftDirName);
      EXCEPT OSUtils.FileNotFound => OSUtils.MakeDir(draftDirName)
      END;
    EXCEPT OSUtils.FileError(t) =>
      RAISE Error("Can't create " & draftDirName & ": " & t);
    END;
    TRY
      wr := OpenWrite(DraftFileName(draftFile), FALSE);
      TRY
        Wr.PutText(wr, msg);
      FINALLY
        Wr.Close(wr);
      END;
    EXCEPT Wr.Failure =>
        RAISE Error("Error while saving draft into \"" &
                    DraftFileName(draftFile) & "\"");
    END;
  END WriteDraftFile;

PROCEDURE DiscardDraftFile(draftFile: CARDINAL) RAISES { Error } =
  VAR draftName := DraftFileName(draftFile);
  BEGIN
    TRY
      OSUtils.CheckFile(draftName);
      OSUtils.Rename(draftName, DraftFileName(draftFile, ","))
    EXCEPT
      | OSUtils.FileError(t) =>
          RAISE Error("Can't rename " & draftName & ": " & t);
      | OSUtils.FileNotFound => (* do nothing *)
    END;
  END DiscardDraftFile;

PROCEDURE SendMsg(draftFile: CARDINAL) RAISES {Error} =
  BEGIN
    EVAL ExecProcess(AT{MHSendCmd, DraftFileName(draftFile)});
  END SendMsg;


(******************)
(* Initialization *)
(******************)

TYPE InitPhase = {notStarted, inProgress, complete};

VAR
  initPhase: InitPhase;
  initThread: Thread.T;


PROCEDURE BeginInit(userParam, homeParam: Text.T; handler: ErrorHandler) =
  BEGIN
    ASSERT (initPhase = InitPhase.notStarted);
    initPhase := InitPhase.inProgress;
    newsSpoolName := Env.Get("NEWSDROP");
    IF newsSpoolName = NIL THEN
      newsSpoolName := "/usr/spool/news";
    END;
    mailboxName := Env.Get("MAILDROP");
    IF mailboxName = NIL THEN
      mailboxName := "/usr/spool/mail/" & userParam;
    END;
    mailDirName := homeParam & "/Mail"; (*TEMP: consider mh_profile "path:" *)
    draftDirName := homeParam & "/.pc_draft";
    folderLock := 0;
    folderHead := NIL;
    folderTail := NIL;
    folderTable := NEW(TextRefTbl.Default).init();
    runScanDaemon := TRUE;
    scanNeeded := FALSE;
    checkpointNeeded := 0;
    newsCount := 0;
    headerFilter := TextList.AppendD(
                      TextList.List3("From", "Date", "To"),
                      TextList.List3("X-URL", "cc", "Subject"));
    forwardFilter := TextList.Append(
                      headerFilter,
                      TextList.AppendD(
                        TextList.List3("Newsgroups", "Sender", "Reply-To"),
                        TextList.List2("Message-ID", "In-Reply-To")));
    InitBackgroundQueue(handler);
        (* Setting up the folders takes several seconds. *)
    initThread := Thread.Fork(
            NEW(Thread.Closure, apply := FolderInit));
        (* But meanwhile, we can scan folders as we find them *)
    scanDaemonThread := Thread.Fork(
            NEW(Thread.Closure, apply := ScanDaemon));
        (* And the checkpoint thread is harmless *)
    checkpointThread := Thread.Fork(
            NEW(Thread.Closure, apply := CheckpointThread));
  END BeginInit;

PROCEDURE CompleteInit () =
  BEGIN
    ASSERT (initPhase = InitPhase.inProgress);
    initPhase := InitPhase.complete;
    IF Thread.Join (initThread) = NIL THEN END;
  END CompleteInit;


(***********)
(* Cleanup *)
(***********)

PROCEDURE CleanUp () =
  BEGIN
    DrainBackgroundQueue ();
    CheckpointCurFiles ();
  END CleanUp;


EXCEPTION ASSERT_FAILED (Text.T);

<*INLINE*> PROCEDURE ASSERT (b: BOOLEAN; msg: Text.T := "") =
  <*FATAL ASSERT_FAILED*>
  BEGIN
    IF NOT b THEN RAISE ASSERT_FAILED (msg) END;
  END ASSERT;

BEGIN
  initPhase := InitPhase.notStarted;
END UnixMail.
