(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Thu Nov 10 15:31:36 PST 1994 by isard      *)

UNSAFE MODULE M3LoaderProcess;

IMPORT WinBase, WinNT, WinDef, Word, IO, Fmt, Text, RTMisc;
IMPORT ThreadContext, IntRefTbl, Ctypes, Cstring, Wr, Thread, Stdio;
IMPORT M3Buf, M3ID, M3toC, M3LoaderDebug AS Debug, M3LoaderAccess;
IMPORT M3Loader, M3LoaderObj;

FROM M3Loader IMPORT ObjList;
FROM M3LoaderAccess IMPORT Buffer, Segment, SegType;

REVEAL
  T = Public BRANDED "M3LoaderProcess.T" OBJECT
      memdata       : ARRAY SegType OF MemData;
      sysinfo       : WinBase.LPSYSTEM_INFO;
      commblock     : INTEGER;
      buffer        : M3Buf.T := NIL;
      process       : WinBase.PROCESS_INFORMATION;
      execname      : ARRAY BOOLEAN OF TEXT;
      debug_event   : WinBase.DEBUG_EVENT;
      debug_ptr     : WinBase.LPDEBUG_EVENT;
      threadtable   : IntRefTbl.Default;
      proctable     : IntRefTbl.Default;
      objs          : ObjList;
      exename       : TEXT;
      still_running := FALSE;
      saved_initial : Buffer := NIL;
    OVERRIDES
      allocate := allocate;
      free := free;
      start := start;
      call := call;
      kill_off_last := kill_off_last;
      initialise_data := initialise_data;
      restore_data := restore_data;
    END;

EXCEPTION
  ProcessCreateError;

TYPE
  FreeList = REF RECORD
    address : INTEGER;
    size    : INTEGER;
    free    : BOOLEAN;
    next    : FreeList;
  END;

TYPE
  MemData = REF RECORD
    filemap       : ARRAY [0 .. 63] OF WinNT.HANDLE;
    segsize       : INTEGER;
    name          : TEXT := NIL;
    start         : INTEGER;
    highcommitted : INTEGER := 0;
    size          : INTEGER := 0;
    nfullfiles    : INTEGER := 0;
    freelist      : FreeList := NIL;
  END;

TYPE
  CallStruct = RECORD
    mem  : ARRAY [0 .. 2] OF MemStruct;
    call : WinDef.DWORD;
  END;

TYPE
  MemStruct = RECORD
    start, size: WinDef.DWORD;
    name: ARRAY [0 .. 63] OF [0 .. 255];
  END;

PROCEDURE error (msg: TEXT) =
  <* FATAL Wr.Failure, Thread.Alerted *>
  BEGIN
    Wr.PutText(Stdio.stderr, msg & "\n");
  END error;

PROCEDURE fill_in_mem (VAR mem: MemStruct; READONLY data: MemData) =
  BEGIN
    mem.start := data.start;
    mem.size := data.size;
    EVAL Cstring.strcpy(LOOPHOLE(ADR(mem.name[0]), Ctypes.char_star),
                        M3toC.TtoS(data.name));
  END fill_in_mem;

PROCEDURE initialise_data (t: T) =
  BEGIN
    t.saved_initial := NEW(Buffer, t.memdata[SegType.Data].size);
    RTMisc.Copy(LOOPHOLE(t.memdata[SegType.Data].start, ADDRESS),
                ADR(t.saved_initial[0]),
                t.memdata[SegType.Data].size);
  END initialise_data;

PROCEDURE restore_data (t: T) =
  BEGIN
    IF t.saved_initial = NIL THEN RETURN END;

    IF t.still_running THEN
      kill_off_last(t);
    END;

    RTMisc.Copy(ADR(t.saved_initial[0]),
                LOOPHOLE(t.memdata[SegType.Data].start, ADDRESS),
                t.memdata[SegType.Data].size);
    t.saved_initial := NIL;
  END restore_data;

PROCEDURE call (t: T; addr: INTEGER; objs: ObjList) =
  VAR
    callstruct : CallStruct;
    check      : INTEGER;
  BEGIN
    fill_in_mem(callstruct.mem[0], t.memdata[SegType.Text]);
    fill_in_mem(callstruct.mem[1], t.memdata[SegType.Data]);
    fill_in_mem(callstruct.mem[2], t.memdata[SegType.Bss]);
    callstruct.call := addr;

    t.objs := objs;

    IF WinBase.WriteProcessMemory(t.process.hProcess,
                                  LOOPHOLE(t.commblock, ADDRESS),
                                  ADR(callstruct),
                                  BYTESIZE(CallStruct),
                                  ADR(check)) = 0 THEN
      error("Can't communicate with stub process");
      RETURN;
    END;

    IF check # BYTESIZE(CallStruct) THEN
      error("Can't communicate with stub process");
      RETURN;
    END;

    IF restart_process(t) THEN
      t.still_running := wait_for_debug_event(t, -1, exit_ok := FALSE,
                                              break_ok := TRUE);
    END
  END call;

PROCEDURE handle_error (t: T) =
  VAR
    debug_exception : WinBase.LPDEBUG_EXCEPTION;
    crashaddr       : INTEGER;
  BEGIN
    debug_exception := LOOPHOLE(t.debug_ptr, WinBase.LPDEBUG_EXCEPTION);

    IO.Put("Process ended with exception ");
    IO.Put
     (exception_text(debug_exception.Exception.ExceptionRecord));
    IO.Put(" at address ");
    crashaddr :=
      LOOPHOLE(debug_exception.Exception.ExceptionRecord.ExceptionAddress,
               INTEGER);
    IO.Put(Fmt.Pad(Fmt.Unsigned(crashaddr), 8, '0'));
    IO.Put("\n");

    backtrace(t, crashaddr);

    t.still_running := TRUE;

    IO.Put("(Type 'debug' to start windbg on the loader process)\n");
  END handle_error;

PROCEDURE exception_text (READONLY record: WinNT.EXCEPTION_RECORD): TEXT =
  BEGIN
    CASE record.ExceptionCode OF
      WinBase.EXCEPTION_ACCESS_VIOLATION =>
        VAR text := "Access Violation - attempted to ";
        BEGIN
          IF record.ExceptionInformation[0] = 0 THEN
            text := text & "read location ";
          ELSE
            text := text & "write location ";
          END;
          text := text & Fmt.Pad(Fmt.Unsigned(record.ExceptionInformation[1]),
                                 8, '0');
          RETURN text;
        END;
    | WinBase.EXCEPTION_DATATYPE_MISALIGNMENT =>
        RETURN "Datatype Misalignment";
    | WinBase.EXCEPTION_BREAKPOINT =>
        RETURN "Debugger Breakpoint";
    | WinBase.EXCEPTION_SINGLE_STEP =>
        RETURN "Debugger Single Step";
    | WinBase.EXCEPTION_ARRAY_BOUNDS_EXCEEDED =>
        RETURN "Array Bounds Exceeded";
    | WinBase.EXCEPTION_FLT_DENORMAL_OPERAND =>
        RETURN "Floating Point Denormal Operand";
    | WinBase.EXCEPTION_FLT_DIVIDE_BY_ZERO =>
        RETURN "Floating Point Divide By Zero";
    | WinBase.EXCEPTION_FLT_INEXACT_RESULT =>
        RETURN "Floating Point Inexact Result";
    | WinBase.EXCEPTION_FLT_INVALID_OPERATION =>
        RETURN "Floating Point Invalid Operation";
    | WinBase.EXCEPTION_FLT_OVERFLOW =>
        RETURN "Floating Point Overflow";
    | WinBase.EXCEPTION_FLT_STACK_CHECK =>
        RETURN "Floating Point Stack Check";
    | WinBase.EXCEPTION_FLT_UNDERFLOW =>
        RETURN "Floating Point Underflow";
    | WinBase.EXCEPTION_INT_DIVIDE_BY_ZERO =>
        RETURN "Divide By Zero";
    | WinBase.EXCEPTION_INT_OVERFLOW =>
        RETURN "Overflow";
    | WinBase.CONTROL_C_EXIT =>
        RETURN "Control-C Pressed";
    ELSE
        RETURN "Unknown";
    END
  END exception_text;

PROCEDURE backtrace (t: T; firstaddr: INTEGER) =
  VAR
    context         := NEW(ThreadContext.PCONTEXT);
    Eip             := firstaddr;
    Ebp             : INTEGER;
    prochandle,
    threadhandle    : WinNT.HANDLE;
    dummy           : REFANY;
  BEGIN
    context.ContextFlags := ThreadContext.CONTEXT_CONTROL;
    IF t.threadtable.get(t.debug_ptr.dwThreadId, dummy) THEN
      threadhandle := LOOPHOLE(dummy, WinNT.HANDLE);
      IF WinBase.GetThreadContext(threadhandle,
                                  LOOPHOLE(context, ADDRESS))
          = 0 THEN
        IO.Put("Failed to get crash context\n");
        RETURN;
      ELSE
        Ebp := context.Ebp;
      END
    ELSE
      IO.Put("Unknown thread ID\n");
      RETURN;
    END;

    IF NOT t.proctable.get(t.debug_ptr.dwProcessId, dummy) THEN
      IO.Put("Unknown process ID\n");
      RETURN;
    END;

    prochandle := LOOPHOLE(dummy, WinNT.HANDLE);

    IO.Put("Call stack backtrace:\n");

    REPEAT
      show_function(t, Eip);
    UNTIL NOT get_next_function(prochandle, Ebp, Eip);
  END backtrace;

PROCEDURE get_next_function (handle: WinNT.HANDLE;
                            VAR Ebp, Eip: INTEGER): BOOLEAN =
  VAR
    stackblock : ARRAY [0 .. 1] OF INTEGER;
    nread      : INTEGER;
  BEGIN
    IF WinBase.ReadProcessMemory(handle, LOOPHOLE(Ebp, WinDef.LPVOID),
                                 LOOPHOLE(ADR(stackblock[0]), WinDef.LPVOID),
                                 8, ADR(nread)) = 0 OR nread # 8 THEN
      RETURN FALSE;
    END;

    Ebp := stackblock[0];
    Eip := stackblock[1];

    RETURN TRUE;
  END get_next_function;

PROCEDURE show_function (t: T; Eip: INTEGER) =
  VAR
    objs := t.objs;
  BEGIN
    WHILE objs # NIL DO
      IF objs.obj.show_function(Eip) THEN
        EXIT;
      END;

      objs := objs.next;
    END
  END show_function;

PROCEDURE kill_off_last (t: T) =
  BEGIN
    IF t.still_running THEN
      REPEAT
      UNTIL handle_debug_event(t, WinBase.EXIT_PROCESS_DEBUG_EVENT) =
              WinBase.EXIT_PROCESS_DEBUG_EVENT;
      EVAL restart_process(t);
      t.still_running := FALSE;
    END
  END kill_off_last;

PROCEDURE start (t: T; dlllibs: DllLib; nlibs: INTEGER;
                 cmdline, cwd, exename: TEXT; console: BOOLEAN): BOOLEAN =
  BEGIN
    kill_off_last(t);

    t.exename := exename;

    TRY
      create_process(t, cmdline, cwd, console);

      IF NOT wait_for_debug_event(t, WinBase.OUTPUT_DEBUG_STRING_EVENT,
                                  exit_ok := FALSE, break_ok := FALSE) THEN
        RAISE ProcessCreateError;
      END;

      t.commblock := debug_string_to_int(t);

      fill_commblock(t, dlllibs, nlibs);

      IF NOT restart_process(t) THEN
        RAISE ProcessCreateError;
      END;

      IF NOT wait_for_debug_event(t, WinBase.OUTPUT_DEBUG_STRING_EVENT,
                                  exit_ok := FALSE, break_ok := FALSE) THEN
        RAISE ProcessCreateError;
      END;

      get_dll_addrs(t, dlllibs);

      RETURN TRUE;
    EXCEPT
      ProcessCreateError =>
        RETURN FALSE;
    END
  END start;

PROCEDURE put_m3buf_int (buf: M3Buf.T; val: INTEGER) =
  BEGIN
    FOR i := 0 TO 3 DO
      M3Buf.PutChar(buf, VAL(Word.And(val, 16_FF), CHAR));
      val := Word.Shift(val, -8);
    END
  END put_m3buf_int;

PROCEDURE log_interesting_events(t: T; code: INTEGER) =
  VAR
    dummy : REFANY;
  BEGIN
    CASE code OF
      WinBase.CREATE_THREAD_DEBUG_EVENT =>
        VAR threadptr := LOOPHOLE(t.debug_ptr, WinBase.LPDEBUG_CREATE_THREAD);
        BEGIN
          EVAL t.threadtable.put(threadptr.dwThreadId,
                                 LOOPHOLE(threadptr.CreateThread.hThread,
                                          REFANY));
        END
    | WinBase.CREATE_PROCESS_DEBUG_EVENT =>
        VAR procptr := LOOPHOLE(t.debug_ptr, WinBase.LPDEBUG_CREATE_PROCESS);
        BEGIN
          EVAL t.proctable.put(procptr.dwProcessId,
                               LOOPHOLE(procptr.CreateProcessInfo.hProcess,
                                        REFANY));
          EVAL t.threadtable.put(procptr.dwThreadId,
                                 LOOPHOLE(procptr.CreateProcessInfo.hThread,
                                          REFANY));
        END
    | WinBase.EXIT_THREAD_DEBUG_EVENT =>
        EVAL t.threadtable.delete(t.debug_ptr.dwThreadId, dummy);
    | WinBase.EXIT_PROCESS_DEBUG_EVENT =>
        EVAL t.proctable.delete(t.debug_ptr.dwProcessId, dummy);
        EVAL t.threadtable.delete(t.debug_ptr.dwThreadId, dummy);
    ELSE
    END
  END log_interesting_events;

PROCEDURE handle_debug_event (t: T; event: INTEGER): INTEGER =
  BEGIN
    IF WinBase.WaitForDebugEvent(t.debug_ptr, WinBase.INFINITE) = 0 THEN
      error("Lost contact with stub");
      RETURN WinBase.EXIT_PROCESS_DEBUG_EVENT;
    END;

    Debug.Txt("Event ");
    Debug.Int(t.debug_ptr.dwDebugEventCode);
    Debug.NL();

    log_interesting_events(t, t.debug_ptr.dwDebugEventCode);

    IF t.debug_ptr.dwDebugEventCode # event THEN
      IF NOT restart_process(t) THEN
        error("Lost contact with stub");
        RETURN WinBase.EXIT_PROCESS_DEBUG_EVENT;
      END
    END;

    RETURN t.debug_ptr.dwDebugEventCode;
  END handle_debug_event;

PROCEDURE wait_for_debug_event (t: T; event: INTEGER;
                                exit_ok, break_ok: BOOLEAN): BOOLEAN =
  VAR
    found      : BOOLEAN;
    code       : INTEGER;
    except_ptr : WinBase.LPDEBUG_EXCEPTION;
  BEGIN
    REPEAT
      code := handle_debug_event(t, event);

      IF t.debug_ptr.dwDebugEventCode = WinBase.EXCEPTION_DEBUG_EVENT THEN
        except_ptr := LOOPHOLE(t.debug_ptr, WinBase.LPDEBUG_EXCEPTION);
      END;

      IF code = event THEN
        found := TRUE;
      ELSE
        CASE t.debug_ptr.dwDebugEventCode OF
          WinBase.EXIT_PROCESS_DEBUG_EVENT => found := TRUE;
        | WinBase.EXCEPTION_DEBUG_EVENT =>
            IF break_ok OR
               except_ptr.Exception.ExceptionRecord.ExceptionCode #
                 WinBase.EXCEPTION_BREAKPOINT THEN
              found := TRUE;
            END
        ELSE
        END
      END
    UNTIL found;

    IF t.debug_ptr.dwDebugEventCode = WinBase.EXCEPTION_DEBUG_EVENT AND
       (NOT break_ok OR except_ptr.Exception.ExceptionRecord.ExceptionCode #
          WinBase.EXCEPTION_BREAKPOINT) THEN
      handle_error(t);
    END;

    IF t.debug_ptr.dwDebugEventCode = WinBase.EXIT_PROCESS_DEBUG_EVENT AND
       NOT exit_ok THEN
      error("Stub exited unexpectedly");
      RETURN FALSE;
    END;

    RETURN TRUE;
  END wait_for_debug_event;

CONST
  DBG_EXCEPTION_NOT_HANDLED = 16_80010001;
  DBG_CONTINUE              = 16_00010002;

PROCEDURE restart_process (t: T): BOOLEAN =
  VAR
    code := DBG_CONTINUE;
  BEGIN
    IF t.debug_ptr.dwDebugEventCode = WinBase.EXCEPTION_DEBUG_EVENT THEN
      VAR except_ptr := LOOPHOLE(t.debug_ptr, WinBase.LPDEBUG_EXCEPTION);
      BEGIN
        IF except_ptr.Exception.ExceptionRecord.ExceptionCode #
             WinBase.EXCEPTION_BREAKPOINT THEN
          code := DBG_EXCEPTION_NOT_HANDLED;
        END
      END
    END;

    IF WinBase.ContinueDebugEvent(t.debug_event.dwProcessId,
                                  t.debug_event.dwThreadId,
                                  code) = 0 THEN
      RETURN FALSE;
    END;

    RETURN TRUE;
  END restart_process;

PROCEDURE debug_string_to_int (t: T): INTEGER RAISES {ProcessCreateError} =
  VAR
    debug_string : WinBase.LPDEBUG_OUTPUT_STRING;
    check        : INTEGER;
  BEGIN
    debug_string := LOOPHOLE(t.debug_ptr, WinBase.LPDEBUG_OUTPUT_STRING);

    VAR buf := NEW(Buffer, debug_string.DebugString.nDebugStringLength);
    BEGIN
      IF debug_string.DebugString.fUnicode # 0 THEN
        error("Process is talking Unicode at me");
        RAISE ProcessCreateError;
      END;

      IF WinBase.ReadProcessMemory(t.process.hProcess,
                                   debug_string.DebugString.lpDebugStringData,
                                   ADR(buf[0]),
                                   debug_string.DebugString.nDebugStringLength,
                                   ADR(check)) = 0 THEN
        error("Can't communicate with stub");
        RAISE ProcessCreateError;
      END;

      IF check # debug_string.DebugString.nDebugStringLength THEN
        error("Can't communicate with stub");
        RAISE ProcessCreateError;
      END;

      RETURN M3LoaderAccess.ascii_to_int(buf, 0, NUMBER(buf^));
    END
  END debug_string_to_int;

PROCEDURE get_dll_addrs (t: T; dlllibs: DllLib) RAISES {ProcessCreateError} =
  VAR
    totalsyms := 0;
    dlllib    := dlllibs;
    dllsym    : DllSymbol;
    buf       : Buffer;
    ptr       : INTEGER;
    check     : INTEGER;
  BEGIN
    WHILE dlllib # NIL DO
      INC(totalsyms, dlllib.nsyms);
      dlllib := dlllib.next;
    END;

    buf := NEW(Buffer, totalsyms*4);
    IF WinBase.ReadProcessMemory(t.process.hProcess,
                                 LOOPHOLE(t.commblock+4, ADDRESS),
                                 ADR(buf[0]),
                                 totalsyms*4,
                                 ADR(check)) = 0 THEN
      error("Can't communicate with stub");
      RAISE ProcessCreateError;
    END;

    IF check # totalsyms*4 THEN
      error("Can't communicate with stub");
      RAISE ProcessCreateError;
    END;

    ptr := 0;
    dlllib := dlllibs;
    WHILE dlllib # NIL DO
      dllsym := dlllib.dllsyms;
      WHILE dllsym # NIL DO
        dllsym.address := M3LoaderAccess.to_int(buf, ptr, 4);
        Debug.Name(dllsym.name);
        Debug.Txt(" = ");
        Debug.Int(dllsym.address);
        Debug.NL();
        INC(ptr, 4);
        dllsym := dllsym.next;
      END;

      dlllib := dlllib.next;
    END
  END get_dll_addrs;

CONST
  MAX_COMMBLOCK = 16_20000;

PROCEDURE fill_commblock (t: T; dlllib: DllLib; nlibs: INTEGER)
            RAISES {ProcessCreateError} =
  VAR
    dllsym  : DllSymbol;
    textbuf : TEXT;
    check   : INTEGER;
  BEGIN
    t.buffer := M3Buf.New();

    put_m3buf_int(t.buffer, nlibs);
    WHILE dlllib # NIL DO
      M3ID.Put(t.buffer, dlllib.name);
      M3Buf.PutChar(t.buffer, '\000');

      put_m3buf_int(t.buffer, dlllib.nsyms);
      dllsym := dlllib.dllsyms;
      WHILE dllsym # NIL DO
        IF dllsym.ordinal # -1 THEN
          M3Buf.PutChar(t.buffer, '\000');
          put_m3buf_int(t.buffer, dllsym.ordinal);
        ELSE
          M3ID.Put(t.buffer, dllsym.name);
          M3Buf.PutChar(t.buffer, '\000');
        END;
        dllsym := dllsym.next;
      END;

      dlllib := dlllib.next;
    END;

    textbuf := M3Buf.ToText(t.buffer);
    IF Text.Length(textbuf) > MAX_COMMBLOCK THEN
      error("Names too long communicating with stub");
      RAISE ProcessCreateError;
    END;

    IF WinBase.WriteProcessMemory(t.process.hProcess,
                                  LOOPHOLE(t.commblock, ADDRESS),
                                  M3toC.TtoS(textbuf),
                                  Text.Length(textbuf),
                                  ADR(check)) = 0 THEN
      error("Can't communicate with stub");
      RAISE ProcessCreateError;
    END;

    IF check # Text.Length(textbuf) THEN
      error("Can't communicate with stub");
      RAISE ProcessCreateError;
    END;

    t.buffer := NIL;
  END fill_commblock;

PROCEDURE create_process (t: T; cmdline, cwd: TEXT;
                          console: BOOLEAN) RAISES {ProcessCreateError} =
  VAR
    cmd           := M3toC.TtoS(get_cmd(t, cmdline, console));
    curdir        : Ctypes.char_star := NIL;
    startupInfo   := WinBase.STARTUPINFO{
      cb            := BYTESIZE(WinBase.STARTUPINFO),
      lpReserved    := NIL,
      lpDesktop     := NIL,
      lpTitle       := M3toC.TtoS(get_exename(t)),
      dwX := 0, dwY := 0, dwXSize := 0, dwYSize := 0,
      dwXCountChars := 0, dwYCountChars := 0, dwFillAttribute := 0,
      dwFlags       := 0,(*WinBase.STARTF_USESTDHANDLES,*)
      wShowWindow   := 0,
      cbReserved2   := 0,
      lpReserved2   := NIL,
      hStdInput     := NIL,
      hStdOutput    := NIL,
      hStdError     := NIL };
  BEGIN
    IF NOT Text.Equal(cwd, "") THEN
      curdir := M3toC.TtoS(cwd);
    END;

    t.process.hProcess := NIL;
    IF WinBase.CreateProcess(
         lpApplicationName := NIL,
         lpCommandLine := cmd,
         lpProcessAttributes := NIL,
         lpThreadAttributes := NIL,
         bInheritHandles := 1,
         dwCreationFlags := Word.Or(WinBase.DEBUG_PROCESS,
                                    WinBase.CREATE_NEW_CONSOLE),
         lpEnvironment := NIL,
         lpCurrentDirectory := curdir,
         lpStartupInfo := ADR(startupInfo),
         lpProcessInformation := ADR(t.process)) = 0 THEN
      error("Can't create process, error code " &
            Fmt.Int(WinBase.GetLastError()));
      RAISE ProcessCreateError;
    END
  END create_process;

PROCEDURE get_exename (t: T): TEXT =
  VAR
    name : TEXT;
  BEGIN
    name := "M3Loader - " & t.exename;
    RETURN name;
  END get_exename;

PROCEDURE get_cmd (t: T; cmdline: TEXT; console: BOOLEAN): TEXT =
  VAR
    exec : TEXT;
  BEGIN
    exec := Text.Cat(t.execname[console], cmdline);
    RETURN exec;
  END get_cmd;

CONST
  TEXTSEGSIZE = 16_4000000;
  DATASEGSIZE = 16_4000000;
  BSSSEGSIZE = 16_4000000;
  FILEMAP_SIZE = 16_100000;

VAR (* CONST *)
  SYSHANDLE : WinNT.HANDLE;

PROCEDURE newmap (t: T; size: INTEGER; name: TEXT; bss: BOOLEAN): MemData 
            RAISES {ProcessCreateError} =
  VAR
    memdata   := NEW(MemData, segsize := size,
                              highcommitted := t.sysinfo.dwPageSize);
    cname     : WinNT.LPSTR;
    processId := WinBase.GetCurrentProcessId();
  BEGIN
    name := name & Fmt.Int(processId) & ":";

    memdata.name := name;

    IF bss = TRUE THEN
      memdata.start := LOOPHOLE(WinBase.VirtualAlloc(NIL,
                                                     size,
                                                     WinNT.MEM_RESERVE,
                                                     WinNT.PAGE_NOACCESS),
                                INTEGER);
      IF memdata.start = 0 THEN
        error("Error allocating memory");
        RAISE ProcessCreateError;
      END;
      memdata.highcommitted := size;
    ELSE
      (* First, create a file mapping object *)

      cname := M3toC.TtoS(name & "0");
      memdata.filemap[0] := WinBase.CreateFileMapping(SYSHANDLE,
                                                      NIL,
                                                      WinNT.PAGE_READWRITE,
                                                      0,
                                                      FILEMAP_SIZE,
                                                      cname);

      IF memdata.filemap[0] = NIL THEN
        error("Error creating filemapping");
        RAISE ProcessCreateError;
      END;

      (* Now reserve a block of memory so that we have a free area in the
         address space *)

      memdata.start := LOOPHOLE(WinBase.VirtualAlloc(NIL,
                                                     size,
                                                     WinNT.MEM_RESERVE,
                                                     WinNT.PAGE_NOACCESS),
                                INTEGER);

      IF memdata.start = 0 THEN
        error("Error allocating memory");
        RAISE ProcessCreateError;
      END;

      Debug.Txt("VirtualAlloc ");
      Debug.Int(memdata.start);
      Debug.NL();

      (* Now, map the file into the first block we want to use *)

      TRY
        map_beginning_of_block(memdata, memdata.highcommitted, 0);
      EXCEPT
        AllocateError =>
          RAISE ProcessCreateError;
      END;
    END;

    (* Set up a new freelist *)

    memdata.freelist := NEW(FreeList, address := memdata.start,
                                      size := size,
                                      free := TRUE,
                                      next := NIL);

    RETURN memdata;
  END newmap;

PROCEDURE map_beginning_of_block (memdata: MemData; newlen, oldlen: INTEGER)
            RAISES {AllocateError} =
  VAR
    ptr    : ADDRESS;
    offset := FILEMAP_SIZE * memdata.nfullfiles;
  BEGIN
    IF newlen = oldlen THEN RETURN END;

    ptr := LOOPHOLE(memdata.start + offset + oldlen, ADDRESS);

    IF WinBase.VirtualFree(ptr, 0, WinNT.MEM_RELEASE) = 0 THEN
      error("Error in map_beginning_of_block: VirtualFree");
      RAISE AllocateError;
    END;

    ptr := LOOPHOLE(memdata.start + offset + newlen, ADDRESS);

    IF (offset + newlen) < memdata.segsize THEN
      IF WinBase.VirtualAlloc(ptr,
                              memdata.segsize-newlen-offset,
                              WinNT.MEM_RESERVE,
                              WinNT.PAGE_NOACCESS) = NIL THEN
        error("Error in map_beginning_of_block: VirtualAlloc");
        RAISE AllocateError;
      END;

      Debug.Txt("VirtualAlloc ");
      Debug.Int(memdata.start+offset+newlen);
      Debug.NL();
    END;

    ptr := LOOPHOLE(memdata.start + offset + oldlen, ADDRESS);

    IF WinBase.MapViewOfFileEx(memdata.filemap[memdata.nfullfiles],
                               WinBase.FILE_MAP_ALL_ACCESS, 0,
                               oldlen, newlen-oldlen, ptr) # ptr THEN
      error("Error in map_beginning_of_block: MapViewOfFileEx");
      RAISE AllocateError;
    END
  END map_beginning_of_block;

PROCEDURE map_memory (t: T; memdata: MemData; size: INTEGER)
            RAISES {AllocateError} =
  VAR
    oldlen,
    newlen     : INTEGER;
  BEGIN
    size := (size + t.sysinfo.dwPageSize - 1) -
              (size + t.sysinfo.dwPageSize - 1) MOD t.sysinfo.dwPageSize;

    WHILE size > FILEMAP_SIZE * (memdata.nfullfiles+1) DO
      add_new_file(memdata);
    END;

    oldlen := memdata.highcommitted - FILEMAP_SIZE *
                                        memdata.nfullfiles;
    newlen := size - FILEMAP_SIZE * memdata.nfullfiles;

    map_beginning_of_block(memdata, newlen, oldlen);

    memdata.highcommitted := size;
  END map_memory;

PROCEDURE add_new_file (memdata: MemData) RAISES {AllocateError} =
  VAR
    name: TEXT;
  BEGIN
    map_beginning_of_block(memdata, FILEMAP_SIZE,
                           memdata.highcommitted - FILEMAP_SIZE *
                                                   memdata.nfullfiles);
    INC(memdata.nfullfiles);
    memdata.highcommitted := FILEMAP_SIZE * memdata.nfullfiles;
    name := memdata.name & Fmt.Int(memdata.nfullfiles);

    memdata.filemap[memdata.nfullfiles] :=
      WinBase.CreateFileMapping(SYSHANDLE,
                                NIL,
                                WinNT.PAGE_READWRITE,
                                0,
                                FILEMAP_SIZE,
                                M3toC.TtoS(name));

    IF memdata.filemap[memdata.nfullfiles] = NIL THEN
      error("Can't create new filemap in add_new_file");
      RAISE AllocateError;
    END
  END add_new_file;

CONST
  ALLOC_GRANULARITY = 16;
  ALLOC_MASK        = 16_FFFFFFF0;

PROCEDURE allocate (t: T; size: INTEGER; segtype: SegType): Segment
            RAISES {AllocateError} =
  VAR
    seg      := Segment { size := size, type := segtype, address := 0 };
    freelist : FreeList;
    memdata  := t.memdata[segtype];
  BEGIN
    IF size = 0 THEN
      RETURN seg;
    END;

    size := Word.And(size + ALLOC_GRANULARITY-1, ALLOC_MASK);

    freelist := memdata.freelist;

    WHILE freelist # NIL DO
      IF freelist.free AND freelist.size >= size THEN
        IF freelist.next = NIL THEN
          memdata.size := freelist.address - memdata.start + size;
        END;

        freelist.free := FALSE;
        seg.address := freelist.address;
        IF freelist.size > size THEN
	  freelist.next := NEW(FreeList, free := TRUE,
                                         size := freelist.size - size,
                                         address := freelist.address + size,
                                         next := freelist.next);
          freelist.size := size;
        END;

        EXIT;
      ELSE
        freelist := freelist.next;
      END
    END;

    IF freelist = NIL THEN
      error("Out of memory in allocate");
      RAISE AllocateError;
    END;

    IF seg.address - memdata.start + size > memdata.highcommitted THEN
      map_memory(t, memdata, seg.address - memdata.start + size);
    END;

    RETURN seg;
  END allocate;

PROCEDURE free (t: T; seg: Segment) RAISES {AllocateError} =
  VAR
    prev     : FreeList := NIL;
    freelist : FreeList;
    memdata  := t.memdata[seg.type];
  BEGIN
    IF seg.address =  0 THEN RETURN END;

    freelist := memdata.freelist;

    WHILE freelist # NIL DO
      IF NOT freelist.free AND freelist.address = seg.address THEN
        IF prev # NIL AND prev.free THEN
          INC(prev.size, freelist.size);
          prev.next := freelist.next;
          freelist := prev;
        END;

        IF freelist.next # NIL AND freelist.next.free THEN
          INC(freelist.size, freelist.next.size);
          freelist.next := freelist.next.next;
        END;

        freelist.free := TRUE;

        IF freelist.next = NIL THEN
          memdata.size := freelist.address - memdata.start;
        END;

        EXIT;
      ELSE
        prev := freelist;
        freelist := freelist.next;
      END
    END;

    IF freelist = NIL THEN
      error("Free called with invalid address");
      RAISE AllocateError;
    END
  END free;

PROCEDURE New (): T =
  VAR
    t := NEW(T, sysinfo := NEW(WinBase.LPSYSTEM_INFO));
  BEGIN
    SYSHANDLE := LOOPHOLE(-1, WinNT.HANDLE);
    WinBase.GetSystemInfo(t.sysinfo);
    t.sysinfo.dwPageSize :=
      Word.And(t.sysinfo.dwPageSize + 16_FFFF, 16_FFFF0000);
    (* Allocations happen on 64K boundaries *)
    TRY
      t.memdata := ARRAY SegType OF MemData {
                     newmap(t, TEXTSEGSIZE, "M3LoaderTextSegment", FALSE),
                     newmap(t, DATASEGSIZE, "M3LoaderDataSegment", FALSE),
                     newmap(t, BSSSEGSIZE, "M3LoaderBssSegment", TRUE) };
    EXCEPT
      ProcessCreateError =>
        RETURN NIL;
    END;
    t.execname := ARRAY BOOLEAN OF TEXT {
                    "m3processstubwin.exe ",
                    "m3processstubcon.exe " };
    t.debug_ptr := ADR(t.debug_event);
    t.threadtable := NEW(IntRefTbl.Default);
    t.threadtable := t.threadtable.init();
    t.proctable := NEW(IntRefTbl.Default);
    t.proctable := t.proctable.init();
    RETURN t;
  END New;

BEGIN
END M3LoaderProcess.
