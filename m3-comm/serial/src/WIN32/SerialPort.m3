(* Copyright (C) 1996, Positron Industries, Public Safety Division.         *)
(* All rights reserved.                                                     *)
(* See the file COPYRIGHT for a full description                            *)

(* Derived from the "CSSSerial" module defined by Claude Chausse and
   Jacques Dagenais in March 1996. 
   Fixed and enhanced by Blair MacIntyre, Feb. 1997.
*)

UNSAFE MODULE SerialPort;

IMPORT File, FileWin32, M3toC, OSError, OSErrorWin32;
IMPORT Pathname, WeakRef, WinBase, WinError, WinNT;

(*IMPORT IO, Fmt;*)

REVEAL
  T = Public BRANDED "SerialPort.T" OBJECT
    read_ov  : WinBase.LPOVERLAPPED := NIL;
    write_ov : WinBase.LPOVERLAPPED := NIL;
  OVERRIDES
    close      := Close;
    read       := Read;
    write      := Write;
    get_config := GetConfig;
    set_config := SetConfig;
  END;
  (* The "Terminal.T" "read" and "write" methods are overridden here
     so that we can do Win32 overlapping I/O.  According to the Win32
     documentation, this hack allows "simultaneous" reading and writing
     on the same serial device.  The "close" method is overridden so
     that we can clean up some untraced allocations. *)

CONST (* sizes of the internal Win32-provided buffers *)
  INBUF_SIZE  = 4096;
  OUTBUF_SIZE = 4096;

CONST (* Win32-like error codes for this module *)
  SERIAL_PORT_ERROR    = 16_e0010000; (* == Error, Customer, Facility = 1 *)
  ERR_UNKNOWN_BAUDRATE = SERIAL_PORT_ERROR + 1;
  ERR_UNKNOWN_BYTESIZE = SERIAL_PORT_ERROR + 2;
  ERR_UNKNOWN_STOPBITS = SERIAL_PORT_ERROR + 3;
  ERR_UNKNOWN_PARITY   = SERIAL_PORT_ERROR + 4;
  ERR_UNKNOWN_DTR_MODE = SERIAL_PORT_ERROR + 5;
  ERR_UNKNOWN_RTS_MODE = SERIAL_PORT_ERROR + 6;
  (* See WinError.i3 for the bit field layout of these values *)

TYPE
  ValueMap = RECORD
    error    : INTEGER;
    last_val : ValIndex;
    values   : ValMap;
  END;
  ValIndex = [0..13];
  ValMap   = ARRAY ValIndex OF INTEGER;

CONST  (* mappings from Modula-3 <-> Win32 constant values *)
  NO_VALUE = -1;

  Win32BaudRates = ValueMap { ERR_UNKNOWN_BAUDRATE, 9,
    ValMap { NO_VALUE, WinBase.CBR_110,   WinBase.CBR_300,   WinBase.CBR_600,
             WinBase.CBR_1200,  WinBase.CBR_2400,  WinBase.CBR_4800,
             WinBase.CBR_9600,  WinBase.CBR_14400, WinBase.CBR_19200,
             WinBase.CBR_38400, .. }
  };

  Win32DataBits = ValueMap { ERR_UNKNOWN_BYTESIZE, 3,
    ValMap { 8, 7, 6, 5, .. }
  };

  Win32StopBits = ValueMap { ERR_UNKNOWN_STOPBITS, 2,
    ValMap { WinBase.ONESTOPBIT, WinBase.ONE5STOPBITS,
             WinBase.TWOSTOPBITS, .. }
  };

  Win32Parity = ValueMap { ERR_UNKNOWN_PARITY, 4,
    ValMap { WinBase.NOPARITY, WinBase.ODDPARITY, WinBase.EVENPARITY,
             WinBase.MARKPARITY, WinBase.SPACEPARITY, .. }
  };

  Win32DTRMode = ValueMap { ERR_UNKNOWN_DTR_MODE, 2,
    ValMap { WinBase.DTR_CONTROL_DISABLE, WinBase.DTR_CONTROL_ENABLE,
             WinBase.DTR_CONTROL_HANDSHAKE, .. }
  };

  Win32RTSMode = ValueMap { ERR_UNKNOWN_RTS_MODE, 3,
    ValMap { WinBase.RTS_CONTROL_DISABLE, WinBase.RTS_CONTROL_ENABLE,
             WinBase.RTS_CONTROL_HANDSHAKE, WinBase.RTS_CONTROL_TOGGLE, .. }
  };

(*--------------------------------------------------- exported procedures ---*)

PROCEDURE Open (p: Pathname.T): T
  RAISES {OSError.E} =
  CONST Attrs = WinNT.FILE_ATTRIBUTE_NORMAL + WinBase.FILE_FLAG_OVERLAPPED;
  CONST RWAccess = WinNT.GENERIC_READ + WinNT.GENERIC_WRITE;
  VAR t := NEW (T);
  BEGIN
    t.read_ov  := NewOverlap ();
    t.write_ov := NewOverlap ();

    (* Get the handle to the underlying Win32 device *)
    WITH str = M3toC.SharedTtoS(p) DO
      t.handle := WinBase.CreateFile(
       lpFileName            := str,
       dwDesiredAccess       := RWAccess,
       dwShareMode           := 0,  (* com devices must be exclusive access *)
       lpSecurityAttributes  := NIL,
       dwCreationDisposition := WinBase.OPEN_EXISTING,
       dwFlagsAndAttributes  := Attrs,
       hTemplateFile         := NIL);
      M3toC.FreeSharedS(p,str);
    END;
    IF LOOPHOLE (t.handle, INTEGER) = WinBase.INVALID_HANDLE_VALUE THEN 
      OSErrorWin32.Raise ();
    END;

    (* Set Win32's input and output buffer sizes *)
    IF WinBase.SetupComm (t.handle, INBUF_SIZE, OUTBUF_SIZE) = 0 THEN
      OSErrorWin32.Raise ();
    END;

    (* Establish the advertised default configuration *)
    t.set_config (InitialConfig);

    (* make sure we get a chance to clean up during GC *)
    EVAL WeakRef.FromRef (t, Cleanup);

    RETURN t;
  END Open;

(*--------------------------------------------------------------- methods ---*)

PROCEDURE Close (t: T)
  RAISES {OSError.E} =
  BEGIN
    DisposeOverlap (t.read_ov);
    DisposeOverlap (t.write_ov);
    IF WinBase.CloseHandle (t.handle) = 0 THEN OSErrorWin32.Raise () END
  END Close;

PROCEDURE GetConfig (t: T): Config
  RAISES {OSError.E} =
  VAR dcb: WinBase.DCB;  c: Config;
  BEGIN
    dcb.DCBlength := BYTESIZE (dcb);
    IF WinBase.GetCommState (t.handle, ADR (dcb)) = 0 THEN
      OSErrorWin32.Raise ();
    END;

    c.baud_rate := VAL (Scan (Win32BaudRates, dcb.BaudRate),    BaudRate);
    c.data_bits := VAL (Scan (Win32DataBits,  dcb.ByteSize),    DataBits);
    c.stop_bits := VAL (Scan (Win32StopBits,  dcb.StopBits),    StopBits);
    c.parity    := VAL (Scan (Win32Parity,    dcb.Parity),      Parity);
    c.DTR_mode  := VAL (Scan (Win32DTRMode,   dcb.fDtrControl), DTR);
    c.RTS_mode  := VAL (Scan (Win32RTSMode,   dcb.fRtsControl), RTS);
    RETURN c;
  END GetConfig;

PROCEDURE SetConfig (t: T;  READONLY c: Config)
  RAISES {OSError.E} =
  VAR dcb: WinBase.DCB;  x: WinBase.COMMTIMEOUTS;
  BEGIN
    (* first, get the current values *)
    dcb.DCBlength := BYTESIZE (dcb);
    IF WinBase.GetCommState (t.handle, ADR (dcb)) = 0 THEN
      OSErrorWin32.Raise ();
    END;

    (* set the values we care about *)
    dcb.fParity           := 1;  (* enable parity checking *)
    dcb.fDtrControl       := Value(Win32DTRMode, ORD (c.DTR_mode));
    dcb.ByteSize          := Value(Win32DataBits, ORD (c.data_bits));
    dcb.Parity            := Value(Win32Parity, ORD (c.parity));
    dcb.StopBits          := Value(Win32StopBits, ORD (c.stop_bits));
    dcb.fOutxCtsFlow      := 0;  (* disable CTS output flow control *)
    dcb.fOutxDsrFlow      := 0;  (* disable DSR output flow control *)
    dcb.fDsrSensitivity   := 0;  (* sensitivity?? *)
    dcb.fAbortOnError     := 0;  (* don't abort read/writes on error *)
    dcb.fRtsControl       := Value(Win32RTSMode, ORD (c.RTS_mode));
    dcb.BaudRate          := Value(Win32BaudRates, ORD (c.baud_rate));

(*  I don't think we need to screw with these ... leave them for now.
    dcb.fBinary           := 0;  (* enable binary transfers, no EOF checking *)
    dcb.fTXContinueOnXoff := 0;  (* XOFF => continues transmission *)
    dcb.fOutX             := 0;  (* disable XON/XOFF flow control on output *)
    dcb.fInX              := 0;  (* disable XON/XOFF flow control on input *)
    dcb.fErrorChar        := 0;  (* disable error replacement *)
    dcb.fNull             := 0;  (* disable NULL stripping *)
*)

    dcb.DCBlength         := BYTESIZE (dcb);

    (* update the device configuration *)
    IF WinBase.SetCommState (t.handle, ADR (dcb)) = 0 THEN
      OSErrorWin32.Raise ();
    END;

    (* finally, reset the timeout values to match the current baud rate *)
    x.ReadIntervalTimeout         := c.timeouts.readInterval;
    x.ReadTotalTimeoutMultiplier  := c.timeouts.readMultiplier;
    x.ReadTotalTimeoutConstant    := c.timeouts.readConstant;
    x.WriteTotalTimeoutMultiplier := 0;
    x.WriteTotalTimeoutConstant   := 0;
    IF WinBase.SetCommTimeouts (t.handle, ADR (x)) = 0 THEN
      OSErrorWin32.Raise ();
    END;
  END SetConfig;

PROCEDURE Read (t: T;  VAR(*OUT*) b: ARRAY OF File.Byte; 
                mayBlock: BOOLEAN): INTEGER
  RAISES {OSError.E} =
  VAR numRead := 0;  len := NUMBER (b);  stats: WinBase.COMSTAT;
  BEGIN
    IF len <= 0 THEN RETURN 0; END;

    IF NOT mayBlock THEN
      (* see if there's any input data already waiting... *)
      IF WinBase.ClearCommError (t.handle, ADR (numRead), ADR (stats)) = 0 THEN
        OSErrorWin32.Raise ();
      END;
      IF (stats.cbInQue <= 0) THEN RETURN -1; END;
      len := MIN (len, stats.cbInQue);
    END;

    IF WinBase.ReadFile (t.handle, ADR (b[0]), len,
                         ADR (numRead), t.read_ov) = 0 THEN
      numRead := IOWait (t, t.read_ov);
    END;

    RETURN numRead;
  END Read;

PROCEDURE Write (t: T;  READONLY b: ARRAY OF File.Byte)
  RAISES {OSError.E} =
  VAR nWritten := 0;
  BEGIN
    IF NUMBER (b) <= 0 THEN RETURN; END;
    IF WinBase.WriteFile (t.handle, ADR (b[0]), NUMBER (b),
                          ADR (nWritten), t.write_ov) = 0 THEN
      nWritten := IOWait (t, t.write_ov);
    END;
    <*ASSERT nWritten = NUMBER (b) *>
  END Write;

(*---------------------------------------------------- internal procedures ---*)

PROCEDURE Cleanup (<*UNUSED*> READONLY w: WeakRef.T;  ref: REFANY) =
  VAR t := NARROW (ref, T);
  BEGIN
    DisposeOverlap (t.read_ov);
    DisposeOverlap (t.write_ov);
    EVAL WinBase.CloseHandle (t.handle);
  END Cleanup;

PROCEDURE Scan (READONLY map: ValueMap;  val: INTEGER): ValIndex
  RAISES {OSError.E} =
  BEGIN
    FOR i := 0 TO map.last_val DO
      IF (val = map.values[i]) THEN RETURN i; END;
    END;
    OSErrorWin32.Raise0 (map.error);
    RETURN 0;
  END Scan;

PROCEDURE Value (READONLY map: ValueMap;  val: ValIndex): INTEGER 
  RAISES {OSError.E} =
  VAR bits := map.values [val];
  BEGIN
    IF (bits = NO_VALUE) OR (val > map.last_val) THEN
      (* unsupported option *)
      OSErrorWin32.Raise0 (map.error);
    END;
    RETURN bits;
  END Value; 

PROCEDURE IOWait (t: T;  ov: WinBase.LPOVERLAPPED): INTEGER
  RAISES {OSError.E} =
  VAR err, result: INTEGER;
  BEGIN
    err := WinBase.GetLastError ();
    (*
    IF ov = t.write_ov THEN
      IO.Put("waiting on write.\n");
    ELSE
      IO.Put("waiting on read.\n");
    END;
    *)
    IF err = WinError.ERROR_HANDLE_EOF THEN RETURN 0; END;

    IF err # WinError.ERROR_IO_PENDING THEN OSErrorWin32.Raise0 (err); END;
    (* The error ERROR_IO_PENDING is normal in overlapped operations,
       it means that the I/O will be completed asynchronously when
       possible. *)

    WHILE WinBase.GetOverlappedResult (t.handle, ov, ADR (result), 1) = 0 DO
      err := WinBase.GetLastError ();
      (*IO.Put("result=0.\n");*)
      IF err = WinError.ERROR_HANDLE_EOF THEN RETURN 0; END;
      (*IO.Put("not eof.\n");*)
      IF err # WinError.ERROR_IO_PENDING THEN
        OSErrorWin32.Raise0 (err);
      END;
      (*IO.Put("IO Pending!\n");*)
    END;
    (*IO.Put("result=" & Fmt.Int(result) & "\n");*)

    IF result = 0 THEN 
      RETURN -1;
    ELSE
      RETURN result;
    END;
  END IOWait;

PROCEDURE NewOverlap (): WinBase.LPOVERLAPPED
  RAISES {OSError.E} =
  VAR ov := NEW (WinBase.LPOVERLAPPED);
  BEGIN
    ov.Internal     := 0;  (* reserved for Win32 *)
    ov.InternalHigh := 0;  (* reserved for Win32 *)
    ov.Offset       := 0;  (* ignored on comm devices *)
    ov.OffsetHigh   := 0;  (* ignored on comm devices *)
    ov.hEvent       := WinBase.CreateEvent (NIL, 1, 0, NIL);
    IF (ov.hEvent = NIL) THEN OSErrorWin32.Raise (); END;
    RETURN ov;
  END NewOverlap;

PROCEDURE DisposeOverlap (VAR ov: WinBase.LPOVERLAPPED) =
  BEGIN
    IF (ov = NIL) THEN RETURN; END;
    IF (ov.hEvent # NIL) THEN
      EVAL WinBase.CloseHandle (ov.hEvent);
      ov.hEvent := NIL;
    END;
    DISPOSE (ov);
    ov := NIL;
  END DisposeOverlap;

BEGIN
END SerialPort.

