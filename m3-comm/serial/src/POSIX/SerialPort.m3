(* Copyright (C) 1996, Positron Industries, Public Safety Division.            *)
(* All rights reserved.                                                        *)
(* See the file COPYRIGHT for a full description                               *)

(* Derived from the "CSSSerial" module defined by Jacques Dagenais
   in April 1996.
   Fixed and enhanced by Blair MacIntyre, Feb. 1997.
 *)

UNSAFE MODULE SerialPort;

IMPORT FilePosix, M3toC, OSError, OSErrorPosix, File, Time, Terminal,
       SchedulerPosix;
IMPORT Pathname, Unix, Utermio, WeakRef, Word;

REVEAL
  T = Public BRANDED "SerialPort.T" OBJECT
    currConfig: Config := InitialConfig;
  OVERRIDES
    read       := Read;
    get_config := GetConfig;
    set_config := SetConfig;
  END;

CONST (* error codes for this module *)
  SERIAL_PORT_ERROR    = 1000;
  ERR_UNKNOWN_BAUDRATE = SERIAL_PORT_ERROR + 1;
  ERR_UNKNOWN_BYTESIZE = SERIAL_PORT_ERROR + 2;
  ERR_UNKNOWN_STOPBITS = SERIAL_PORT_ERROR + 3;
  ERR_UNKNOWN_PARITY   = SERIAL_PORT_ERROR + 4;
  ERR_UNKNOWN_DTR_MODE = SERIAL_PORT_ERROR + 5;
  ERR_UNKNOWN_RTS_MODE = SERIAL_PORT_ERROR + 6;

TYPE
  ValueMap = RECORD
    error    : INTEGER;
    last_val : ValIndex;
    mask     : INTEGER;
    values   : ValMap;
  END;
  ValIndex = [0..13];
  ValMap   = ARRAY ValIndex OF INTEGER;

CONST  (* mappings from Modula-3 <-> POSIX constant values *)
  NO_VALUE = -1;

  UnixBaudRates = ValueMap { ERR_UNKNOWN_BAUDRATE, 10, Utermio.BAUDBITS,
    ValMap { Utermio.B75, Utermio.B110,   Utermio.B300,   Utermio.B600,
             Utermio.B1200,  Utermio.B2400,  Utermio.B4800,
             Utermio.B9600,  Utermio.B14400, Utermio.B19200,
             Utermio.B38400, Utermio.B57600, Utermio.B115200, 
             Utermio.B230400 }
  };

  UnixDataBits = ValueMap { ERR_UNKNOWN_BYTESIZE, 3, Utermio.CSIZE,
    ValMap { Utermio.CS8, Utermio.CS7, Utermio.CS6, Utermio.CS5, .. }
  };

  UnixStopBits = ValueMap { ERR_UNKNOWN_STOPBITS, 2, Utermio.CSTOPB,
    ValMap { 0(*1.0*), NO_VALUE(*1.5*), Utermio.CSTOPB(*2.0*), .. }
  };

  UnixParity = ValueMap { ERR_UNKNOWN_PARITY, 2, Utermio.PARITYBITS,
    ValMap { Utermio.PARNONE, Utermio.PARODD, Utermio.PAREVEN, .. }
  };

  (* I believe that these last 2 could be implemented manually, using the
     termios modem control line IOCTLs TIOCMGET/TIOCMSET.  I'm not
     sure how practical that is, however.  -- Blair 
     Note:  both Enabled and Disabled are allowed, since some devices
     on NT require it. *)
  UnixDTRMode = ValueMap { ERR_UNKNOWN_DTR_MODE, 1, 0,
    ValMap { 0, .. }  (* Ignored on Unix *)
  };

  UnixRTSMode = ValueMap { ERR_UNKNOWN_RTS_MODE, 1, 0,
    ValMap { 0, .. }  (* Ignored on Unix *)
  };

(*---------------------------------------------------- exported procedures ---*)

PROCEDURE Open (p: Pathname.T): T
  RAISES {OSError.E} =
  VAR t := NEW (T, ds := FilePosix.ReadWrite);
  BEGIN
    (* Get the handle to the underlying Unix file *)
    WITH str = M3toC.SharedTtoS(p) DO
      t.fd := Unix.open (str, Unix.O_RDWR + Unix.O_EXCL, Unix.Mrwrwrw);
      M3toC.FreeSharedS(p,str);
    END;

    (* Set the direction set correctly *)
    t.ds := FilePosix.ReadWrite;

    IF (t.fd < 0) THEN OSErrorPosix.Raise (); END;

    (* Establish the advertised default configuration *)
    t.set_config (InitialConfig);

    (* make sure we get a chance to clean up during GC *)
    EVAL WeakRef.FromRef (t, Cleanup);

    RETURN t;
  END Open;

(*---------------------------------------------------------------- methods ---*)

PROCEDURE GetConfig (t: T): Config
  RAISES {OSError.E} =
  VAR info: Utermio.struct_termios;  c: Config;
  BEGIN
    IF Utermio.tcgetattr (t.fd, ADR (info)) < 0 THEN
      OSErrorPosix.Raise ();
    END;

    c.baud_rate := VAL (Scan (UnixBaudRates, info.c_cflag), BaudRate);
    c.data_bits := VAL (Scan (UnixDataBits,  info.c_cflag), DataBits);
    c.stop_bits := VAL (Scan (UnixStopBits,  info.c_cflag), StopBits);
    c.parity    := VAL (Scan (UnixParity,    info.c_cflag), Parity);
    c.DTR_mode  := VAL (Scan (UnixDTRMode,   info.c_cflag), DTR);
    c.RTS_mode  := VAL (Scan (UnixRTSMode,   info.c_cflag), RTS);
    c.timeouts  := t.currConfig.timeouts;
    t.currConfig := c;
    RETURN c;
  END GetConfig;

PROCEDURE SetConfig (t: T;  READONLY c: Config)
  RAISES {OSError.E} =
  VAR info: Utermio.struct_termios;
  BEGIN
    (* first, get the current values *)
    IF Utermio.tcgetattr (t.fd, ADR (info)) < 0 THEN
      OSErrorPosix.Raise ();
    END;

    (* set the values we care about *)
    info.c_lflag := 0;   (* no local modes! *)
    info.c_iflag := Word.Or(Utermio.IGNBRK, Utermio.IGNPAR);

    info.c_cflag := Word.Or (info.c_cflag, Utermio.CREAD);
    SetBits (info.c_cflag, UnixBaudRates, ORD (c.baud_rate));
    SetBits (info.c_cflag, UnixDataBits,  ORD (c.data_bits));
    SetBits (info.c_cflag, UnixStopBits,  ORD (c.stop_bits));
    SetBits (info.c_cflag, UnixParity,    ORD (c.parity));
    SetBits (info.c_cflag, UnixDTRMode,   ORD (c.DTR_mode));
    SetBits (info.c_cflag, UnixRTSMode,   ORD (c.RTS_mode));

    info.c_cc[Utermio.VMIN] := 1;
    info.c_cc[Utermio.VTIME] := 0;

    (* update the device configuration *)
    IF Utermio.tcsetattr (t.fd, Utermio.TCSANOW, ADR (info)) < 0 THEN
      OSErrorPosix.Raise ();
    END;
    t.currConfig := c;
  END SetConfig;

(*---------------------------------------------------- internal procedures ---*)

PROCEDURE Cleanup (<*UNUSED*> READONLY w: WeakRef.T;  ref: REFANY) =
  VAR t := NARROW (ref, T);
  BEGIN
    EVAL Unix.close (t.fd);
  END Cleanup;

PROCEDURE Scan (READONLY map: ValueMap;  val: INTEGER): ValIndex
  RAISES {OSError.E} =
  VAR bits: INTEGER;
  BEGIN
    val := Word.And (val, map.mask);
    FOR i := 0 TO map.last_val DO
      bits := map.values[i];
      IF (bits # NO_VALUE) AND (val = bits) THEN RETURN i; END;
    END;
    OSErrorPosix.Raise0 (map.error);
    RETURN 0;
  END Scan;

PROCEDURE SetBits (VAR flags: INTEGER;  READONLY map: ValueMap;  val: ValIndex)
  RAISES {OSError.E} =
  VAR bits := map.values [val];
  BEGIN
    IF (bits = NO_VALUE) OR (val > map.last_val) THEN
      (* unsupported option *)
      OSErrorPosix.Raise0 (map.error);
    END;
    flags := Word.Or (Word.And (flags, Word.Not (map.mask)), bits);
  END SetBits;

PROCEDURE Read (t: T;  VAR(*OUT*) b: ARRAY OF File.Byte; 
                mayBlock: BOOLEAN): INTEGER RAISES {OSError.E}=
  VAR
    timeoutInterval := FLOAT(t.currConfig.timeouts.readConstant + 
                               t.currConfig.timeouts.readMultiplier*NUMBER(b), 
                               LONGREAL)/1000.0D0;
    read_so_far: CARDINAL := 0;
    start_time : Time.T   := Time.Now();
    length     : CARDINAL := NUMBER(b);

    status     : INTEGER;
  BEGIN
    LOOP
      (* do a non-blocking read of what is left of the buffer, counting
         what we have read so far. *)
      WITH left = length - read_so_far DO
        (* read with our parents read method *)
        status := Terminal.T.read(t, SUBARRAY(b, read_so_far, left), FALSE);
      END;

      (* check to see if we've read all we wanted. *)
      IF status = 0 THEN
        RETURN 0; (*EOF*)
      ELSIF status > 0 THEN
        INC(read_so_far, status);
        IF read_so_far = NUMBER(b) THEN RETURN read_so_far END;
      END;

      (* If we are not to block, return.
         If we are blocking, but the time limit has expired, return.
         Otherwise, wait for more characters.   *)
      WITH time_left = timeoutInterval - (Time.Now() - start_time) DO
        IF (NOT mayBlock) OR (timeoutInterval > 0.0D0 AND time_left < 0.0D0) OR
          SchedulerPosix.IOWait(t.fd, TRUE, time_left)
             = SchedulerPosix.WaitResult.Timeout THEN
          (* times up. If nothing read, return -1.
             Else, return the number of characters read *)
          IF read_so_far = 0 THEN
            RETURN -1;
          ELSE
            RETURN read_so_far;
          END;
        END;
      END;
    END;
  END Read;

BEGIN
END SerialPort.
