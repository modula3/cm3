(* Copyright (C) 1996, Positron Industries, Public Safety Division.            *)
(* All rights reserved.                                                        *)
(* See the file COPYRIGHT for a full description                               *)

(* Derived from the "CSSSerial" module defined by Jacques Dagenais
   in April 1996. *)

UNSAFE MODULE SerialPort;

IMPORT FilePosix, M3toC, OSError, OSErrorPosix;
IMPORT Pathname, Unix, Userial, WeakRef, Word;

REVEAL
  T = Public BRANDED "SerialPort.T" OBJECT
  OVERRIDES
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
  ValIndex = [0..12];
  ValMap   = ARRAY ValIndex OF INTEGER;

CONST  (* mappings from Modula-3 <-> POSIX constant values *)
  NO_VALUE = -1;

  UnixBaudRates = ValueMap { ERR_UNKNOWN_BAUDRATE, 9, Userial.B38400,
    ValMap { Userial.B110,   Userial.B300,   Userial.B600,
             Userial.B1200,  Userial.B2400,  Userial.B4800,
             Userial.B9600,  NO_VALUE,       Userial.B19200,
             Userial.B38400, .. }
  };

  UnixDataBits = ValueMap { ERR_UNKNOWN_BYTESIZE, 3, Userial.CSIZE,
    ValMap { Userial.CS5, Userial.CS6, Userial.CS7, Userial.CS8, .. }
  };

  UnixStopBits = ValueMap { ERR_UNKNOWN_STOPBITS, 2, Userial.CSTOPB,
    ValMap { 0(*1.0*), NO_VALUE(*1.5*), Userial.CSTOPB(*2.0*), .. }
  };

  UnixParity = ValueMap { ERR_UNKNOWN_PARITY, 2, Userial.PARENB + Userial.PARODD,
    ValMap { 0, Userial.PARENB + Userial.PARODD, Userial.PARENB, .. }
  };

  UnixDTRMode = ValueMap { ERR_UNKNOWN_DTR_MODE, 0, 0,
    ValMap { 0, .. }  (* Ignored on Linux *)
  };

  UnixRTSMode = ValueMap { ERR_UNKNOWN_RTS_MODE, 3, 0,
    ValMap { 0, .. }  (* Ignored on Linux *)
  };

(*---------------------------------------------------- exported procedures ---*)

PROCEDURE Open (p: Pathname.T): T
  RAISES {OSError.E} =
  VAR t := NEW (T, ds := FilePosix.ReadWrite);
  BEGIN
    (* Get the handle to the underlying Unix file *)
    t.fd := Unix.open (M3toC.TtoS (p), Unix.O_RDWR + Unix.O_EXCL, Unix.Mrwrwrw);
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
  VAR info: Userial.struct_termios;  c: Config;
  BEGIN
    IF Userial.tcgetattr (t.fd, ADR (info)) < 0 THEN
      OSErrorPosix.Raise ();
    END;

    c.baud_rate := VAL (Scan (UnixBaudRates, info.c_cflag), BaudRate);
    c.data_bits := VAL (Scan (UnixDataBits,  info.c_cflag), DataBits);
    c.stop_bits := VAL (Scan (UnixStopBits,  info.c_cflag), StopBits);
    c.parity    := VAL (Scan (UnixParity,    info.c_cflag), Parity);
    c.DTR_mode  := VAL (Scan (UnixDTRMode,   info.c_cflag), DTR);
    c.RTS_mode  := VAL (Scan (UnixRTSMode,   info.c_cflag), RTS);
    RETURN c;
  END GetConfig;

PROCEDURE SetConfig (t: T;  READONLY c: Config)
  RAISES {OSError.E} =
  VAR info: Userial.struct_termios;
  BEGIN
    (* first, get the current values *)
    IF Userial.tcgetattr (t.fd, ADR (info)) < 0 THEN
      OSErrorPosix.Raise ();
    END;

    (* set the values we care about *)

    info.c_lflag := Word.And (info.c_lflag, Word.Not (Userial.ICANON));
    info.c_lflag := Word.And (info.c_lflag, Word.Not (Userial.ECHO));

    info.c_iflag := Userial.IGNBRK + Userial.IGNPAR;

    info.c_cflag := Word.Or (info.c_cflag, Userial.CREAD);
    SetBits (info.c_cflag, UnixBaudRates, ORD (c.baud_rate));
    SetBits (info.c_cflag, UnixDataBits,  ORD (c.data_bits));
    SetBits (info.c_cflag, UnixStopBits,  ORD (c.stop_bits));
    SetBits (info.c_cflag, UnixParity,    ORD (c.parity));
    SetBits (info.c_cflag, UnixDTRMode,   ORD (c.DTR_mode));
    SetBits (info.c_cflag, UnixRTSMode,   ORD (c.RTS_mode));

    (* update the device configuration *)
    IF Userial.tcsetattr (t.fd, Userial.TCSANOW, ADR (info)) < 0 THEN
      OSErrorPosix.Raise ();
    END;
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

BEGIN
END SerialPort.

