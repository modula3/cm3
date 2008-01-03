(* Copyright (C) 1996, Positron Industries, Public Safety Division.      *)
(* All rights reserved.                                                  *)
(* See the file COPYRIGHT for a full description                         *)

(* A "SerialPort.T" or serial port represents a serial communications
   device.  It is a subtype of "File.T" that can be read or written.
   The configuration details of the underlying device can also be
   read or written. *)

INTERFACE SerialPort;

IMPORT OSError, Pathname, Terminal;

TYPE
  T <: Public;
  Public = Terminal.T OBJECT METHODS
    get_config (): Config                  RAISES {OSError.E};
    set_config (READONLY  config: Config)  RAISES {OSError.E};
  END;

TYPE
  Config = RECORD
    baud_rate : BaudRate;
    data_bits : DataBits;
    stop_bits : StopBits;
    parity    : Parity;
    DTR_mode  : DTR;
    RTS_mode  : RTS;
    timeouts  : Timeouts;
  END;

TYPE
  BaudRate = {BR75, BR110, BR300, BR600, BR1200, BR2400, BR4800, BR9600,
              BR14400, BR19200, BR38400, BR57600, BR115200, BR230400, BR460800};
  DataBits = {DB8, DB7, DB6, DB5};
  StopBits = {SB1, SB15, SB2};
  Parity   = {None, Odd, Even, Mark, Space};
  DTR      = {Disabled, Enabled, Handshake};
  RTS      = {Disabled, Enabled, Handshake, Toggle};

  (* Three values are used to determine if and when a serial port read
     times out.  "readInterval" is the maximum time, in milliseconds,
     between the arrival of any two characters.  The time period
     begins when the first character is received.  If the interval
     between the arrival of any two characters exceeds this amount,
     the read times out. The total timeout for a read is calculated by
     multiplying "readMultiplier" by the number of characters to be
     read and adding it to "readConstant."  If the result is zero,
     there is no timeout.  "writeMultiplier" and "writeConstant" are
     used analogously. *)

  Timeouts = RECORD
    readInterval: CARDINAL := 0;
    readMultiplier: CARDINAL := 0;
    readConstant: CARDINAL := 0;
  END;

CONST
  InitialConfig = Config {
    BaudRate.BR9600, DataBits.DB8, StopBits.SB1, Parity.None,
    DTR.Disabled, RTS.Disabled, Timeouts{0, 0, 0}
  };

PROCEDURE Open (p: Pathname.T): T RAISES {OSError.E};
(* Opens the serial device named "p" and returns a "SerialPort.T"
   that can read and write the device.  The initial configuration
   is set to "InitialConfig". *)

END SerialPort.

(*
Given an open serial port "s",

  "s.get_config()" returns the current configuration of the
  underlying device.

  "s.set_config(cfg)" configures the underlying device to
  correspond to "cfg".

Implementation Restrictions:

|  Windows NT and Windows 95:
|    BR75, BR230400 are not implemented
|    The combination of "SB2" and "DB5" is not allowed by Windows.
|    The combination of "SB15" and "DB6", "DB7", or "DB8" is not
|      allowed by Windows.
  
|  Unix:
|    The "BR14400" rate is not supported on any Unix.  Baud rates
|    above BR38400 are supported on very few.
|    The "MRK" and "SPC" parity are not supported.
|    The "SB15" (1.5 stopbits) is not supported.
|    The "Timeouts.readInterval" is not supported.
|    DTR and RTS control are not supported.  They must be set to Disabled.
*)

(* A serial handle "h" can be used anywhere a "Terminal.T" can be
   used.  The difference is that the behaviour of the call

| h.read(b, mayBlock)

  is modified by the timeout values specified in the device
  configuration "cfg" passed to "h.set_config(cfg)".   If mayBlock is
  FALSE, "h" behaves as a "Terminal.T".  Otherwise, if the timeout
  values are not 0, a call to "h.read(b,TRUE)" may less than the
  requested number of bytes.  The call will block for at most the
  amount of time specified by the timeouts, and will return either "0"
  (end-of-file), -1 (no data available, time interval exceeded) or
  some value ">0" up to "num(b)" if some data was read.

*)
