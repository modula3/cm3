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
  END;

TYPE
  BaudRate = {BR110, BR300, BR600, BR1200, BR2400, BR4800, BR9600,
              BR14400, BR19200, BR38400, BR56000, BR128000, BR256000};
  DataBits = {DB8, DB7, DB6, DB5};
  StopBits = {SB1, SB15, SB2};
  Parity   = {None, Odd, Even, Mark, Space};
  DTR      = {Disabled, Enabled, Handshake};
  RTS      = {Disabled, Enabled, Handshake, Toggle};

CONST
  InitialConfig = Config {
    BaudRate.BR9600, DataBits.DB8, StopBits.SB1, Parity.None,
    DTR.Disabled, RTS.Disabled
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
|    The combination of "SB2" and "DB5" is not allowed by Windows.
|    The combination of "SB15" and "DB6", "DB7", or "DB8" is not
|      allowed by Windows.
  
|  Linux:
|    The "BR14400" rate is not supported.
|    The "MRK" and "SPC" parity are not supported.
|    The "SB15" (1.5 stopbits) is not supported.
*)

