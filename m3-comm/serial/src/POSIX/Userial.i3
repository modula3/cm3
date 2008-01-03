(* Copyright (C) 1996, POSITRON INDUSTRIES INC.                *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Created by Jacques Dagenais on April 16 1996                *)
(*                                                             *)
(* This interface maps on the linux systems calls to access    *)
(* the serial ports                                            *)
(* It defines constant that are useable by C                   *)

INTERFACE Userial;

FROM Ctypes IMPORT int, unsigned_long_int, unsigned_char;


TYPE
   struct_termios_star = UNTRACED REF struct_termios;
   struct_termios = RECORD
     c_iflag: unsigned_long_int;
     c_oflag: unsigned_long_int;
     c_cflag: unsigned_long_int;
     c_lflag: unsigned_long_int;
     c_line:  unsigned_char;
     c_cc:    ARRAY[0..8] OF unsigned_char;
   END;


CONST
  TCSANOW = 0;

(* c_cflag bit setting *)

  CSIZE   = 8_0000060;
  CS5     = 8_0000000;
  CS6	  = 8_0000020;
  CS7	  = 8_0000040;
  CS8	  = 8_0000060;
  CSTOPB  = 8_0000100;
  CREAD	  = 8_0000200;
  PARENB  = 8_0000400;
  PARODD  = 8_0001000;
  HUPCL	  = 8_0002000;
  CLOCAL  = 8_0004000;
  CBAUDEX = 8_0010000;
  B57600  = 8_0010001;
  B115200 = 8_0010002;
  B230400 = 8_0010003;
  CIBAUD  = 8_002003600000;	
  CRTSCTS = 8_020000000000;

(* Special flags for baudrate settings  *)
  B0    = 8_0000000;
  B50   = 8_0000001;
  B75   = 8_0000002;
  B110  = 8_0000003;
  B134  = 8_0000004;
  B150  = 8_0000005;
  B200  = 8_0000006;
  B300  = 8_0000007;
  B600  = 8_0000010;
  B1200 = 8_0000011;
  B1800 = 8_0000012;
  B2400 = 8_0000013;
  B4800 = 8_0000014;
  B9600 = 8_0000015;
  B19200 = 8_0000016;
  B38400 = 8_0000017;
  BCLEAR = 240;

  DATACLR = 207;

  PARITYCLR = 65471;

  SBCLEAR = 65539;

(* c_iflag bits settings *)

  IGNBRK  = 8_0000001;
  BRKINT  = 8_0000002;
  IGNPAR  = 8_0000004;
  PARMRK  = 8_0000010;
  INPCK   = 8_0000020;
  ISTRIP  = 8_0000040;
  INLCR   = 8_0000100;
  IGNCR   = 8_0000200;
  ICRNL   = 8_0000400;
  IUCLC   = 8_0001000;
  IXON    = 8_0002000;
  IXANY   = 8_0004000;
  IXOFF   = 8_0010000;
  IMAXBEL = 8_0020000;

(* c_oflag *)
  OPOST	 = 8_0000001;
  OLCUC	 = 8_0000002;
  ONLCR	 = 8_0000004;
  OCRNL	 = 8_0000010;
  ONOCR	 = 8_0000020;
  ONLRET = 8_0000040;
  OFILL	 = 8_0000100;
  OFDEL	 = 8_0000200;
  NLDLY	 = 8_0000400;
  NL0	 = 8_0000000;
  NL1	 = 8_0000400;
  CRDLY	 = 8_0003000;
  CR0	 = 8_0000000;
  CR1	 = 8_0001000;
  CR2	  = 8_0002000;
  CR3	  = 8_0003000;
  TABDLY  = 8_0014000;
  TAB0	  = 8_0000000;
  TAB1	  = 8_0004000;
  TAB2	  = 8_0010000;
  TAB3	  = 8_0014000;
  XTABS   = 8_0014000;
  BSDLY	  = 8_0020000;
  BS0	  = 8_0000000;
  BS1	  = 8_0020000;
  VTDLY	  = 8_0040000;
  VT0	  = 8_0000000;
  VT1	  = 8_0040000;
  FFDLY	  = 8_0100000;
  FF0	  = 8_0000000;
  FF1	  = 8_0100000;


(* c_lflag bits *)
  ISIG	   = 8_0000001;
  ICANON   = 8_0000002;
  XCASE	   = 8_0000004;
  ECHO	   = 8_0000010;
  ECHOE	   = 8_0000020;
  ECHOK	   = 8_0000040;
  ECHONL   = 8_0000100;
  NOFLSH   = 8_0000200;
  TOSTOP   = 8_0000400;
  ECHOCTL  = 8_0001000;
  ECHOPRT  = 8_0002000;
  ECHOKE   = 8_0004000;
  FLUSHO   = 8_0010000;
  PENDIN   = 8_0040000;
  IEXTEN   = 8_0100000;

(* TCFLOW stuff *)

  TCOOFF   = 0;
  TCOON    = 1;
  TCIOFF   = 2;
  TCION    = 3;

<*EXTERNAL*>
PROCEDURE tcgetattr (fd: int;  termios_p: struct_termios_star): int;

<*EXTERNAL*>
PROCEDURE tcsetattr (fd: int;  optional_actions: int;
                     termios_p: struct_termios_star): int;

END Userial.











