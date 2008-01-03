(*
 * For information about this program, contact Blair MacIntyre            
 * (bm@cs.columbia.edu) or Steven Feiner (feiner@cs.columbia.edu)         
 * at the Computer Science Dept., Columbia University,                    
 * 1214 Amsterdam Ave. Mailstop 0401, New York, NY, 10027.                
 *                                                                        
 * Copyright (C) 1995, 1996 by The Trustees of Columbia University in the 
 * City of New York.  Blair MacIntyre, Computer Science Department.       
 * 
 * Author          : Blair MacIntyre
 * Created On      : Mon Jan 30 15:33:55 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Sun Feb 16 15:25:20 1997
 * Update Count    : 70
 * 
 *)

INTERFACE Utermio;

FROM Ctypes IMPORT char, int;
FROM Utypes IMPORT u_short, u_char, u_int;

CONST
  (* Taken from /usr/include/sys/termio.h and related files. *)
  CBAUD         = 8_0000037;
  BAUDBITS      = 8_0000037;
  B0      	= 8_0000000;
  B50     	= 8_0000001;
  B75     	= 8_0000002;
  B110    	= 8_0000003;
  B134    	= 8_0000004;
  B150    	= 8_0000005;
  B200    	= 8_0000006;
  B300    	= 8_0000007;
  B600    	= 8_0000010;
  B900    	= 8_0000011;
  B1200   	= 8_0000012;
  B1800   	= 8_0000013;
  B2400   	= 8_0000014;
  B3600   	= 8_0000015;
  B4800   	= 8_0000016;
  B7200   	= 8_0000017;
  B9600   	= 8_0000020;
  B14400        = -1;			(* unsupported should be marked thus *)
  B19200  	= 8_0000021;
  B38400  	= 8_0000022;
  B57600  	= 8_0000023;
  B115200 	= 8_0000024;
  B230400 	= 8_0000025;
  B460800 	= 8_0000026;

  (* c_cflag bit setting *)
  CSIZE     	= 8_0000140;
  CS5           = 8_0000000;
  CS6           = 8_0000040;
  CS7           = 8_0000100;
  CS8		= 8_0000140;

  CSTOPB        = 8_0000200;
  CREAD		= 8_0000400;

  PARITYBITS    = 8_0003000;
  PARNONE       = 8_0000000;
  PAREVEN	= 8_0001000;
  PARODD	= 8_0003000;

  HUPCL		= 8_0004000;

  CLOCAL	= 8_0010000;
  LOBLK         = 8_0020000;

  (* input modes *)
  IGNBRK        = 8_0000001;
  BRKINT        = 8_0000002;
  IGNPAR        = 8_0000004;
  PARMRK        = 8_0000010;
  INPCK         = 8_0000020;
  ISTRIP        = 8_0000040;
  INLCR         = 8_0000100;
  IGNCR         = 8_0000200;
  ICRNL         = 8_0000400;
  IUCLC   	= 8_0001000;
  IXON    	= 8_0002000;
  IXANY   	= 8_0004000;
  IXOFF   	= 8_0010000;
  IENQAK  	= 8_0020000;
  IMAXBEL 	= 8_0040000;

  (* c_oflag *)
  OPOST	 	= 8_0000001;
  OLCUC	 	= 8_0000002;
  ONLCR	 	= 8_0000004;
  OCRNL	 	= 8_0000010;
  ONOCR	 	= 8_0000020;
  ONLRET 	= 8_0000040;
  OFILL	 	= 8_0000100;
  OFDEL	 	= 8_0000200;
  NLDLY	 	= 8_0000400;

  NL0	 	= 8_0000000;
  NL1	 	= 8_0000400;

  CRDLY	 	= 8_0003000;
  CR0	 	= 8_0000000;
  CR1	 	= 8_0001000;
  CR2	  	= 8_0002000;
  CR3	  	= 8_0003000;

  TABDLY  	= 8_0014000;
  TAB0	  	= 8_0000000;
  TAB1	  	= 8_0004000;
  TAB2	  	= 8_0010000;
  TAB3	  	= 8_0014000;
  XTABS   	= 8_0014000;

  BSDLY	  	= 8_0020000;
  BS0	  	= 8_0000000;
  BS1	  	= 8_0020000;

  VTDLY	  	= 8_0040000;
  VT0	  	= 8_0000000;
  VT1	  	= 8_0040000;

  FFDLY	  	= 8_0100000;
  FF0	  	= 8_0000000;
  FF1	  	= 8_0100000;

  (* line discipline modes *)
  ISIG          = 8_0000001;
  ICANON        = 8_0000002;
  ECHO          = 8_0000010;
  ECHOE         = 8_0000020;
  ECHOK         = 8_0000040;
  ECHONL        = 8_0000100;
  NOFLSH        = 8_0000200;
  ECHOCTL  	= 8_0000400;
  ECHOPRT  	= 8_0001000;
  ECHOKE   	= 8_0002000;
  FLUSHO   	= 8_0004000;
  PENDIN   	= 8_0010000;
  TOSTOP        = 8_010000000000;
  IEXTEN        = 8_020000000000;

  (* control characters *)
  VINTR         = 0;
  VQUIT         = 1;
  VERASE        = 2;
  VKILL         = 3;
  VEOF          = 4;
  VEOL          = 5;
  VEOL2         = 6;
  VMIN          = 4;
  VTIME         = 5;
  VSWTCH        = 7;
  VSTART        = 14;
  VSTOP         = 15;
  VSUSP         = 13;

  TCIFLUSH	= 0;  (* flush data received but not read *)
  TCOFLUSH	= 1;  (* flush data written but not transmitted *)
  TCIOFLUSH	= 2;  (* flush both data both input and output queues *)

  NCC		= 8;
  NCCS		= 16;

  TCSANOW       = 0;
  TCSADRAIN     = 1;
  TCSAFLUSH     = 2;

TYPE
  struct_termio = RECORD
	c_iflag:   u_short := 0;	 (* input modes *)
	c_oflag:   u_short := 0;	 (* output modes *)
	c_cflag:   u_short := 0;	 (* control modes *)
	c_lflag:   u_short := 0;	 (* line discipline modes *)
	c_line:    char    := VAL(0, char); (* line discipline *)
	c_cc := ARRAY [0..NCC-1] OF u_char{0,..}; (* control chars *)
  END;
  struct_termio_star = UNTRACED REF struct_termio;

  speed_t  = u_int;
  tcflag_t = u_int;
  cc_t     = u_char;

  struct_termios = RECORD
	c_iflag:   tcflag_t := 0;	 (* input modes *)
	c_oflag:   tcflag_t := 0;	 (* output modes *)
	c_cflag:   tcflag_t := 0;	 (* control modes *)
	c_lflag:   tcflag_t := 0;	 (* line discipline modes *)
	c_reserved: tcflag_t := 0;	 (* line discipline modes *)
	c_cc := ARRAY [0..NCCS-1] OF cc_t{0,..}; (* control chars *)
  END;
  struct_termios_star = UNTRACED REF struct_termios;

<*EXTERNAL "tcgetattr"*> 
PROCEDURE tcgetattr (fildes: int; 
                     termios_p: struct_termios_star): int; 

<*EXTERNAL "tcsetattr"*> 
PROCEDURE tcsetattr(fildes, optional_actions: int;
                    termios_p: struct_termios_star) : int;

<*EXTERNAL "tcsendbreak"*> PROCEDURE tcsendbreak(fildes, duration: int): int;

<*EXTERNAL "tcdrain"*> PROCEDURE tcdrain(fildes: int): int;

<*EXTERNAL "tcflush"*> PROCEDURE tcflush(fildes, queue_selector: int): int;

<*EXTERNAL "tcflow"*> PROCEDURE  tcflow(fildes, action: int): int;

<*EXTERNAL "cfgetospeed"*> 
PROCEDURE cfgetospeed(termios_p: struct_termios_star): speed_t;

<*EXTERNAL "cfsetospeed"*> 
PROCEDURE cfsetospeed(termios_p: struct_termios_star;
                      speed: speed_t): int;

<*EXTERNAL "cfgetispeed"*> 
PROCEDURE cfgetispeed(termios_p: struct_termios_star): speed_t;

<*EXTERNAL "cfsetispeed"*> 
PROCEDURE cfsetispeed(termios_p: struct_termios_star;
                      speed: speed_t): int;
 
END Utermio.
