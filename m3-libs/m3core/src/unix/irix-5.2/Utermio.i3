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
 * Last Modified On: Sun Feb 16 15:25:52 1997
 * Update Count    : 69
 * 
 *)

INTERFACE Utermio;

FROM Word IMPORT Shift, Or;
FROM Ctypes IMPORT char, int;
FROM Utypes IMPORT u_long, u_char, u_short;

CONST
  (* Taken from /usr/include/sys/termio.h and related files. *)
  CBAUD         = 8_0000017;
  BAUDBITS      = 8_0000017;
  B0      	= 8_0000000;
  B50     	= 8_0000001;
  B75     	= 8_0000002;
  B110    	= 8_0000003;
  B134    	= 8_0000004;
  B150    	= 8_0000005;
  B200    	= 8_0000006;
  B300    	= 8_0000007;
  B600    	= 8_0000010;
  B1200   	= 8_0000011;
  B1800   	= 8_0000012;
  B2400   	= 8_0000013;
  B4800   	= 8_0000014;
  B9600   	= 8_0000015;
  B14400        = -1;			(* unsupported should be marked thus *)
  B19200  	= 8_0000016;
  B38400  	= 8_0000017;
  B57600  	= -1;
  B76800  	= -1;
  B115200 	= -1;
  B153600       = -1;
  B230400 	= -1;
  B307200       = -1;
  B460800 	= -1;

  CSIZE         = 8_0000060;
  CS5           = 0;
  CS6           = 8_0000020;
  CS7           = 8_0000040;
  CS8		= 8_0000060;

  PARITYBITS    = 8_00001400;
  PARNONE       = 8_00000000;
  PAREVEN	= 8_00000400;
  PARODD	= 8_00001400;

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

  (* line discipline modes *)
  ISIG          = 8_0000001;
  ICANON        = 8_0000002;
  ECHO          = 8_0000010;
  ECHOE         = 8_0000020;
  ECHOK         = 8_0000040;
  ECHONL        = 8_0000100;
  NOFLSH        = 8_0000200;
  IEXTEN        = 8_0000400;
  ITOSTOP       = 8_0100000;
  TOSTOP        = ITOSTOP;

  (* control modes *)
  CSTOPB	= 8_0000100;
  CREAD		= 8_0000200;
  HUPCL	        = 8_0002000;
  CLOCAL	= 8_0004000;

  NCC		= 8;
  NCCS		= 23;

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
  VSTART        = 8;
  VSTOP         = 9;
  VSUSP         = 10;
  VDSUSP        = 11;
  VREPRINT      = 12;
  VDISCARD      = 13;
  VWERASE       = 14;
  VLNEXT        = 15;

  TCIFLUSH	= 0;  (* flush data received but not read *)
  TCOFLUSH	= 1;  (* flush data written but not transmitted *)
  TCIOFLUSH	= 2;  (* flush both data both input and output queues *)

  (* wrong in the Unix file *)
  TC 	 = Shift (ORD ('T'), 8);
  TIOC   = TC;

  TCSANOW  = Or(TIOC, 14);
  TCSETSW  = Or(TIOC, 15);
  TCSADRAIN = Or(TIOC, 15);
  TCSETSF   = Or(TIOC, 16);
  TCSAFLUSH = Or(TIOC, 16);

TYPE
  struct_termio = RECORD
	c_iflag:   u_short := 0;	 (* input modes *)
	c_oflag:   u_short := 0;	 (* output modes *)
	c_cflag:   u_short := 0;	 (* control modes *)
	c_lflag:   u_short := 0;	 (* line discipline modes *)
	c_line:    char    := VAL(0, char); (* line discipline *)
	c_cc := ARRAY [0..NCC-1] OF u_char{0,..}; (* control chars *)
  END;

  speed_t  = u_long;
  tcflag_t = u_long;
  cc_t     = u_char;

  struct_termios = RECORD
	c_iflag:   tcflag_t := 0;	 (* input modes *)
	c_oflag:   tcflag_t := 0;	 (* output modes *)
	c_cflag:   tcflag_t := 0;	 (* control modes *)
	c_lflag:   tcflag_t := 0;	 (* line discipline modes *)
	c_cc := ARRAY [0..NCCS-1] OF cc_t{0,..}; (* control chars *)
  END;

<*EXTERNAL "tcgetattr"*> 
PROCEDURE tcgetattr (fildes: int; 
                     termios_p: UNTRACED REF struct_termios): int; 

<*EXTERNAL "tcsetattr"*> 
PROCEDURE tcsetattr(fildes, optional_actions: int;
                    termios_p: UNTRACED REF struct_termios) : int;

<*EXTERNAL "tcsendbreak"*> PROCEDURE tcsendbreak(fildes, duration: int): int;

<*EXTERNAL "tcdrain"*> PROCEDURE tcdrain(fildes: int): int;

<*EXTERNAL "tcflush"*> PROCEDURE tcflush(fildes, queue_selector: int): int;

<*EXTERNAL "tcflow"*> PROCEDURE  tcflow(fildes, action: int): int;

<*EXTERNAL "cfgetospeed"*> 
PROCEDURE cfgetospeed(termios_p: UNTRACED REF struct_termios): speed_t;

<*EXTERNAL "cfsetospeed"*> 
PROCEDURE cfsetospeed(termios_p: UNTRACED REF struct_termios;
                      speed: speed_t): int;

<*EXTERNAL "cfgetispeed"*> 
PROCEDURE cfgetispeed(termios_p: UNTRACED REF struct_termios): speed_t;

<*EXTERNAL "cfsetispeed"*> 
PROCEDURE cfsetispeed(termios_p: UNTRACED REF struct_termios;
                      speed: speed_t): int;
 
END Utermio.
