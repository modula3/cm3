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
 * Last Modified On: Sun Feb 16 15:26:53 1997
 * Update Count    : 84
 * 
 *)

INTERFACE Utermio;

FROM Word IMPORT Shift, Or;
FROM Ctypes IMPORT char, int;
FROM Utypes IMPORT u_short, u_char, u_long;

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
  (* these are defined in /usr/include/sys/termios.h, but conflict
     with CSIZE below.  So, we won't use them:
  B57600  	= 8_0000020;
  B76800  	= 8_0000021;
  B115200 	= 8_0000022;
  B153600       = 8_0000023;
  B230400 	= 8_0000024;
  B307200       = 8_0000025;
  B460800 	= 8_0000026;
  *)
  B57600  	= -1;
  B76800  	= -1;
  B115200 	= -1;
  B153600       = -1;
  B230400 	= -1;
  B307200       = -1;
  B460800 	= -1;

  (* c_cflag bit setting *)
  CSIZE         = 16_0000300;
  CS5           = 16_0000000;
  CS6           = 16_0000100;
  CS7           = 16_0000200;
  CS8		= 16_0000300;

  CSTOPB	= 16_0000400;
  CREAD		= 16_0000800;

  PARITYBITS    = 16_0003000;
  PARNONE       = 16_0000000;
  PAREVEN	= 16_0001000;
  PARODD	= 16_0003000;

  HUPCL		= 16_0004000;

  CLOCAL	= 16_0008000;
  (** LOBLK         = 8_0040000; **)

  (* Input Modes *)
  IGNBRK	= 16_0000001;
  BRKINT        = 16_0000002;
  IGNPAR	= 16_0000004;
  PARMRK        = 16_0000008;
  INPCK         = 16_0000010;
  ISTRIP        = 16_0000020;
  INLCR         = 16_0000040;
  IGNCR         = 16_0000080;
  ICRNL         = 16_0000100;

  (* line discipline modes *)
  ISIG          = 16_00000080;
  ICANON        = 16_00000100;
  ECHO          = 16_00000008;
  ECHOE         = 16_00000002;
  ECHOK         = 16_00000004;
  ECHONL        = 16_00000010;
  NOFLSH        = 16_80000000;
  TOSTOP        = 16_00400000;

  (* control characters *)
  VEOF          = 0;
  VEOL          = 1;
  VEOL2         = 2;
  VERASE        = 3;
  VWERASE       = 4;
  VKILL         = 5;
  VREPRINT      = 6;
  (* spare      = 7; *)
  VINTR         = 8;
  VQUIT         = 9;
  VSUSP         = 10;
  VDSUSP        = 11;
  VSTART        = 12;
  VSTOP         = 13;
  VLNEXT        = 14;
  VDISCARD      = 15;
  VFLUSH        = VDISCARD;
  VMIN          = 16;
  VTIME         = 17;
  VSTATUS       = 18;
  (* spare      = 19; *)

  NCCS          = 20;

  (* wrong in the Unix file *)
  TC 	 = Shift (ORD ('T'), 8);
  TIOC   = TC;

  TCGETA   = Or(TIOC, 1);
  TCSETA   = Or(TIOC, 2);
  TCSETAW  = Or(TIOC, 3);
  TCSETAF  = Or(TIOC, 4);
  TCSBRK   = Or(TIOC, 5);
  TCXONC   = Or(TIOC, 6);
  TCFLSH   = Or(TIOC, 7);

  TCGETS   = Or(TIOC, 13);
  TCSETS   = Or(TIOC, 14);
  TCSANOW  = Or(TIOC, 14);
  TCSETSW  = Or(TIOC, 15);
  TCSADRAIN = Or(TIOC, 15);
  TCSETSF   = Or(TIOC, 16);
  TCSAFLUSH = Or(TIOC, 16);

  TCIFLUSH	= 0;  (* flush data received but not read *)
  TCOFLUSH	= 1;  (* flush data written but not transmitted *)
  TCIOFLUSH	= 2;  (* flush both data both input and output queues *)

  NCC		= 8;

TYPE
  struct_termio = RECORD
	c_iflag:   u_short := 0;	 (* input modes *)
	c_oflag:   u_short := 0;	 (* output modes *)
	c_cflag:   u_short := 0;	 (* control modes *)
	c_lflag:   u_short := 0;	 (* line discipline modes *)
	c_line:    char    := VAL(0, char); (* line discipline *)
	c_cc := ARRAY [0..NCCS-1] OF u_char{0,..}; (* control chars *)
  END;

  speed_t  = u_long;
  cc_t     = u_char;
  tcflag_t = u_long;

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
