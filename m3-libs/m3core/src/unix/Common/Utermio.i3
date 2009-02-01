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
 * Last Modified By: Douglas H. Quebbeman
 * Last Modified On: Sun May 26 15:26:00 2002
 * Update Count    : 85
 * 
 *)

INTERFACE Utermio;
IMPORT Usysdep;
FROM Ctypes IMPORT int;

(*CONST*)
VAR
  B110: int;
  B115200: int;
  B1200: int;
  B14400: int;
  B19200: int;
  B230400: int;
  B2400: int;
  B300: int;
  B38400: int;
  B4800: int;
  B57600: int;
  B600: int;
  B75: int;
  B9600: int;
  BAUDBITS: int;
  CREAD: int;
  CS5: int;
  CS6: int;
  CS7: int;
  CS8: int;
  CSIZE: int;
  CSTOPB: int;
  IGNBRK: int;
  IGNPAR: int;
  PAREVEN: int;
  PARITYBITS: int;
  PARNONE: int;
  PARODD: int;
  TCSANOW: int;
  VMIN: int;
  VTIME: int;

TYPE
  struct_termios = Usysdep.struct_termios;
  speed_t = Usysdep.speed_t;

<*EXTERNAL*> PROCEDURE tcgetattr (fildes: int; termios_p: UNTRACED REF struct_termios): int; 
<*EXTERNAL*> PROCEDURE tcsetattr(fildes, optional_actions: int; termios_p: UNTRACED REF struct_termios) : int;
<*EXTERNAL*> PROCEDURE tcsendbreak(fildes, duration: int): int;
<*EXTERNAL*> PROCEDURE tcdrain(fildes: int): int;
<*EXTERNAL*> PROCEDURE tcflush(fildes, queue_selector: int): int;
<*EXTERNAL*> PROCEDURE tcflow(fildes, action: int): int;
<*EXTERNAL*> PROCEDURE cfgetospeed(termios_p: UNTRACED REF struct_termios): speed_t;
<*EXTERNAL*> PROCEDURE cfsetospeed(termios_p: UNTRACED REF struct_termios; speed: speed_t): int;
<*EXTERNAL*> PROCEDURE cfgetispeed(termios_p: UNTRACED REF struct_termios): speed_t;
<*EXTERNAL*> PROCEDURE cfsetispeed(termios_p: UNTRACED REF struct_termios; speed: speed_t): int;

END Utermio.
