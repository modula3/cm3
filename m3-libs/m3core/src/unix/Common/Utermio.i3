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
FROM Ctypes IMPORT const_int, int;

VAR
  B110: const_int;
  B115200: const_int;
  B1200: const_int;
  B14400: const_int;
  B19200: const_int;
  B230400: const_int;
  B2400: const_int;
  B300: const_int;
  B38400: const_int;
  B4800: const_int;
  B57600: const_int;
  B600: const_int;
  B75: const_int;
  B9600: const_int;
  BAUDBITS: const_int;
  CREAD: const_int;
  CS5: const_int;
  CS6: const_int;
  CS7: const_int;
  CS8: const_int;
  CSIZE: const_int;
  CSTOPB: const_int;
  IGNBRK: const_int;
  IGNPAR: const_int;
  PAREVEN: const_int;
  PARITYBITS: const_int;
  PARNONE: const_int;
  PARODD: const_int;
  TCSANOW: const_int;
  VMIN: const_int;
  VTIME: const_int;

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
