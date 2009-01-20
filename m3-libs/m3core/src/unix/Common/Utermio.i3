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

<*EXTERNAL*> INTERFACE Utermio;
IMPORT Usysdep;
FROM Ctypes IMPORT int;

(*CONST*)
<*EXTERNAL "Utermio_B110"*> VAR B110: int;
<*EXTERNAL "Utermio_B115200"*> VAR B115200: int;
<*EXTERNAL "Utermio_B1200"*> VAR B1200: int;
<*EXTERNAL "Utermio_B14400"*> VAR B14400: int;
<*EXTERNAL "Utermio_B19200"*> VAR B19200: int;
<*EXTERNAL "Utermio_B230400"*> VAR B230400: int;
<*EXTERNAL "Utermio_B2400"*> VAR B2400: int;
<*EXTERNAL "Utermio_B300"*> VAR B300: int;
<*EXTERNAL "Utermio_B38400"*> VAR B38400: int;
<*EXTERNAL "Utermio_B4800"*> VAR B4800: int;
<*EXTERNAL "Utermio_B57600"*> VAR B57600: int;
<*EXTERNAL "Utermio_B600"*> VAR B600: int;
<*EXTERNAL "Utermio_B75"*> VAR B75: int;
<*EXTERNAL "Utermio_B9600"*> VAR B9600: int;
<*EXTERNAL "Utermio_BAUDBITS"*> VAR BAUDBITS: int;
<*EXTERNAL "Utermio_CREAD"*> VAR CREAD: int;
<*EXTERNAL "Utermio_CS5"*> VAR CS5: int;
<*EXTERNAL "Utermio_CS6"*> VAR CS6: int;
<*EXTERNAL "Utermio_CS7"*> VAR CS7: int;
<*EXTERNAL "Utermio_CS8"*> VAR CS8: int;
<*EXTERNAL "Utermio_CSIZE"*> VAR CSIZE: int;
<*EXTERNAL "Utermio_CSTOPB"*> VAR CSTOPB: int;
<*EXTERNAL "Utermio_IGNBRK"*> VAR IGNBRK: int;
<*EXTERNAL "Utermio_IGNPAR"*> VAR IGNPAR: int;
<*EXTERNAL "Utermio_PAREVEN"*> VAR PAREVEN: int;
<*EXTERNAL "Utermio_PARITYBITS"*> VAR PARITYBITS: int;
<*EXTERNAL "Utermio_PARNONE"*> VAR PARNONE: int;
<*EXTERNAL "Utermio_PARODD"*> VAR PARODD: int;
<*EXTERNAL "Utermio_TCSANOW"*> VAR TCSANOW: int;
<*EXTERNAL "Utermio_VMIN"*> VAR VMIN: int;
<*EXTERNAL "Utermio_VTIME"*> VAR VTIME: int;

TYPE
  struct_termios = Usysdep.struct_termios;
  speed_t = Usysdep.speed_t;

PROCEDURE tcgetattr (fildes: int; termios_p: UNTRACED REF struct_termios): int; 
PROCEDURE tcsetattr(fildes, optional_actions: int; termios_p: UNTRACED REF struct_termios) : int;
PROCEDURE tcsendbreak(fildes, duration: int): int;
PROCEDURE tcdrain(fildes: int): int;
PROCEDURE tcflush(fildes, queue_selector: int): int;
PROCEDURE tcflow(fildes, action: int): int;
PROCEDURE cfgetospeed(termios_p: UNTRACED REF struct_termios): speed_t;
PROCEDURE cfsetospeed(termios_p: UNTRACED REF struct_termios; speed: speed_t): int;
PROCEDURE cfgetispeed(termios_p: UNTRACED REF struct_termios): speed_t;
PROCEDURE cfsetispeed(termios_p: UNTRACED REF struct_termios; speed: speed_t): int;

END Utermio.
