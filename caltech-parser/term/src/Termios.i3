(* $Id: Termios.i3,v 1.4 2009-07-21 11:18:42 jkrell Exp $ *)
UNSAFE INTERFACE Termios;

FROM Ctypes IMPORT int;

(*CONST*)
<*EXTERNAL Termios__Stdin*>     VAR Stdin: int;
<*EXTERNAL Termios__Tcsanow*>   VAR Tcsanow: int;

TYPE
  T = ADDRESS;

<*EXTERNAL Termios__tcsetattr*> PROCEDURE tcsetattr(fd, action: int; t: T);

END Termios.
