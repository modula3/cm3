(* $Id$ *)
UNSAFE INTERFACE Termios;

FROM Ctypes IMPORT int;

(*CONST*)
<*EXTERNAL Termios__Stdin*>     VAR Stdin: int;
<*EXTERNAL Termios__Tcsanow*>   VAR Tcsanow: int;

TYPE
  T = ADDRESS;

<*EXTERNAL Termios__tcsetattr*> PROCEDURE tcsetattr(fd, action: int; t: T);

END Termios.
