(* $Id: Termios.i3,v 1.1.1.1 2001-09-19 11:40:10 wagner Exp $ *)
UNSAFE INTERFACE Termios;

CONST
  Stdin = 0;
  Stdout = 1;
  Stderr = 2;

  Tcsanow = 0;
  Tcsadrain = 1;
  Tcsaflush = 2;
  Tcsasoft = 16;

TYPE
  T = REF ARRAY [0..511] OF CHAR;

<*EXTERNAL tcgetattr*> PROCEDURE tcgetattr(fd: INTEGER; t: T);
<*EXTERNAL cfmakeraw*> PROCEDURE cfmakeraw(t: T);
<*EXTERNAL tcsetattr*> PROCEDURE tcsetattr(fd, action: INTEGER; t: T);

END Termios.
