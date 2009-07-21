(* $Id: Termios.i3,v 1.3 2009-07-21 10:58:57 jkrell Exp $ *)
UNSAFE INTERFACE Termios;

FROM Ctypes IMPORT int;

(*CONST*)
<*EXTERNAL Termios__Stdin*>     VAR Stdin: int;
<*EXTERNAL Termios__Stdout*>    VAR Stdout: int; (* not used *)
<*EXTERNAL Termios__Stderr*>    VAR Stderr: int; (* not used *)
<*EXTERNAL Termios__Tcsanow*>   VAR Tcsanow: int;
<*EXTERNAL Termios__Tcsadrain*> VAR Tcsadrain: int; (* not used *)
<*EXTERNAL Termios__Tcsaflush*> VAR Tcsaflush: int; (* not used *)

TYPE
  T = ADDRESS;

<*EXTERNAL Termios__tcgetattr*> PROCEDURE tcgetattr(fd: int; t: T); (* not used *)
<*EXTERNAL Termios__cfmakeraw*> PROCEDURE cfmakeraw(t: T); (* not used *)
<*EXTERNAL Termios__tcsetattr*> PROCEDURE tcsetattr(fd, action: int; t: T);

END Termios.
