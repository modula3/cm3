UNSAFE INTERFACE TermC;
IMPORT Termios;

<*EXTERNAL TermC__Init*>PROCEDURE Init();
<*EXTERNAL TermC__GetTermCooked*>PROCEDURE GetTermCooked(): Termios.T;
<*EXTERNAL TermC__GetTermRaw*>PROCEDURE GetTermRaw(): Termios.T;

END TermC.
