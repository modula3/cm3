INTERFACE SocketPosix;

(* Move this hack to C so ifdef is there and C backend
 * output is platform-independent.
 *
 * This is an OSF/Ultrix hack so hardly worth anything.
 *)
<*EXTERNAL "SocketPosix__RefetchError"*> PROCEDURE RefetchError(fd: INTEGER);

END SocketPosix.
