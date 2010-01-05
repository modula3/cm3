(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE Csetjmp;

FROM Ctypes IMPORT int, double;

TYPE jmp_buf = RECORD
      RP    : int;
      SP    : int;
      RET0  : int;
      GR0   : int;
      GR3   : int;
      GR4   : int;
      GR5   : int;
      GR6   : int;
      GR7   : int;
      GR8   : int;
      GR9   : int;
      GR10  : int;
      GR11  : int;
      GR12  : int;
      GR13  : int;
      GR14  : int;
      GR15  : int;
      GR16  : int;
      GR17  : int;
      GR18  : int;
      JUNK1 : int;
      SR3   : int;
      FR12  : double;
      FR13  : double;
      FR14  : double;
      FR15  : double;
      ARG1  : int;
      JUNK2 : int;
      FR16  : double;
      FR17  : double;
      FR18  : double;
      FR19  : double;
      FR20  : double;
      FR21  : double;
      RPP   : int;
      LTP   : int;
      JUNK3 : int;
      JUNK4 : int;
      JUNK5 : int;
      JUNK6 : int;
   END;

<*EXTERNAL "_longjmp" *> PROCEDURE ulongjmp (VAR env: jmp_buf; val: int);

END Csetjmp.
