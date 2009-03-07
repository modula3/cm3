(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
INTERFACE Test;

IMPORT Text;
IMPORT Usysdep;
IMPORT Csetjmp;

VAR
  errors:   INTEGER := 0;
  warnings: INTEGER := 0;

TYPE
  T = RECORD
    d := ARRAY [1..11] OF LONGREAL {0.0d0, 1.0d0, 2.0d0, 3.0d0, -1.0d0, -2.0d0, -3.0d0, 12.34d0, -124.456d0, 1000.0d0, -10000.0d0};
    f := ARRAY [1..11] OF     REAL {0.0e0, 1.0e0, 2.0e0, 3.0e0, -1.0e0, -2.0e0, -3.0e0, 12.34e0, -124.456e0, 1000.0e0, -10000.0e0};
    sizes : RECORD
      (* keep these sorted by name for easier human comprehension *)
      gid := BYTESIZE(Usysdep.gid_t);
      hostent_addrtype_t := BYTESIZE(Usysdep.hostent_addrtype_t);
      hostent_length_t := BYTESIZE(Usysdep.hostent_length_t);
      linger := BYTESIZE(Usysdep.struct_linger);
      pid := BYTESIZE(Usysdep.pid_t);
      socklen := BYTESIZE(Usysdep.socklen_t);
      time := BYTESIZE(Usysdep.time_t);
      timeval := BYTESIZE(Usysdep.struct_timeval);
      tm := BYTESIZE(Usysdep.struct_tm);
      uid := BYTESIZE(Usysdep.uid_t);
      (* pthreads omitted on purpose *)
    END;
  END;

<*EXTERNAL "Test__CheckFloatsAndTypes"*> PROCEDURE CheckFloatsAndTypes(READONLY t:T; size := BYTESIZE(T); jbsize := BYTESIZE(Csetjmp.jmp_buf));

PROCEDURE msg (t: Text.T);
PROCEDURE msgB (b: BOOLEAN);
PROCEDURE msgI (i: INTEGER);
PROCEDURE msgC (c: CHAR);
PROCEDURE msgR (r: REAL);

PROCEDURE check (b: BOOLEAN);
PROCEDURE checkM (b: BOOLEAN; msg: TEXT);
PROCEDURE checkB (b, shouldBe: BOOLEAN);
PROCEDURE checkI (i, shouldBe: INTEGER);
PROCEDURE checkN (i, shouldBe: LONGINT);
PROCEDURE checkC (i, shouldBe: CHAR);
PROCEDURE checkR (r, shouldBe: REAL);
PROCEDURE checkL (r, shouldBe: LONGREAL);
PROCEDURE checkX (r, shouldBe: EXTENDED);

PROCEDURE warn (b: BOOLEAN);

PROCEDURE Err (a, b, c, d: TEXT := NIL);
PROCEDURE Out (a, b, c, d, e: TEXT := NIL);

PROCEDURE done ();

END Test.

