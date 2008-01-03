(* Copyright (C) 1995, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* by Ted Wobber                                             *)
(*                                                           *)
(* Last modified on Mon Feb  6 17:05:55 PST 1995 by wobber   *)

MODULE WinSock;

IMPORT Word;

PROCEDURE FD_CLR(s: SOCKET; VAR set: struct_fd_set) =
  VAR i: u_int := 0;
  BEGIN
    WHILE (i < set.fd_count) DO
      IF s = set.fd_array[i] THEN
        WHILE i < set.fd_count-1 DO
          set.fd_array[i] := set.fd_array[i+1];
          INC(i);
        END;
        DEC(set.fd_count);
        RETURN;
      END;
      INC(i);
    END;
  END FD_CLR;

PROCEDURE FD_SET(s: SOCKET; VAR set: struct_fd_set) =
  BEGIN
    IF set.fd_count < FD_SETSIZE THEN
      set.fd_array[set.fd_count] := s;
      INC(set.fd_count);
    END;
  END FD_SET;

PROCEDURE FD_ZERO(VAR set: struct_fd_set) =
  BEGIN
    set.fd_count := 0;
  END FD_ZERO;

PROCEDURE FD_ISSET(s: SOCKET; VAR set: struct_fd_set): BOOLEAN =
  BEGIN
    FOR i := 0 TO set.fd_count-1 DO
      IF s = set.fd_array[i] THEN
        RETURN TRUE;
      END;
    END;
    RETURN FALSE;
  END FD_ISSET;


PROCEDURE IN_CLASSA(in: struct_in_addr): BOOLEAN =
  BEGIN
    RETURN (Word.And(in.s_addr, 16_80000000) = 0);
  END IN_CLASSA;

PROCEDURE IN_CLASSB(in: struct_in_addr): BOOLEAN =
  BEGIN
    RETURN (Word.And(in.s_addr, 16_c0000000) = 16_80000000);
  END IN_CLASSB;

PROCEDURE IN_CLASSC(in: struct_in_addr): BOOLEAN =
  BEGIN
    RETURN (Word.And(in.s_addr, 16_e0000000) = 16_c0000000);
  END IN_CLASSC;


BEGIN
END WinSock.
