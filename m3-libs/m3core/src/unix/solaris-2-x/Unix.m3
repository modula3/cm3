(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Mon Oct 24 15:21:36 PDT 1994 by kalsow     *)

(***************************************************************************)
(*   Author:  Geoffrey Wyant - Sun Microsystems Labs BOS		   *)
(***************************************************************************)

(* $Revision: 1.1.1.1 $ *)

MODULE Unix;

IMPORT Uerror;
FROM   Ctypes IMPORT int;

(*
 * This is to work around a bug in the Solaris-2 'libsocket' library 
 * which redefines 'fcntl' in such a way as to zero out 'errno' if the
 * call is successful.
 *)
PROCEDURE fcntl(fd, request, arg: int): int =
  VAR
    errno := Uerror.errno;
    res   := raw_fcntl(fd, request, arg);
  BEGIN
    IF res = 0 THEN
      Uerror.errno := errno;
    END;
    RETURN res;
  END fcntl;


(*
 * This is to work around a bug in the Solaris-2 'libsocket' library 
 * which redefines 'ioctl' in such a way as to zero out 'errno' if the
 * call is successful.
 *)
PROCEDURE ioctl(fd, request: int; argp: ADDRESS): int =
  VAR
    errno := Uerror.errno;
    res   := raw_ioctl(fd, request, argp);
  BEGIN
    IF res = 0 THEN
      Uerror.errno := errno;
    END;
    RETURN res;
  END ioctl;

BEGIN
END Unix.
