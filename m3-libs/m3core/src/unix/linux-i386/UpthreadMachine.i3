(* Copyright (C) 1994, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

INTERFACE UpthreadMachine;

(* <bits/pthreadtypes.h> *)

CONST
  SIZEOF_PTHREAD_ATTR_T        = 36;
  SIZEOF_PTHREAD_MUTEX_T       = 24;
  SIZEOF_PTHREAD_RWLOCK_T      = 32;

END UpthreadMachine.
