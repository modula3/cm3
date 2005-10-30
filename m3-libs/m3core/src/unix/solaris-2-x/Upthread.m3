(*
 * Copyright (c) 1997-2001 by Sun Microsystems, Inc.
 * All rights reserved.
 *)

MODULE Upthread;

IMPORT Uthread;

BEGIN
  PTHREAD_STACK_MIN := Uthread.min_stack();
END Upthread.
