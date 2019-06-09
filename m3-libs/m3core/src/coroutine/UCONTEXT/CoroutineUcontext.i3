(* Copyright (C) 2018-2019 Intel Corporation *)
(* SPDX-License-Identifier: BSD-3-Clause *)
(* see the file COPYRIGHT-INTEL for more information *)

UNSAFE INTERFACE CoroutineUcontext;
FROM Coroutine IMPORT T, Closure;

TYPE
  Arg = REF RECORD
    cl          : Closure;
    firstcaller : T;  (* special trick for passing initial caller into 
                         a coroutine the first time it runs, gets NILed 
                         right away *)
    id          : Id; (* need this to find my coroutine in Run *)
  END;

  Entry = PROCEDURE(arg : Arg);

  Id = UNTRACED REF INTEGER;
  (* this strange declaration is used because our Id is held as a thread
     local (i.e., global) with pthreads thread local system.  That system
     can only handle pointers. *)
      
END CoroutineUcontext.
