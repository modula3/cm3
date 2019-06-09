(* Copyright (C) 2018-2019 Intel Corporation *)
(* SPDX-License-Identifier: BSD-3-Clause *)
(* see the file COPYRIGHT-INTEL for more information *)

MODULE CoroutineDummy EXPORTS Coroutine;

REVEAL T = BRANDED OBJECT END; (* nothing here *)

PROCEDURE Create(<*UNUSED*>cl : Closure) : T =
  BEGIN <*ASSERT FALSE*> END Create;

PROCEDURE Call(<*UNUSED*>t : T) =
  BEGIN <*ASSERT FALSE*> END Call;

PROCEDURE Retval(<*UNUSED*>t : T) : REFANY =
  BEGIN <*ASSERT FALSE*>; RETURN NIL END Retval;

BEGIN END CoroutineDummy.
  
