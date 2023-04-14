(* Copyright (C) 2023, Peter McKinna *)
(* All rights reserved.              *)
(* Licensed under the MIT license.   *)

INTERFACE LSP;

PROCEDURE HandleMsg (msg : TEXT) : BOOLEAN;

PROCEDURE CheckFile(uri : TEXT);

END LSP.
