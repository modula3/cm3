(* Copyright (C) 2022 Peter McKinna. All rights reserved. *)
(* See file COPYRIGHT-BSD for details. *)

UNSAFE INTERFACE RTEHScan;

IMPORT RTStack;

PROCEDURE ScanEHTable(VAR f : RTStack.Frame; excUid : INTEGER) : BOOLEAN;

END RTEHScan.
