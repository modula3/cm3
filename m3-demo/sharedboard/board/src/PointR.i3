(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)

INTERFACE PointR;

TYPE T = RECORD h, v: REAL END;

CONST Origin = T { 0.0, 0.0 };

PROCEDURE Add (READONLY p, q: T): T;
(* Return "T{p.h + q.h, p.v + q.v}". *)


END PointR.
