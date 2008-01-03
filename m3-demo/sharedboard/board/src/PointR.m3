(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)

MODULE PointR;

PROCEDURE Add (READONLY p, q: T): T =
  BEGIN RETURN T{p.h + q.h, p.v + q.v} END Add;

BEGIN
END PointR.
