(* Copyright (C) 2026 Modula-3 contributors. All rights reserved.       *)
(* See file COPYRIGHT-CMASS for details.                                *)

(* The result of a call to a nested procedure passed by reference to a
   READONLY formal (simplegrid's Rect(), arithmetic): MSIR gave up on the
   address of a nested procedure's result. *)

MODULE Main;

IMPORT Test;

TYPE
  Point = RECORD x, y: INTEGER END;
  Rect  = RECORD ll, ur: Point END;
  Small = SET OF [0 .. 15];

PROCEDURE Area (READONLY r: Rect): INTEGER =
  BEGIN
    RETURN (r.ur.x - r.ll.x) * (r.ur.y - r.ll.y);
  END Area;

PROCEDURE Twice (READONLY i: INTEGER): INTEGER =
  BEGIN RETURN 2 * i END Twice;

PROCEDURE Ord (READONLY c: CHAR): INTEGER =
  BEGIN RETURN ORD (c) END Ord;

PROCEDURE Card (READONLY s: Small): INTEGER =
  VAR n := 0;
  BEGIN
    FOR i := 0 TO 15 DO IF i IN s THEN INC (n) END END;
    RETURN n;
  END Card;

PROCEDURE Outer (w, h: INTEGER): INTEGER =
  VAR ll, ur: Point;  total := 0;

  PROCEDURE Box (): Rect =
    BEGIN RETURN Rect {ll, ur} END Box;

  PROCEDURE Width (): INTEGER =
    BEGIN RETURN ur.x - ll.x END Width;

  PROCEDURE Letter (): CHAR =
    BEGIN RETURN VAL (ORD ('a') + w, CHAR) END Letter;

  PROCEDURE Bits (): Small =
    BEGIN RETURN Small {0 .. h} END Bits;

  BEGIN
    ll := Point {1, 2};
    ur := Point {1 + w, 2 + h};
    <* ASSERT Area (Box ()) = w * h *>
    INC (total, Area (Box ()));
    INC (total, Twice (Width ()));
    INC (total, Ord (Letter ()));
    INC (total, Card (Bits ()));
    RETURN total;
  END Outer;

BEGIN
  (* w * h + 2 * w + ORD ('a') + w + h + 1 *)
  Test.checkI (Outer (3, 4), 12 + 6 + 100 + 5);
  Test.checkI (Outer (0, 0), 0 + 0 + 97 + 1);
  Test.done ();
END Main.
