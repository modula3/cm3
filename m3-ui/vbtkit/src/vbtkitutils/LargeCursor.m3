(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Tue Aug 27 11:40:26 PDT 1996 by najork                   *)
(*      modified on Tue Mar 28 14:43:05 PST 1995 by mhb                      *)
(*       Created on Wed Jan 11 21:59:00 PST 1995 by najork                   *)

MODULE LargeCursor;

IMPORT Cursor, Point, Rect, ScrnPixmap, Word;

TYPE
  Byte = [16_00 .. 16_ff];


(* This part has to be modified for each new cursor pixmap.  You can use 
   the "bitmap" program (a standard X utility) to design the cursor.
   "bitmap" saves a file that contains a snippet of C code. Use this
   code as a template to fill in "width", "height", and "bits"
   (remember to replace "0x" by "16_" when copying "bits"). 
   You also have to fill in "hotspot" and "color". *)

CONST
  width  = 36;
  height = 36;
  bits   = ARRAY OF Byte {
                      16_00, 16_00, 16_00, 16_00, 16_00, 16_00, 
                      16_00, 16_00, 16_00, 16_06, 16_00, 16_00,
                      16_00, 16_80, 16_07, 16_00, 16_00, 16_00, 
                      16_f0, 16_03, 16_00, 16_00, 16_00, 16_fc,
                      16_03, 16_00, 16_00, 16_00, 16_ff, 16_01, 
                      16_00, 16_00, 16_c0, 16_ff, 16_01, 16_00,
                      16_00, 16_f0, 16_ff, 16_01, 16_00, 16_00, 
                      16_fe, 16_ff, 16_00, 16_00, 16_80, 16_ff,
                      16_ff, 16_00, 16_00, 16_e0, 16_ff, 16_7f, 
                      16_00, 16_00, 16_f8, 16_ff, 16_7f, 16_00,
                      16_00, 16_fe, 16_ff, 16_3f, 16_00, 16_00, 
                      16_ff, 16_ff, 16_3f, 16_00, 16_00, 16_fe,
                      16_ff, 16_1f, 16_00, 16_00, 16_fc, 16_ff, 
                      16_1f, 16_00, 16_00, 16_f8, 16_ff, 16_0f,
                      16_00, 16_00, 16_f0, 16_ff, 16_0f, 16_00, 
                      16_00, 16_e0, 16_ff, 16_0f, 16_00, 16_00,
                      16_f0, 16_ff, 16_07, 16_00, 16_00, 16_f8, 
                      16_ff, 16_07, 16_00, 16_00, 16_fc, 16_ff,
                      16_03, 16_00, 16_00, 16_fe, 16_ff, 16_03, 
                      16_00, 16_00, 16_ff, 16_fd, 16_01, 16_00,
                      16_80, 16_ff, 16_f8, 16_01, 16_00, 16_c0, 
                      16_7f, 16_f0, 16_00, 16_00, 16_e0, 16_3f,
                      16_e0, 16_00, 16_00, 16_f0, 16_1f, 16_40, 
                      16_00, 16_00, 16_f8, 16_0f, 16_00, 16_00,
                      16_00, 16_fc, 16_07, 16_00, 16_00, 16_00, 
                      16_fe, 16_03, 16_00, 16_00, 16_00, 16_fe,
                      16_01, 16_00, 16_00, 16_00, 16_fc, 16_00, 
                      16_00, 16_00, 16_00, 16_78, 16_00, 16_00,
                      16_00, 16_00, 16_30, 16_00, 16_00, 16_00, 
                      16_00, 16_00, 16_00, 16_00, 16_00, 16_00};
  hotspot = Point.T {0, 0};
  color   = Cursor.RGB{1.0, 0.0, 0.0};

(********** End of part that has to be modified for new pixmap **********)


PROCEDURE NewNWArrow (): Cursor.T =
  CONST 
    linelength = (((width - 1) DIV 8) + 1) * 8;
  VAR 
    pm := ScrnPixmap.NewRaw (1, Rect.T{0, width, 0, height});
  BEGIN
    FOR i := 0 TO height-1 DO 
      FOR j := 0 TO width-1 DO
        WITH byte = bits[(i * linelength + j) DIV 8],
             bit  = Word.Extract(byte,j MOD 8,1) DO
          pm.set(Point.T{width-1-j,i}, bit);
        END;
      END;
    END;

    RETURN Cursor.FromRaw(Cursor.Raw {
                                       plane1  := pm,
                                       plane2  := pm,
                                       hotspot := hotspot,
                                       color1  := color,
                                       color2  := Cursor.RGB{0.0, 0.0, 0.0},
                                       color3  := Cursor.RGB{0.0, 0.0, 0.0}});
  END NewNWArrow;


BEGIN
  NWArrow := NewNWArrow();
END LargeCursor.
