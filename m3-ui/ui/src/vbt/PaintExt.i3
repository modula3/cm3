(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* Last modified on Mon Oct  4 11:37:10 PDT 1993 by sfreeman *)
(* modified on Mon Feb 24 13:57:19 PST 1992 by muller *)
(* modified on Fri Sep 6 17:25:31 PDT 1991 by gnelson *)
<*PRAGMA LL*>

INTERFACE PaintExt;

IMPORT PaintPrivate, Word, Point, VBT;

TYPE
  PathRec = RECORD
              curveCount: CARDINAL;
              (* data: ARRAY OF Word.T *)
            END;
  PathPtr = UNTRACED REF PathRec;

  FillRec = RECORD
              ext  : PaintPrivate.ExtensionRec;
              delta: Point.T;
              wind : BITS Word.Size FOR VBT.WindingCondition;
              path : PathRec
            END;
  FillPtr = UNTRACED REF FillRec;

CONST FillCommand = 0;

TYPE
  StrokeRec = RECORD
                ext  : PaintPrivate.ExtensionRec;
                delta: Point.T;
                width: CARDINAL;
                end  : BITS Word.Size FOR VBT.EndStyle;
                join : BITS Word.Size FOR VBT.JoinStyle;
                path : PathRec
              END;
  StrokePtr = UNTRACED REF StrokeRec;

CONST StrokeCommand = 1;

TYPE
  LineRec = RECORD
              ext  : PaintPrivate.ExtensionRec;
              delta: Point.T;
              width: CARDINAL;
              end  : BITS Word.Size FOR VBT.EndStyle;
              join : BITS Word.Size FOR VBT.JoinStyle;
              p, q : Point.T;
            END;
  LinePtr = UNTRACED REF LineRec;

CONST LineCommand = 2;

TYPE
  PictureRec =
    RECORD
      ext       : PaintPrivate.ExtensionRec;
      picture   : ADDRESS;
      completion: ADDRESS;
      (* /picture/ and /completion/ are really an Picture.T and
         PictureRep.Completion, respectively, but we don't want to put
         traced storage in an untraced structure (and so lose the safety of
         the interface).  Consequently, the creator of this record must
         hang on to the picture and completion objects to stop them being
         moved by the collector.

         trust us... *)
    END;
  PicturePtr = UNTRACED REF PictureRec;

CONST PictureCommand = 3;

END PaintExt.
