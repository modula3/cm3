(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* Last modified on Tue Jan 31 12:59:34 PST 1995 by kalsow   *)
(*      modified on Wed Oct  6 09:23:22 PDT 1993 by sfreeman *)

UNSAFE MODULE PictureImpl EXPORTS Picture, PictureRep;

IMPORT Completion, Ctypes, VBT, X, XPicture, XScreenType, XSharedMem;

(* this module provides the implementation-dependant parts of Picture.i3;
   in particular, X provides routines for allocating XImages. *)

PROCEDURE New (st: VBT.ScreenType; width, height: CARDINAL): T
  RAISES {ScreenTypeNotSupported, TrestleFail} =
  BEGIN
    TYPECASE (st) OF
    | XScreenType.T => RETURN NewPicture(st).init(st, width, height);
    ELSE
      RAISE ScreenTypeNotSupported;
    END;
  END New;

PROCEDURE FromImage (st          : VBT.ScreenType;
                     image       : ImageStar;
                     sharedMemory                   := FALSE): T
  RAISES {ScreenTypeNotSupported, TrestleFail} =
  BEGIN
    TYPECASE (st) OF
    | XScreenType.T =>
        RETURN NewPicture(st).initFromImage(st, image, sharedMemory);
    ELSE
      RAISE ScreenTypeNotSupported;
    END;
  END FromImage;

PROCEDURE NewPicture (st: XScreenType.T): XPicture.T =
  BEGIN
    IF XSharedMem.UsesExtension(st) THEN
      RETURN XSharedMem.New();
    ELSE
      RETURN XPicture.New();
    END;
  END NewPicture;

PROCEDURE MakeImage (st            : VBT.ScreenType;
                     width, height : Ctypes.int;
                     xoffset       : Ctypes.int       := 0;
                     bitmap_pad    : Ctypes.int       := 0;
                     bytes_per_line: Ctypes.int       := 0  ): ImageStar
  RAISES {ScreenTypeNotSupported, TrestleFail} =
  BEGIN
    TYPECASE (st) OF
    | XScreenType.T =>
        RETURN XPicture.MakeImage(
                 st, width, height, xoffset, bitmap_pad, bytes_per_line);
    ELSE
      RAISE ScreenTypeNotSupported;
    END;
  END MakeImage;

PROCEDURE MakeCompletion (picture: T): Completion.T =
  BEGIN
    TYPECASE (picture) OF
    | XSharedMem.T (xsh) => RETURN XSharedMem.MakeCompletion(xsh);
    | XPicture.T (xim) => RETURN XPicture.MakeCompletion(xim);
    ELSE
      RETURN Completion.New();
    END
  END MakeCompletion;

PROCEDURE Supported (st: VBT.ScreenType; sharedMem := FALSE): BOOLEAN =
  BEGIN
    TYPECASE st OF
    | XScreenType.T (xst) =>
        IF sharedMem THEN
          RETURN XSharedMem.UsesExtension(xst);
        ELSE
          RETURN TRUE;
        END;
    ELSE
      RETURN FALSE;
    END;
  END Supported;

PROCEDURE GetXpmImage (screenType : VBT.ScreenType; file : TEXT) : T
  RAISES {TrestleFail} =
  BEGIN
    RETURN XPicture.GetXpmImage(screenType, file);
  END GetXpmImage;

BEGIN
  (* this is a dumb check to try to keep Picture and XPicture in step *)
  <*ASSERT BYTESIZE(X.XImage) = BYTESIZE(ImageRec)*>
END PictureImpl.
