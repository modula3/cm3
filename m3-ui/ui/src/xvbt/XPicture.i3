(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* Last modified on Mon Nov 22 12:08:31 PST 1993 by steveg   *)
(*      modified on Wed Oct  6 09:23:51 PDT 1993 by sfreeman *)

UNSAFE INTERFACE XPicture;

IMPORT Completion, Ctypes, Picture, Point, Rect, TrestleComm, VBT, X;

PROCEDURE MakeImage (st            : VBT.ScreenType;
                     width, height : Ctypes.int;
                     xoffset       : Ctypes.int       := 0;
                     bitmap_pad    : Ctypes.int       := 0;
                     bytes_per_line: Ctypes.int       := 0  ):
  Picture.ImageStar
  RAISES {Picture.ScreenTypeNotSupported, Picture.TrestleFail};
(* implements Picture.MakeImage *)

PROCEDURE MakeCompletion (picture: T): Completion.T;
(* used for PictureRep.MakeCompletion *)

TYPE
  T <: Public;
  Public = Picture.T OBJECT
           METHODS
             put (         dpy       : X.DisplayStar;
                           d         : X.Drawable;
                           gc        : X.GC;
                  READONLY clip      : Rect.T;
                  READONLY delta     : Point.T;
                           completion: Completion.T   ) RAISES {TrestleComm.Failure};
             (* this method is called by XPaint to put the picture on the
                actual screen.  It returns when the it has finished with
                the image data.  The caller is responsible for making sure
                the image data is protected *)
           END;

PROCEDURE New (): T;
(* pictures got from this procedure will be returned to the free list when
   done with *)

END XPicture.
