(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* Last modified on Mon Oct 11 20:56:22 PDT 1993 by sfreeman *)

INTERFACE Picture;

(* this interface supports writing Pictures to the display.  It is supposed
   to be platform-independant but bears a striking resemblance to the X
   version.  It also includes some hooks for shared memory transmission of
   the pictures; these are ignored if the implementation does not support
   them. *)

IMPORT Ctypes, Point, Rect, Thread, VBT;

EXCEPTION
  ScreenTypeNotSupported;
  TrestleFail;                   (* the implementation returned an error *)

<*PRAGMA LL *>

TYPE T <: ROOT;

PROCEDURE New (st: VBT.ScreenType; width, height: CARDINAL): T
  RAISES {ScreenTypeNotSupported, TrestleFail};
(* create a new T of size /width/ * /height/, based on origin =
   Point.Origin.  Further details, including the use of shared memory, are
   extracted from /v/.  All the fields in the Picture.Image which are not
   accessible in the interface are set to the server's defaults *)

PROCEDURE FromImage (st          : VBT.ScreenType;
                     image       : ImageStar;
                     sharedMemory                   := FALSE): T
  RAISES {ScreenTypeNotSupported, TrestleFail};
(* create a new T based on an existing Picture.  The caller is also
   responsible for freeing /image/ as Picture.T will not do so for storage
   it has not allocated.  /sharedMemory/ is a hint to the implementation *)

TYPE FreeProc = PROCEDURE (param: REFANY);
PROCEDURE Paint (         v        : VBT.Leaf;
                          src      : T;
                 READONLY clip                  := Rect.Full;
                 READONLY delta                 := Point.Origin;
                          freeProc : FreeProc   := NIL;
                          freeParam: REFANY     := NIL           )
  RAISES {Thread.Alerted};
<* LL.sup < v *>
(* translate the picture /src/ by /delta/, and paint it to the screen of
   /v/, clipping to rectangle /clip/.  To place the picture with the top
   left corner aligned with /v/, set /delta/ := Rect.NorthWest(v.domain).

   If /freeProc/ # NIL then /Put/ will return after the paint request has
   been made and /freeProc/ will be called with parameter /freeParam/ when
   the Picture data structures may safely be changed.  If /freeProc/ = NIL,
   then /Put/ will not return until the Picture data structures are safe *)

PROCEDURE Destroy (picture: T);
(* destroy /picture/, detaching any shared memory segments first.  This
   procedure will be called before /picture/ is collected if it hasn't been
   already. *)

TYPE
  SharedMemInfo = OBJECT
                    id     : Ctypes.int;  (* shared memory identifier *)
                    address: ADDRESS      := NIL;
                    (* address to which shared memory is attached *)
                    readOnly := FALSE;
                    (* is the server allowed to write to the segment? *)
                  END;


PROCEDURE AttachData (picture: T;
                      dataPtr: Ctypes.char_star;
                      shmInfo: SharedMemInfo      := NIL)
  RAISES {TrestleFail};
(* attach the data segment /dataPtr/ to the Picture.  The caller is
   expected to have acquired the memory in which to put the picture.  If
   /shmInfo/ # NIL then the implementation will use shared memory to
   transmit the picture if it can.  The caller is responsible for freeing
   the data segment *)

PROCEDURE DetachData (picture: T) RAISES {TrestleFail};
(* detach the data from "picture".  This call does not free any data
   allocated *)

PROCEDURE Image (picture: T): ImageStar;
(* return the X.ImageStar associated with /picture/.  Can be NIL *)

PROCEDURE MakeImage (st            : VBT.ScreenType;
                     width, height : Ctypes.int;
                     xoffset       : Ctypes.int       := 0;
                     bitmap_pad    : Ctypes.int       := 0;
                     bytes_per_line: Ctypes.int       := 0  ): ImageStar
  RAISES {ScreenTypeNotSupported, TrestleFail};
<* LL >= VBT.mu *>
(* create an ImageRec for the given parameters.  The format is assumed to
   be ZPixmap.  It is the caller's responsibility to free the storage.

   The ImageStar is not suitable for use with Shared Memory *)

PROCEDURE Supported (st: VBT.ScreenType; sharedMem := FALSE): BOOLEAN;
(* return TRUE if this screen type is supported by the Picture module.  If
   "sharedMem" is TRUE then also test whether the screen type supports
   shared memory for the transmission of pictures *)

(* the following is the raw structure for an image, which bears a
   remarkable resemblance to an XImage structure.  See the X manuals for a
   description of its fields *)

TYPE
  CreateImageProc = PROCEDURE (): ImageStar;
  DestroyImageProc = PROCEDURE (i: ImageStar): Ctypes.int;
  GetPixelProc =
    PROCEDURE (i: ImageStar; x, y: Ctypes.int): Ctypes.unsigned_long;
  PutPixelProc =
    PROCEDURE (i: ImageStar; x, y: Ctypes.int; p: Ctypes.unsigned_long):
      Ctypes.int;
  SubImageProc =
    PROCEDURE (i: ImageStar; x, y: Ctypes.int; w, h: Ctypes.unsigned_int):
      ImageStar;
  AddPixelProc = PROCEDURE (i: ImageStar): Ctypes.int;

  ImageRec =
    RECORD
      width, height: Ctypes.int;  (* size of image *)
      xoffset: Ctypes.int;       (* number of pixels offset in X
                                    direction *)
      format: Ctypes.int;        (* XYBitmap, XYPixmap, ZPixmap *)
      data  : Ctypes.char_star;  (* pointer to image data *)
      byte_order : Ctypes.int;   (* data byte order, LSBFirst, MSBFirst *)
      bitmap_unit: Ctypes.int;   (* quant.  of scanline 8, 16, 32 *)
      bitmap_bit_order: Ctypes.int;  (* LSBFirst, MSBFirst *)
      bitmap_pad      : Ctypes.int;  (* 8, 16, 32 either XY or ZPixmap *)
      depth           : Ctypes.int;  (* depth of image *)
      bytes_per_line  : Ctypes.int;  (* accelarator to next line *)
      bits_per_pixel  : Ctypes.int;  (* bits per pixel (ZPixmap) *)
      red_mask  : Ctypes.unsigned_long;  (* bits in z arrangment *)
      green_mask: Ctypes.unsigned_long;
      blue_mask : Ctypes.unsigned_long;
      obdata: Ctypes.char_star;  (* hook for the object routines to hang
                                    on *)
      f: RECORD                  (* image manipulation routines *)
           create_image : CreateImageProc;
           destroy_image: DestroyImageProc;
           get_pixel    : GetPixelProc;
           put_pixel    : PutPixelProc;
           sub_image    : SubImageProc;
           add_pixel    : AddPixelProc;
         END;
    END;
  ImageStar = UNTRACED REF ImageRec;

END Picture.
