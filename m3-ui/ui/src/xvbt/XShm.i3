(* Copyright (C) 1993, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* Last modified on Mon Nov 22 12:11:00 PST 1993 by steveg   *)
(*      modified on Thu Nov 11 10:45:05 PST 1993 by kalsow   *)

UNSAFE INTERFACE XShm;

(* M3 version of X Shared memory extension interface *)

IMPORT X, Ctypes;

CONST
  ShmQueryVersion = 0;
  ShmAttach       = 1;
  ShmDetach       = 2;
  ShmPutImage     = 3;
  ShmGetImage     = 4;
  ShmCreatePixmap = 5;

CONST
  ShmCompletion   = 0;
  ShmNumberEvents = ShmCompletion + 1;

  BadShmSeg       = 0;
  ShmNumberErrors = BadShmSeg + 1;


TYPE ShmSeg = Ctypes.unsigned_int;

TYPE
  CompletionEvent =
    RECORD
      type: X.Int;               (* of event *)
      serial: Ctypes.unsigned_long;  (* # of last request processed by
                                        server *)
      send_event: X.Bool;        (* true if this came frome a SendEvent
                                    request *)
      display   : X.DisplayStar;  (* Display the event was read from *)
      drawable  : X.Drawable;     (* drawable of request *)
      major_code: X.Int;          (* ShmReqCode *)
      minor_code: X.Int;          (* X_ShmPutImage *)
      shmseg    : ShmSeg;         (* the ShmSeg used in the request *)
      offset: Ctypes.unsigned_long;  (* the offset into ShmSeg used in the
                                        request *)
    END;
  CompletionEventStar = UNTRACED REF CompletionEvent;

TYPE
  SegmentInfo =
    RECORD
      shmseg : ShmSeg;           (* resource id *)
      shmid  : X.Int            := -1; (* kernel id *)
      shmaddr: Ctypes.char_star := NIL; (* address in client *)
      readOnly: X.Bool := X.False; (* how the server should attach it *)
    END;
  SegmentInfoStar = UNTRACED REF SegmentInfo;

<* EXTERNAL XShmQueryExtension *>
  PROCEDURE QueryExtension (dpy: X.DisplayStar): X.Bool RAISES {X.Error};

<* EXTERNAL XShmGetEventBase *>
  PROCEDURE GetEventBase (dpy: X.DisplayStar): X.Int RAISES {X.Error};
(* returns -1 for errors *)

<* EXTERNAL XShmQueryVersion *>
  PROCEDURE QueryVersion (dpy          : X.DisplayStar;
                          majorVersion : Ctypes.int_star;
                          minorVersion : Ctypes.int_star;
                          sharedPixmaps: X.BoolStar       ): X.Bool RAISES {X.Error};

<* EXTERNAL XShmAttach*>
  PROCEDURE Attach (dpy: X.DisplayStar; shminfo: SegmentInfoStar):
  X.Status RAISES {X.Error};

<* EXTERNAL XShmDetach *>
  PROCEDURE Detach (dpy: X.DisplayStar; shminfo: SegmentInfoStar):
  X.Status RAISES {X.Error};

<* EXTERNAL XShmPutImage *>
  PROCEDURE PutImage (dpy                  : X.DisplayStar;
                      d                    : X.Drawable;
                      gc                   : X.GC;
                      image                : X.XImageStar;
                      src_x, src_y         : X.Int;
                      dst_x, dst_y         : X.Int;
                      src_width, src_height: Ctypes.unsigned_int;
                      sendEvent            : X.Bool;              ):
  X.Status RAISES {X.Error};

<* EXTERNAL XShmGetImage *>
  PROCEDURE GetImage (dpy       : X.DisplayStar;
                      d         : X.Drawable;
                      image     : X.XImageStar;
                      x, y      : X.Int;
                      plane_mask: X.Mask         ): X.Status RAISES {X.Error};

<* EXTERNAL XShmCreateImage *>
  PROCEDURE CreateImage (dpy          : X.DisplayStar;
                         visual       : X.VisualStar;
                         depth        : Ctypes.unsigned_int;
                         format       : X.Int;
                         data         : Ctypes.char_star;
                         shminfo      : SegmentInfoStar;
                         width, height: Ctypes.unsigned_int  ):
  X.XImageStar RAISES {X.Error};

<* EXTERNAL XShmCreatePixmap *>
  PROCEDURE CreatePixmap (dpy                 : X.DisplayStar;
                          d                   : X.Drawable;
                          data                : Ctypes.char_star;
                          shminfo             : SegmentInfoStar;
                          widht, height, depth: Ctypes.unsigned_int):
  X.Pixmap RAISES {X.Error};

END XShm.
