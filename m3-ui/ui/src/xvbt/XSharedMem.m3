(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* Last modified on Mon Apr 24 16:50:31 PDT 1995 by msm      *)
(*      modified on Tue Nov 23 14:21:24 PST 1993 by steveg   *)
(*      modified on Fri Oct 22 14:58:51 PDT 1993 by sfreeman *)

UNSAFE MODULE XSharedMem;

IMPORT Completion, Compl, ComplSeq, Ctypes, Picture, PictureRep, IP, M3toC,
       Point, Rect, Text, TrestleComm, VBT, X, XClient, XClientExt,
       XClientF, XPicture, TrestleOnX, XScreenType, XShm;

(* New() exported by XSharedFree *)

(* {{{ -- XClient and XScreenType stuff -- *)

REVEAL
  XClient_T = XClientF.T_Rel BRANDED OBJECT
                wf: WaitFor := NIL; (* this catches all the completion
                                       events for this client *)
                shmEventBase := -1; (* GetEventBase returns -1 on error, so
                                       use it to signify no extension *)
              END;

PROCEDURE InitXClient (v: XClient.T) RAISES {TrestleComm.Failure} =
  BEGIN
    TRY
      IF SameHost(v) AND XShm.QueryExtension(v.dpy) = X.True THEN
        v.shmEventBase := XShm.GetEventBase(v.dpy);
        v.wf := NEW(WaitFor, seq := NEW(ComplSeq.T).init(),
                    timeout := FALSE, timelimit := -1);
        v.wf.types[0] := v.shmEventBase + XShm.ShmCompletion;
        v.wf.types[1] := 0;
        XClientF.RegisterWaiter(v, v.wf);
      END;
    EXCEPT
      X.Error => RAISE TrestleComm.Failure
    END;
  END InitXClient;

PROCEDURE InitXScreenType (<* UNUSED *> st: XScreenType.T) =
  BEGIN
  END InitXScreenType;

PROCEDURE UsesExtension (st: VBT.ScreenType): BOOLEAN =
  BEGIN
    TYPECASE st OF
    | XScreenType.T (xst) => RETURN xst.trsl.shmEventBase # -1;
    ELSE
      RETURN FALSE;
    END;
  END UsesExtension;

PROCEDURE EventBase (v: XClient.T): X.Int =
  BEGIN
    RETURN v.shmEventBase;
  END EventBase;

PROCEDURE PictureUsesExt (st: VBT.ScreenType; picture: Picture.T):
  BOOLEAN =
  BEGIN
    TYPECASE (st) OF
    | XScreenType.T (xst) =>
        TYPECASE (picture) OF
        | T (shpicture) =>
            RETURN
              xst.trsl.shmEventBase # -1 AND shpicture.segmentInfo # NIL
                AND shpicture.dpy = xst.trsl.dpy;
        ELSE
          RETURN FALSE;
        END;
    ELSE
      RETURN FALSE;
    END;
  END PictureUsesExt;

PROCEDURE MakeCompletion (<*UNUSED*> im: T): Completion.T =
  BEGIN
    RETURN Completion.New();
  END MakeCompletion;

(* }}} *)
(* {{{ -- host name stuff -- *)

(* return TRUE if server and client are on same host *)
PROCEDURE SameHost (trsl: XClient.T): BOOLEAN =
  VAR
    display                 := DisplayHost(trsl);
    displayAddr: IP.Address;
  BEGIN
    IF display = NIL THEN RETURN TRUE; END;

    TRY
      IF NOT IP.GetHostByName(display, displayAddr) THEN RETURN FALSE; END;
      RETURN displayAddr = IP.GetHostAddr();
    EXCEPT
    | IP.Error => RETURN FALSE;
    END;
  END SameHost;

PROCEDURE DisplayHost (trsl: XClient.T): TEXT =
  (* return NIL if host is local *)
  VAR display := M3toC.CopyStoT(X.XDisplayString(trsl.dpy));
  BEGIN
    WITH ix = Text.FindChar(display, ':') DO
      IF ix <= 0 THEN RETURN NIL; END;
      display := Text.Sub(display, 0, ix);
    END;
    IF Text.Equal(display, "local") THEN display := NIL; END;
    RETURN display;
  END DisplayHost;

(* }}} *)
(* {{{ -- WaitFor -- *)
(* the WaitFor is protected by the XClient lock *)

TYPE
  WaitFor =
    XClientF.WaitFor OBJECT
      seq: ComplSeq.T := NIL;
      (* we assume that XShm Completion events arrive in the same order as
         their related XShmPutPicture.  There is an element in the sequence
         for each X call which generates an X Completion event *)
      nextSerial: Ctypes.unsigned_long;  (* cache of seq.getLo().serial *)
      nextSerialValid := FALSE;  (* false when seq.size() = 0 *)
    METHODS
      addC (xserial: Ctypes.unsigned_long; c: Completion.T) := AddC;
      (* append the details of the X request to the sequence *)
    OVERRIDES
      match  := Match;
      notify := Notify;
    END;

PROCEDURE AddC (wf: WaitFor; xserial: Ctypes.unsigned_long; c: Completion.T) =
  BEGIN
    WITH compl = Compl.Get() DO
      compl.serial := xserial;
      compl.completion := c;
      wf.seq.addhi(compl);
      IF NOT wf.nextSerialValid THEN
        wf.nextSerial := xserial;
        wf.nextSerialValid := TRUE;
      END;
    END;
  END AddC;

PROCEDURE Match (wf: WaitFor; READONLY ev: X.XEvent): BOOLEAN =
  VAR serial: Ctypes.unsigned_int;
  BEGIN
    WITH any = LOOPHOLE(ADR(ev), X.XAnyEventStar) DO
      IF any.type = 0 THEN
        WITH error = LOOPHOLE(ADR(ev), X.XErrorEventStar) DO
          serial := error.serial;
        END;
      ELSE
        serial := any.serial;
      END;
    END;
    RETURN wf.nextSerialValid AND wf.nextSerial = serial;
  END Match;

PROCEDURE Notify (wf: WaitFor; READONLY ev: X.XEvent; xcon: XClient.T) =
  VAR serial: Ctypes.unsigned_int;
  BEGIN
    WITH seq  = wf.seq,
         size = seq.size() DO
      <* ASSERT size > 0 *>
      WITH compl = wf.seq.remlo(),
           e     = LOOPHOLE(ADR(ev), X.XAnyEventStar) DO
        IF e.type = 0 THEN
          serial := LOOPHOLE(e, X.XErrorEventStar).serial;
        ELSE
          serial := e.serial;
        END;
        <* ASSERT compl.serial = serial *>
        compl.completion.dec();
        compl.completion := NIL; (* so it can be collected *)
        Compl.Free(compl);
      END;

      IF size > 1 THEN
        wf.nextSerial := seq.getlo().serial;
        (* we know wf.nextSerialValid = TRUE *)
      ELSE
        wf.nextSerialValid := FALSE;
      END;
    END;
    XClientF.RegisterWaiter(xcon, wf); (* wf will have been removed from
                                          the list *)
  END Notify;

(* }}} *)
(* {{{ -- picture type and methods -- *)

REVEAL
  T = XPicture.T BRANDED "XSharedMem.Picture" OBJECT
        xcon: XClient.T     := NIL;
        dpy : X.DisplayStar := NIL;
        (* a shared memory segment is associated with a particular display
           so this field is set during the initialisation.  If a caller
           attempts to put the picture to another display, it is sent using
           XPutPicture. *)
        segmentInfo: XShm.SegmentInfoStar := NIL;
      OVERRIDES
        init          := Init;
        initFromImage := InitFromImage;
        attachData    := AttachData;
        detachData    := DetachData;
        destroy       := Destroy;
        put           := Put;
      END;

PROCEDURE Init (t: Picture.T; st: VBT.ScreenType; width, height: CARDINAL):
  Picture.T RAISES {Picture.ScreenTypeNotSupported, Picture.TrestleFail} =
  VAR
    picture                       := NARROW(t, T);
    shminfo: XShm.SegmentInfoStar;
  BEGIN
    TRY
      TYPECASE st OF
      | XScreenType.T (xst) =>
          shminfo := NewSegment();
          TrestleOnX.Enter(xst.trsl);
          TRY
            WITH trsl = xst.trsl,
                 ximage = XShm.CreateImage(
                            trsl.dpy, xst.visual,
                            X.XDefaultDepth(trsl.dpy, xst.screenID),
                            X.ZPixmap, NIL, shminfo, width, height) DO
              IF ximage = NIL THEN
                FreeSegment(shminfo);
                RAISE Picture.TrestleFail;
              END;
              picture.dpy := trsl.dpy;
              picture.allocByCaller := FALSE;
              picture.image := LOOPHOLE(ximage, Picture.ImageStar);
              picture.segmentInfo := shminfo;
              picture.xcon := trsl;
            END
          FINALLY
            TrestleOnX.Exit(xst.trsl)
          END;
      ELSE
        RAISE Picture.ScreenTypeNotSupported;
      END;
      EVAL Picture.T.init(picture, st, width, height);
    EXCEPT
      X.Error, TrestleComm.Failure => RAISE Picture.TrestleFail
    END;
    RETURN picture;
  END Init;

PROCEDURE InitFromImage (im          : Picture.T;
                         st          : VBT.ScreenType;
                         image       : Picture.ImageStar;
                         sharedMemory                      := FALSE):
  Picture.T RAISES {Picture.ScreenTypeNotSupported, Picture.TrestleFail} =
  BEGIN
    EVAL XPicture.T.initFromImage(im, st, image, sharedMemory);

    IF sharedMemory THEN
      <* ASSERT ISTYPE(im, T) *>
      WITH t = NARROW(im, T) DO
        IF st = NIL THEN RAISE Picture.ScreenTypeNotSupported; END;
        TYPECASE st OF
        | XScreenType.T (xst) =>
            <* ASSERT image.obdata # NIL *>
            (* obdata is used to hold the segment info *)
            t.dpy := xst.trsl.dpy;
            t.segmentInfo := LOOPHOLE(image.obdata, XShm.SegmentInfoStar);
            t.xcon := xst.trsl;
        ELSE
          RAISE Picture.ScreenTypeNotSupported;
        END;
      END;
    END;
    RETURN im;
  END InitFromImage;

CONST
  ReadOnly       = ARRAY BOOLEAN OF X.Bool{X.False, X.True};
  InvalidSegment = -1;

PROCEDURE AttachData (t      : Picture.T;
                      dataPtr: Ctypes.char_star;
                      info   : Picture.SharedMemInfo := NIL)
  RAISES {Picture.TrestleFail} =
  VAR picture: T;
  BEGIN
    TYPECASE t OF
    | T (it) => picture := it;
    ELSE
      RAISE Picture.TrestleFail;
    END;

    TRY
      picture.image.data := dataPtr;
      IF picture.segmentInfo # NIL THEN
        IF info = NIL THEN
          (* treat as ordinary data *)
          picture.segmentInfo.shmid := InvalidSegment;
          picture.segmentInfo.shmaddr := NIL;
        ELSE
          picture.segmentInfo.shmid := info.id;
          picture.segmentInfo.shmaddr := dataPtr;
          picture.segmentInfo.readOnly := ReadOnly[info.readOnly];
          TrestleOnX.Enter(picture.xcon);
          TRY
            IF XShm.Attach(picture.dpy, picture.segmentInfo) # X.True THEN
              RAISE Picture.TrestleFail;
            END
          FINALLY
            TrestleOnX.Exit(picture.xcon)
          END;
        END;
      END;
    EXCEPT
      X.Error, TrestleComm.Failure => RAISE Picture.TrestleFail
    END;
  END AttachData;

PROCEDURE DetachData (t: Picture.T) RAISES {Picture.TrestleFail} =
  BEGIN
    TRY
      TYPECASE (t) OF
      | T (xshm) =>
          IF xshm.image # NIL THEN
            IF xshm.dpy # NIL AND xshm.segmentInfo # NIL THEN
              TrestleOnX.Enter(xshm.xcon);
              TRY
                IF XShm.Detach(xshm.dpy, xshm.segmentInfo) # X.True THEN
                  RAISE Picture.TrestleFail;
                END
              FINALLY
                TrestleOnX.Exit(xshm.xcon)
              END;
              FreeSegment(xshm.segmentInfo);
              xshm.segmentInfo := NIL;
            END;
          END;
      | XPicture.T (xpicture) => XPicture.T.detachData(xpicture);
      ELSE
        Picture.T.detachData(t);
      END;
    EXCEPT
      X.Error, TrestleComm.Failure => RAISE Picture.TrestleFail
    END;
  END DetachData;

PROCEDURE Destroy (t: Picture.T) =
  <* FATAL Picture.TrestleFail *>
  BEGIN
    TYPECASE (t) OF
    | T (xshm) =>
        (* don't free the ximage if it was allocated by someone else. *)
        IF xshm.image # NIL AND NOT xshm.allocByCaller THEN
          DetachData(t);

          xshm.image.data := NIL;
          (* XDestroyImage frees the data as well, but it doesn't belong to
             us *)
          EVAL
            xshm.image.f.destroy_image(LOOPHOLE(xshm.image, X.XImageStar));
          xshm.image := NIL;
        END;

        IF xshm.segmentInfo # NIL THEN
          FreeSegment(xshm.segmentInfo);
          xshm.segmentInfo := NIL;
        END;
    | XPicture.T (xpicture) => XPicture.T.destroy(xpicture);
    ELSE
      Picture.T.destroy(t);      (* will crash *)
    END;
  END Destroy;

PROCEDURE Put (         t         : XPicture.T;
                        dpy       : X.DisplayStar;
                        d         : X.Drawable;
                        gc        : X.GC;
               READONLY clip      : Rect.T;
               READONLY delta     : Point.T;
                        completion: Completion.T   )
  RAISES {TrestleComm.Failure} =
  BEGIN
    WITH picture = NARROW(t, T) DO
      IF dpy # picture.dpy OR picture.segmentInfo = NIL
           OR picture.segmentInfo.shmid = InvalidSegment THEN
        (* this is not the display the picture data is attached to, or
           there is no shared memory segment *)
        XPicture.T.put(t, dpy, d, gc, clip, delta, completion);
      ELSE
        VAR
          imageStar := LOOPHOLE(t.image, X.XImageStar);
          clp := Rect.Meet(clip, Rect.FromCorner(delta, imageStar.width,
                                                 imageStar.height));
          srcX   := clp.west - delta.h;
          srcY   := clp.north - delta.v;
          width  := clp.east - clp.west;
          height := clp.south - clp.north;
        BEGIN
          (* we could remove the last 12 pixels from the width as these are
             blank with the JVideo card width := MIN(width,
             (imageStar.width - srcX - 12)); *)
          IF 0 < width AND 0 < height THEN
            completion.inc();    (* decremented when X.CompletionEvent
                                    arrives *)
            picture.xcon.wf.addC(X.XNextRequest(dpy), completion);
            TRY
              WITH status = XShm.PutImage(
                              dpy, d, gc, imageStar, srcX, srcY, clp.west,
                              clp.north, width, height, X.True) DO
                <* ASSERT status = X.True *>
              END;
            EXCEPT
              X.Error => RAISE TrestleComm.Failure;
            END;
          END;
        END;
      END;
    END;
  END Put;

(* }}} *)
(* {{{ -- free list for Segment Info -- *)
(* these procedures provide clean access to the free list.  Freeing is
   explicit as the record is not traced *)

PROCEDURE NewSegment (): XShm.SegmentInfoStar =
  VAR res: SegInfoStar := NIL;
  BEGIN
    LOCK freeMu DO
      IF freeSegs # NIL THEN res := freeSegs; freeSegs := res.next; END;
    END;
    IF res = NIL THEN res := NEW(SegInfoStar); END;

    RETURN LOOPHOLE(res, XShm.SegmentInfoStar);
  END NewSegment;

PROCEDURE FreeSegment (s: XShm.SegmentInfoStar) =
  BEGIN
    WITH si = LOOPHOLE(s, SegInfoStar) DO
      LOCK freeMu DO si.next := freeSegs; freeSegs := si; END;
    END;
  END FreeSegment;

TYPE
  SegInfo = RECORD
              s   : XShm.SegmentInfo;
              next: SegInfoStar;
            END;
  SegInfoStar = UNTRACED REF SegInfo;

VAR
  freeMu                := NEW(MUTEX);
  freeSegs: SegInfoStar := NIL;

(* }}} *)
BEGIN
END XSharedMem.
