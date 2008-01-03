(* Copyright (C) 1993, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* Last modified on Mon Apr 24 16:55:28 PDT 1995 by msm      *)
(*      modified on Tue Jan 31 09:23:06 PST 1995 by kalsow   *)
(*      modified on Mon Oct 11 20:54:25 PDT 1993 by sfreeman *)

UNSAFE MODULE Picture EXPORTS Picture, PictureRep;

IMPORT Batch, BatchRep, BatchUtil, Completion, Ctypes, PictureRep,
       PaintExt, PaintPrivate, Point, Rect, Thread, VBT, VBTRep,
       Word;

(* -- public procedures -- *)
(* New and NewFromImage are implementation-specific and contained in
   PictureImpl.m3 *)

PROCEDURE Paint (         v        : VBT.Leaf;
                          src      : T;
                 READONLY clip                  := Rect.Full;
                 READONLY delta                 := Point.Origin;
                          freeProc : FreeProc   := NIL;
                          freeParam: REFANY     := NIL           )
  RAISES {Thread.Alerted} =
  CONST
    bsize = ADRSIZE(PaintExt.PictureRec);
    size  = bsize DIV ADRSIZE(Word.T);
  VAR
    p: PaintExt.PicturePtr;
    completion := PictureRep.MakeCompletion(src).init(
                    1, freeProc, freeParam);
  BEGIN    
    LOCK v DO
      IF v.remaining < bsize THEN
        IF v.st = NIL THEN RETURN END;
        VBTRep.NewBatch(v, size);
      END;

      DEC(v.remaining, bsize);
      WITH b = v.batch DO
        p := b.next;
        INC(b.next, bsize);
        p.ext.command := PaintPrivate.PaintCommand.ExtensionCom;
        p.ext.clip := clip;
        p.ext.szOfRec := size;
        p.ext.delta := delta;
        p.ext.subCommand := PaintExt.PictureCommand;
        p.picture := LOOPHOLE(src, ADDRESS); (* see the note PaintExt.i3 *)
        p.completion := LOOPHOLE(completion, ADDRESS);
        BatchUtil.SetPicture(b);
      END;
      VBTRep.ForceBatch(v);
    END;

    VBT.Sync(v, FALSE);

    IF freeProc = NIL THEN
      (* synchronous version *)
      TRY
        completion.waitUntilFree();
      FINALLY
        IF NOT completion.isFree() THEN completion.dec(); END;
        Completion.Dispose(completion);
      END;
    END;
  END Paint;

PROCEDURE Destroy (picture: T) =
  BEGIN
    picture.destroy();
  END Destroy;

PROCEDURE AttachData (picture: T;
                      dataPtr: Ctypes.char_star;
                      shmInfo: SharedMemInfo      := NIL)
  RAISES {TrestleFail} =
  BEGIN
    picture.attachData(dataPtr, shmInfo);
  END AttachData;

PROCEDURE DetachData (picture: T) RAISES {TrestleFail} =
  BEGIN
    picture.detachData();
  END DetachData;

PROCEDURE Image (picture: T): ImageStar =
  BEGIN
    RETURN picture.image;
  END Image;

(* -- types and methods -- *)

REVEAL
  T = Public BRANDED OBJECT
      OVERRIDES
        init          := TInit;
        initFromImage := TInitFromImage;
        attachData    := TAttachData;
        detachData    := TDetachData;
        destroy       := DestroyCrash;
      END;

PROCEDURE TInit (            t            : T;
                 <* UNUSED*> st           : VBT.ScreenType;
                 <* UNUSED*> width, height: CARDINAL        ): T =
  BEGIN
    RETURN t;
  END TInit;

PROCEDURE TInitFromImage (            t    : T;
                          <* UNUSED*> st   : VBT.ScreenType;
                                      image: ImageStar;
                          <* UNUSED*> sharedMemory := FALSE): T =
  BEGIN
    t.allocByCaller := TRUE;
    t.image := image;
    RETURN t;
  END TInitFromImage;

PROCEDURE TAttachData (            picture: T;
                                   dataPtr: Ctypes.char_star;
                       <* UNUSED*> shmInfo: SharedMemInfo      := NIL) =
  BEGIN
    picture.image.data := dataPtr;
  END TAttachData;

PROCEDURE TDetachData (picture: T) =
  BEGIN
    picture.image.data := NIL;
  END TDetachData;

EXCEPTION Fatal;

PROCEDURE DestroyCrash (<* UNUSED *> picture: T) =
  <* FATAL Fatal *>
  BEGIN
    RAISE Fatal;
  END DestroyCrash;

(* -- utilities -- *)

TYPE LockElt = UNTRACED REF Word.T;

PROCEDURE Freeze (picture: T): Lock =
  VAR res: Lock;
  BEGIN
    res.a := LOOPHOLE(ADR(picture), LockElt);
    IF picture # NIL THEN
      res.b := LOOPHOLE(picture.image, LockElt);
      IF picture.image # NIL THEN
        res.c := LOOPHOLE(picture.image.data, LockElt)
      END
    END;
    RETURN res;
  END Freeze;

PROCEDURE Thaw (<*UNUSED*> l: Lock) =
  BEGIN
  END Thaw;

TYPE WalkProc = PROCEDURE (completion: Completion.T);

PROCEDURE DecrementBatch (ba: Batch.T) =
  PROCEDURE Dec (comp: Completion.T) =
    BEGIN
      comp.dec();
    END Dec;
  BEGIN
    WalkBatch(ba, Dec);
  END DecrementBatch;

PROCEDURE IncrementBatch (ba: Batch.T) =
  PROCEDURE Inc (comp: Completion.T) =
    BEGIN
      comp.inc();
    END Inc;
  BEGIN
    WalkBatch(ba, Inc);
  END IncrementBatch;

PROCEDURE WalkBatch (ba: Batch.T; proc: WalkProc) =
  VAR cptr: PaintPrivate.CommandPtr := BatchUtil.Succ(ba, NIL);
  BEGIN
    WHILE cptr # NIL DO
      IF cptr.command = PaintPrivate.PaintCommand.ExtensionCom THEN
        WITH op = LOOPHOLE(cptr, PaintExt.PicturePtr) DO
          IF op.ext.subCommand = PaintExt.PictureCommand THEN
            (* see PaintExt.i3 for LOOPHOLE *)
            proc(LOOPHOLE(op.completion, Completion.T));
          END;
        END;
      END;
      cptr := BatchUtil.Succ(ba, cptr);
    END;
  END WalkBatch;

BEGIN
END Picture.
