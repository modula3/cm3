(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* Last modified on Mon Apr 24 16:55:28 PDT 1995 by msm      *)
(*      modified on Wed Oct  6 09:21:22 PDT 1993 by sfreeman *)

INTERFACE PictureRep;

(* further detail about the representation of an Picture.T. *)

IMPORT Batch, Completion, Ctypes, Picture, VBT, Word;
FROM Picture IMPORT TrestleFail, ScreenTypeNotSupported;

REVEAL Picture.T <: Public;

TYPE
  Public =
    MUTEX OBJECT
      allocByCaller := FALSE;
      (* if TRUE, Picture belongs to the caller, so should not be freed by
         destroy *)
      image: Picture.ImageStar := NIL;
      next : Picture.T         := NIL; (* for free list *)
    METHODS
      init (st: VBT.ScreenType; width, height: CARDINAL): Picture.T
            RAISES {ScreenTypeNotSupported, TrestleFail};
      initFromImage (st          : VBT.ScreenType;
                     image       : Picture.ImageStar;
                     sharedMemory                      := FALSE): Picture.T
                     RAISES {ScreenTypeNotSupported, TrestleFail};

      attachData (dataPtr: Ctypes.char_star;
                  shmInfo: Picture.SharedMemInfo := NIL)
                  RAISES {TrestleFail};
      detachData () RAISES {TrestleFail};

      destroy ();
    END;

(* -- utilities -- *)

PROCEDURE MakeCompletion (picture: Picture.T): Completion.T;

(* completions should be got from here, so specific implementations can do
   extra things with the basic type.  Call the init() method on the
   result *)

TYPE Lock = RECORD a, b, c: UNTRACED REF Word.T END;

PROCEDURE Freeze (picture: Picture.T): Lock;
PROCEDURE Thaw (l: Lock);

(* To read the contents of an Picture, you must first call "Freeze",
   preventing the allocator from moving the internal data.  You must then
   call "Thaw", passing the result of the call to "Freeze" when you no
   longer need the pointers to be maintained correctly. *)

PROCEDURE IncrementBatch (ba: Batch.T);
PROCEDURE DecrementBatch (ba: Batch.T);

(* search the batch and increment or decrement the Completion in any
   PaintExt.ImageRec's. *)

END PictureRep.
