(* Copyright (C) 1989, 1993 Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* Last modified on Fri Oct  8 09:06:02 PDT 1993 by sfreeman *)

(* further interesting details about a VideoVBT *)

INTERFACE VideoVBTRep;

<* PRAGMA LL *>

IMPORT Picture, JVFromDecomp, JVSink, Jvs, OSError, VBT, VideoVBT;

(* we associate a Buffer with an Picture.T to encourage reuse of storage.
   A factory and its related buffers are associated with a given screen.
   If you want another screen, use another factory *)

TYPE
  Buffer <: BufferPublic;
  BufferPublic =
    JVFromDecomp.T OBJECT
    METHODS
      <* LL < {self} *>
      picture (): Picture.T;
      (* return an Picture.T associated with the buffer.  May return NIL *)
    END;

(* a Factory produces our kind of Buffer *)
TYPE
  Factory <: PublicFactory;
  PublicFactory =
    JVFromDecomp.Factory OBJECT
    METHODS
      preInit (st: VBT.ScreenType; width, height: CARDINAL)
               RAISES {Picture.ScreenTypeNotSupported, OSError.E};
      (* set the screen type associated with the factory.  Raises an
         exception if the screen type is not supported *)
    END;

PROCEDURE SetPictureParams (         v         : VideoVBT.T;
                                     sourceHost: TEXT;
                                     quality   : JVSink.Quality;
                            READONLY dparams   : Jvs.DcmpParams;
                            READONLY cmap      : Jvs.ColormapInfo);
<* LL < v *>
(* change the details of the picture stream *)

PROCEDURE GetPictureParams (    v         : VideoVBT.T;
                            VAR sourceHost: TEXT;
                            VAR quality   : JVSink.Quality;
                            VAR dparams   : Jvs.DcmpParams;
                            VAR cmap      : Jvs.ColormapInfo);
<* LL < v *>
(* get the details of the picture *)

END VideoVBTRep.
