(* Copyright (C) 1989, 1993 Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* Last modified on Tue Jan 31 12:27:43 PST 1995 by msm      *)
(*      modified on Sat Oct  9 16:06:22 PDT 1993 by sfreeman *)

(* this VBT attaches itself to a stream from a JVideo source and displays
   the image stream when it is mapped *)

INTERFACE VideoVBT;

<* PRAGMA LL *>

IMPORT JVDecomp, JVSink, VBT;

TYPE
  T <: Public;
  Public =
    VBT.Leaf OBJECT
    METHODS
      <*  LL =< {v} *>
      init (sourceHost   : TEXT;
            quality      : JVSink.Quality;
            ncolours     : CARDINAL         := 0;
            width        : CARDINAL         := 320;
            height       : CARDINAL         := 240;
            synchronous                     := FALSE;
            fixedSize                       := FALSE;
            minFrameMSecs: CARDINAL         := 0      ): T;
      (* initial setup.  Describes the connection we want.  /ncolours/ is
         the number of cells requested for the colourmap; if 0, the colormap
         will be left unchanged, unless it was previously empty, in which
         case a suitable default vale will be supplied.  /width/ and
         /height/ are the requested size of the image, the JVideo card may
         round these values up.  If the image is larger than the VBT, the
         lower right sides will be clipped.  If /synchronous/ then the
         paint loop waits until the image buffer is free before starting to
         paint the next frame, otherwise it tries to overlap its activity
         some.  If /fixedSize/ then the VBT will attempt to remain the same
         size as its video images, otherwise it will attempt to fit the
         video to its current size.  /minFrameMSecs/ specifies the minimum
         number of milliseconds a video frame will take.  This allows the
         caller to slow the video stream down. *)

      (* procedures to set values *)
      <* LL < v *>
      setQuality       (quality: JVSink.Quality);
      setSize          (width, height: CARDINAL);
      setMinFrameMSecs (msecs: CARDINAL);
      setSynchronous   (synchronous: BOOLEAN);
      setFixedSize     (fixedSize : BOOLEAN);

      getDecomp (): JVDecomp.T;
      (* return the JVDecomp.T associated with the T.  May return NIL *)

      getSize (VAR width, height: CARDINAL);
      (* return the size of the video images *)

      setPaused (paused := FALSE);
      (* stop or start the VBT without breaking any connections *)

      (* JVConverter-like statistics *)
      startStats ();
      stopStats  ();
      getStats   (): JVDecomp.Statistics;
    END;

END VideoVBT.
