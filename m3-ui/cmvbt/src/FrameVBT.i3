
(* Copyright 1997, Critical Mass, Inc. All Rights Reserved. *)

INTERFACE FrameVBT;

(* Interface "FrameVBT" creates a simple labeled frame
   around its child, similar to ShadowedVBT, except
   that the label covers the top-left corner of the
   frame. *)

IMPORT Font, Shadow, BorderedVBT, VBT;

TYPE
  T <: Public;
  Public = BorderedVBT.T OBJECT METHODS
    init (ch     : VBT.T;
          title  : TEXT;
          fnt    : Font.T   := Font.BuiltIn;
          shadow : Shadow.T := NIL          ): T;
  END;
  (* The call "v.init(ch, title, fnt, shadow)" initializes
     "v" as a frame named "title" with font "fnt", 
     around its child "ch". The colors in "shadow" 
     are used for the painting of the frame with 
     the style "Shadow.Style.Chiseled". *)

PROCEDURE New (ch: VBT.T;  title: TEXT;  fnt: Font.T := Font.BuiltIn;
                shadow: Shadow.T := NIL): T;
(* == NEW(T).init(ch, title, fnt, shadow) *)

END FrameVBT.
