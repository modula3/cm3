(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Mon Jan 16 09:28:31 PST 1995 by najork                   *)
(*       Created on Mon Jan 16 09:18:57 PST 1995 by najork                   *)


MODULE PictureImpl EXPORTS Picture, PictureRep;

IMPORT Completion, Ctypes, VBT;


PROCEDURE New (<*UNUSED*> st    : VBT.ScreenType; 
               <*UNUSED*> width : CARDINAL; 
               <*UNUSED*> height: CARDINAL): T
    RAISES {ScreenTypeNotSupported} =
  BEGIN
    RAISE ScreenTypeNotSupported;
  END New;


PROCEDURE Supported (<*UNUSED*> st       : VBT.ScreenType; 
                     <*UNUSED*> sharedMem: BOOLEAN): BOOLEAN =
  BEGIN
    RETURN FALSE;
  END Supported;


PROCEDURE MakeImage (<*UNUSED*> st            : VBT.ScreenType;
                     <*UNUSED*> width, height : Ctypes.int;
                     <*UNUSED*> xoffset       : Ctypes.int;
                     <*UNUSED*> bitmap_pad    : Ctypes.int;
                     <*UNUSED*> bytes_per_line: Ctypes.int): ImageStar
    RAISES {ScreenTypeNotSupported} =
  BEGIN
    RAISE ScreenTypeNotSupported;
  END MakeImage;


PROCEDURE FromImage (<*UNUSED*> st          : VBT.ScreenType;
                     <*UNUSED*> image       : ImageStar;
                     <*UNUSED*> sharedMemory: BOOLEAN): T
    RAISES {ScreenTypeNotSupported} =
  BEGIN
    RAISE ScreenTypeNotSupported;
  END FromImage;


PROCEDURE MakeCompletion (<*UNUSED*> picture: T): Completion.T =
  BEGIN
    RETURN Completion.New();
  END MakeCompletion;


BEGIN
END PictureImpl.
