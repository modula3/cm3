(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Wed Aug 16 09:54:21 PDT 1995 by mhb                      *)

INTERFACE Images;

IMPORT Pixmap, Thread;

EXCEPTION Error; 

PROCEDURE FromJPEG (bits: TEXT): Pixmap.T
  RAISES {Error, Thread.Alerted};

PROCEDURE FromGIF (bits: TEXT): Pixmap.T
  RAISES {Error, Thread.Alerted};

PROCEDURE FromXBM (bits: TEXT): Pixmap.T
  RAISES {Error, Thread.Alerted};


PROCEDURE ToCache (url: TEXT; pm: Pixmap.T);
(* Stores "pm" into a cache under the key "url",
   replacing any pixmap already stored under "url". *)

PROCEDURE FromCache (url: TEXT; VAR pm: Pixmap.T): BOOLEAN;
(* Looks for "url" in the cache, and returns the assciated
  pixmap "pm" and a flag indicating if it found the "url". *)

END Images.


