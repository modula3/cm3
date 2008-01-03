(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Mar  7 16:08:28 PST 1996 by mhb                      *)

INTERFACE ImageUtils;

IMPORT Rd, Thread, Wr;

EXCEPTION Error; 

PROCEDURE SetupGIF(filter: TEXT);

PROCEDURE SetupJPEG(filter: TEXT);

PROCEDURE SetupXBM(filter: TEXT);

PROCEDURE giftopbm (source: Rd.T; dest: Wr.T)
  RAISES {Error, Thread.Alerted};

PROCEDURE jpegtopbm (source: Rd.T; dest: Wr.T)
  RAISES {Error, Thread.Alerted};

PROCEDURE xbmtopbm (source: Rd.T; dest: Wr.T)
  RAISES {Error, Thread.Alerted};

END ImageUtils.


