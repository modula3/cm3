(* Copyright (C) 1991, Digital Equipment Corporation                    *)
(* Copyright 1990 David Lemke and Network Computing Devices             *)
(* Copyright (c) 1989, Donald R. Woods and Sun Microsystems, Inc.       *)
(* All rights reserved.                                                 *)
(* See the file COPYRIGHT for a full description.                       *)

(* Last modified on Thu Sep 12 17:58:20 PDT 1991 by msm     *)

MODULE CardRank;

IMPORT Pixmap, PixmapFromXData;

FROM Card IMPORT Rank;

VAR 
  mu := NEW(MUTEX);
  inited := FALSE;
  pix: ARRAY Rank OF Pixmap.T;
  xip: ARRAY Rank OF Pixmap.T;

TYPE RankData = ARRAY Rank OF PixmapFromXData.T;

CONST
  Data = RankData{Ace, Deuce, Trey, Four, Five, Six, Seven,
    Eight, Nine, Ten, Jack, Queen, King};

PROCEDURE Pix(r: Rank): Pixmap.T =
  BEGIN
    LOCK mu DO
      IF NOT inited THEN Init() END;
      RETURN pix[r]
    END
  END Pix;

PROCEDURE Xip(r: Rank): Pixmap.T =
  BEGIN
    LOCK mu DO
      IF NOT inited THEN Init() END;
      RETURN xip[r]
    END
  END Xip;

PROCEDURE Init() =
  BEGIN
    FOR r := FIRST(Rank) TO LAST(Rank) DO
      pix[r] := PixmapFromXData.P(Data[r]);
      xip[r] := PixmapFromXData.Flip(Data[r])
    END;
    inited := TRUE
  END Init;

CONST Ace = PixmapFromXData.T{9, 14,
  "38 00 38 00 38 00 6c 00 6c 00 6c 00 " &
  "6c 00 c6 00 fe 00 fe 00 c6 00 83 01 " &
  "83 01 83 01 "};

CONST Deuce = PixmapFromXData.T{9, 14,
  "7c 00 fe 00 c7 01 83 01 80 01 c0 01 " &
  "e0 00 70 00 38 00 1c 00 0e 00 87 01 " &
  "ff 01 ff 01 "};

CONST Trey = PixmapFromXData.T{9, 14,
  "ff 01 ff 01 c3 01 e0 00 70 00 78 00 " &
  "fc 00 c8 01 80 01 80 01 82 01 c7 01 " &
  "fe 00 7c 00 "};

CONST Four = PixmapFromXData.T{9, 14,
  "e0 00 f0 00 f0 00 d8 00 d8 00 cc 00 " &
  "cc 00 c6 00 c6 00 ff 01 ff 01 c0 00 " &
  "e0 01 e0 01 "};

CONST Five = PixmapFromXData.T{9, 14,
  "ff 00 ff 00 03 00 03 00 7b 00 ff 00 " &
  "c7 01 82 01 80 01 80 01 82 01 c7 01 " &
  "fe 00 7c 00 "};

CONST Six = PixmapFromXData.T{9, 14,
  "7c 00 fe 00 c7 01 83 00 03 00 7b 00 " &
  "ff 00 c7 01 83 01 83 01 83 01 c7 01 " &
  "fe 00 7c 00 "};

CONST Seven = PixmapFromXData.T{9, 14,
  "ff 01 ff 01 83 01 c0 00 c0 00 60 00 " &
  "60 00 30 00 30 00 30 00 18 00 18 00 " &
  "18 00 18 00 "};

CONST Eight = PixmapFromXData.T{9, 14,
  "7c 00 fe 00 c7 01 83 01 c7 01 fe 00 " &
  "7c 00 fe 00 c7 01 83 01 83 01 c7 01 " &
  "fe 00 7c 00 "};

CONST Nine = PixmapFromXData.T{9, 14,
  "7c 00 fe 00 c7 01 83 01 83 01 83 01 " &
  "c7 01 fe 01 bc 01 80 01 82 01 c7 01 " &
  "fe 00 7c 00 "};

CONST Ten = PixmapFromXData.T{9, 14,
  "f3 00 fb 01 9b 01 9b 01 9b 01 9b 01 " &
  "9b 01 9b 01 9b 01 9b 01 9b 01 9b 01 " &
  "fb 01 f3 00 "};

CONST Jack = PixmapFromXData.T{9, 14,
  "e0 01 e0 01 c0 00 c0 00 c0 00 c0 00 " &
  "c0 00 c0 00 c0 00 c3 00 c3 00 e7 00 " &
  "7e 00 3c 00 "};

CONST Queen = PixmapFromXData.T{9, 14,
  "38 00 7c 00 ee 00 c6 00 c6 00 c6 00 " &
  "c6 00 c6 00 df 00 ff 00 f6 00 ee 00 " &
  "fc 01 b8 00 "};

CONST King = PixmapFromXData.T{9, 14,
  "ef 01 ef 01 e6 00 76 00 3e 00 1e 00 " &
  "1e 00 3e 00 36 00 76 00 66 00 e6 00 " &
  "ef 01 ef 01 "};

BEGIN END CardRank.
