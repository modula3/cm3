(* Copyright (C) 1991, Digital Equipment Corporation                    *)
(* Copyright 1990 David Lemke and Network Computing Devices             *)
(* Copyright (c) 1989, Donald R. Woods and Sun Microsystems, Inc.       *)
(* All rights reserved.                                                 *)
(* See the file COPYRIGHT for a full description.                       *)

(* Last modified on Thu Sep 12 23:13:49 PDT 1991 by msm     *)

MODULE CardSuit;

IMPORT Pixmap, PixmapFromXData;

FROM Card IMPORT Suit, Family;

VAR 
  mu := NEW(MUTEX);
  inited := FALSE;
  acepix, rankpix, pippix, rankxip, pipxip: ARRAY Suit OF Pixmap.T;

TYPE SuitData = ARRAY Suit OF PixmapFromXData.T;

CONST
  PipData = SuitData{Spade, Heart, Diamond, Club};
  RankData = SuitData{SmallSpade, SmallHeart, SmallDiamond, SmallClub};

PROCEDURE Init() =
  BEGIN
    FOR s := FIRST(Suit) TO LAST(Suit) DO
      VAR halftone := s = Family.Hearts OR s = Family.Diamonds; BEGIN
        rankpix[s] := PixmapFromXData.P(RankData[s], halftone);
        rankxip[s] := PixmapFromXData.Flip(RankData[s], halftone);
        pippix[s] := PixmapFromXData.P(PipData[s], halftone);
        pipxip[s] := PixmapFromXData.Flip(PipData[s], halftone)
      END;
      IF s = Family.Spades THEN
        acepix[s] := PixmapFromXData.P(BigSpade)
      ELSE
        acepix[s] := pippix[s]
      END
    END;
    inited := TRUE
  END Init;

PROCEDURE AcePix(s: Suit): Pixmap.T =
  BEGIN
    LOCK mu DO
      IF NOT inited THEN Init() END;
      RETURN acepix[s]
    END
  END AcePix;

PROCEDURE RankPix(s: Suit): Pixmap.T =
  BEGIN
    LOCK mu DO
      IF NOT inited THEN Init() END;
      RETURN rankpix[s]
    END
  END RankPix;

PROCEDURE RankXip(s: Suit): Pixmap.T =
  BEGIN
    LOCK mu DO
      IF NOT inited THEN Init() END;
      RETURN rankxip[s]
    END
  END RankXip;

PROCEDURE PipPix(s: Suit): Pixmap.T =
  BEGIN
    LOCK mu DO
      IF NOT inited THEN Init() END;
      RETURN pippix[s]
    END
  END PipPix;

PROCEDURE PipXip(s: Suit): Pixmap.T =
  BEGIN
    LOCK mu DO
      IF NOT inited THEN Init() END;
      RETURN pipxip[s]
    END
  END PipXip;

CONST Spade = PixmapFromXData.T{15, 19,
  "80 00 80 00 c0 01 c0 01 e0 03 f0 07 " &
  "f0 07 f8 0f fc 1f fe 3f fe 3f ff 7f " &
  "ff 7f ff 7f ff 7f be 3e 9c 1c c0 01 " &
  "e0 03 "};

CONST BigSpade = PixmapFromXData.T{39, 52,
  "00 00 08 00 00 00 00 08 00 00 00 00 " &
  "08 00 00 00 00 1c 00 00 00 00 1c 00 " &
  "00 00 00 1c 00 00 00 00 3e 00 00 00 " &
  "00 3e 00 00 00 00 7f 00 00 00 00 7f " &
  "00 00 00 80 ff 00 00 00 c0 ff 01 00 " &
  "00 c0 ff 01 00 00 e0 ff 03 00 00 f0 " &
  "ff 07 00 00 f0 ff 07 00 00 f8 ff 0f " &
  "00 00 fc ff 1f 00 00 fe ff 3f 00 00 " &
  "ff ff 7f 00 80 ff ff ff 00 c0 ff ff " &
  "ff 01 e0 ff ff ff 03 f0 ff ff ff 07 " &
  "f8 ff ff ff 0f f8 ff ff ff 0f fc ff " &
  "ff ff 1f fc ff ff ff 1f fe ff ff ff " &
  "3f fe ff ff ff 3f fe ff ff ff 3f ff " &
  "ff ff ff 7f ff ff ff ff 7f ff ff ff " &
  "ff 7f ff ff ff ff 7f ff ff ff ff 7f " &
  "fe ff ff ff 3f fe ff ff ff 3f fc ff " &
  "be ff 1f fc 7f 1c ff 1f f8 3f 1c fe " &
  "0f f0 1f 1c fc 07 c0 07 1c f0 01 00 " &
  "00 1c 00 00 00 00 3e 00 00 00 00 3e " &
  "00 00 00 00 7f 00 00 00 00 7f 00 00 " &
  "00 80 ff 00 00 00 c0 ff 01 00 00 e0 " &
  "ff 03 00 00 f0 ff 07 00 "};

CONST SmallSpade = PixmapFromXData.T{9, 12,
  "10 00 10 00 38 00 38 00 7c 00 fe 00 " &
  "ff 01 ff 01 ff 01 d6 00 10 00 38 00 "};

CONST Heart = PixmapFromXData.T{15, 17,
  "1c 1c 3e 3e 7f 7f 7f 7f ff 7f ff 7f " &
  "fe 3f fe 3f fc 1f f8 0f f0 07 f0 07 " &
  "e0 03 c0 01 c0 01 80 00 80 00 "};

CONST SmallHeart = PixmapFromXData.T{9, 11,
  "c6 00 ef 01 ff 01 ff 01 fe 00 fe 00 " &
  "7c 00 38 00 38 00 10 00 10 00 "};

CONST Club = PixmapFromXData.T{15, 16,
  "c0 81 e0 83 f0 87 f0 87 f0 87 e0 83 " &
  "dc 9d fe bf ff ff ff ff ff ff be 3e " &
  "9c 1c c0 81 c0 81 e0 83 "};

CONST SmallClub = PixmapFromXData.T{9, 11,
  "38 00 7c 00 7c 00 38 00 d6 00 ff 81 " &
  "ff 81 ff 81 d6 00 00 00 38 00 "};

CONST Diamond = PixmapFromXData.T{13, 19,
  "40 00 40 00 e0 00 e0 00 f0 01 f8 03 " &
  "f8 03 fc 07 fe 0f ff 1f fe 0f fc 07 " &
  "f8 03 f8 03 f0 01 e0 00 e0 00 40 00 " &
  "40 00 "};

CONST SmallDiamond = PixmapFromXData.T{7, 12,
  "08 08 1c 1c 3e 7f 3e 1c 1c 08 08 00 "};

BEGIN 
  PixWidth[Family.Spades] := Spade.width;
  PixWidth[Family.Hearts] := Heart.width;
  PixWidth[Family.Diamonds] := Diamond.width;
  PixWidth[Family.Clubs] := Club.width;
  PixHeight[Family.Spades] := Spade.height;
  PixHeight[Family.Hearts] := Heart.height;
  PixHeight[Family.Diamonds] := Diamond.height;
  PixHeight[Family.Clubs] := Club.height;
END CardSuit.
