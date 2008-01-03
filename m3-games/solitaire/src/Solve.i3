(* Copyright (C) 1992, Xerox Corporation.                      *)
(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the files COPYRIGHT and COPYRIGHT.extras for details.   *)
(*                                                             *)
(* Last Modified On Tue Nov  1 10:13:13 PST 1994 By kalsow     *)
(* Created on Sat Feb 22 00:20:14 PST 1992 By goldberg@parc.xerox.com *)

INTERFACE Solve;

CONST noCard = CardType{suit := FIRST(Suit), val := 0};

TYPE
  WhyStop = {NoSolution, Aborted, GiveUp, (* give up in this branch *)
             Exhausted,         (* give up totally *)
             Solution};

TYPE
  Suit = {Spade, Heart, Diamond, Club};

  CardType = RECORD
               suit            := FIRST(Suit);
               val : [0 .. 13] := 0; (* 0 means no card *)
             END;

  Group =
    {Foundation, Tableau, Talon}; (* card layout divided into 3 groups *)

  CardList = REF RECORD
    card: CardType;
    nxt : CardList   := NIL;
  END;

  Layout =                      (* an arrangement of cards *)
    RECORD
      fnd := Foundation{
               CardType{VAL(0, Suit), 0}, CardType{VAL(1, Suit), 0},
               CardType{VAL(2, Suit), 0}, CardType{VAL(3, Suit), 0}};
      tal: Talon;
      tab: Tableau;
    END;
  Tableau = ARRAY [1 .. 10] OF CardList;
  Foundation = ARRAY [1 .. 4] OF CardType;
  Talon = ARRAY [1 .. 4] OF CardType;

  Location = RECORD             (* a location within a layout *)
               grp  : Group;
               where: [1 .. 10];  (* where in grp *)
             END;

  TreeRec = RECORD              (* tree of all possible layouts *)
              layout  : Layout;
              level   : CARDINAL;
              children: REF ARRAY OF Tree := NIL;
            END;
  Tree = REF TreeRec;

  Callback = PROCEDURE (cnt: CARDINAL);


(*
 * depth, breadth, total are measured in moves generated.
 * breadth is how far to search bread-first, depth is far to
 * search depth from each leaf of breadth search, total is limit
 * on entire search.  IF callback # NIL
 * then it will be called periodically with status.
 *)
PROCEDURE NextMove (    layout  : Layout;
                    VAR why     : WhyStop;
                        depth   : CARDINAL  := 2000;
                        breadth : CARDINAL  := 500;
                        total   : CARDINAL  := 100000;
                        verbose             := FALSE;
                        callback: Callback  := NIL     ): TEXT;
END Solve.
