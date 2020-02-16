(* $Id$ *)

MODULE MagSlasher;
IMPORT MagCell, MagCellExtendable, MagSubCell, TextReader;
IMPORT TextList, TextWr, Wr, Thread;

<* FATAL Thread.Alerted, Wr.Failure *>

REVEAL
  T = Public BRANDED Brand OBJECT
    c : MagCell.Labelled;
  METHODS
    gotSub(nam : TEXT; VAR sub : MagCell.Labelled) : BOOLEAN := GotSub;
  OVERRIDES
    init := Init;
    slash := Slash;
  END;

PROCEDURE GotSub(t : T; nam : TEXT; VAR sub : MagCell.Labelled) : BOOLEAN = 
  VAR
    sss : MagSubCell.T;
    haveIt := t.c.getSubCell(nam, sss);
  BEGIN
    IF NOT haveIt THEN 
      RETURN FALSE 
    END;
    
    sub := sss.c;
    RETURN TRUE
  END GotSub;
  
PROCEDURE Init(t : T; c : MagCell.Labelled) : T =
  BEGIN t.c := c; RETURN t END Init;

PROCEDURE Slash(t : T; label : TEXT; VAR slashDotted : TEXT) : BOOLEAN =
  VAR
    reader := NEW(TextReader.T).init(label);
    arcs := reader.shatter(".","");
    sub : MagCell.Labelled;
    soFar : TEXT := "";
  BEGIN
    WHILE arcs # NIL DO
      soFar := soFar & arcs.head;

      IF t.gotSub(soFar, sub) THEN
        VAR
          subSlashed : TEXT;
          subLabel := ReAssembleWithDots(arcs.tail);
          subSlasher := NEW(T).init(sub);
          found := subSlasher.slash(subLabel, subSlashed);
        BEGIN
          IF found THEN
            slashDotted := soFar & "/" & subSlashed;
            RETURN TRUE
          ELSE
            RETURN FALSE
          END
        END
      END;

      soFar := soFar & "."; 
      arcs := arcs.tail
    END;
    (* no subcell matches---now look for a label! *)
    
    VAR
      labels := t.c.getLabels(label);
    BEGIN
      IF labels # NIL THEN
        slashDotted := label; RETURN TRUE
      END
    END;
    RETURN FALSE
  END Slash;


PROCEDURE ReAssembleWithDots(t : TextList.T) : TEXT =
  VAR 
    wr := NEW(TextWr.T).init();
  BEGIN
    WHILE t # NIL DO
      Wr.PutText(wr, t.head);
      IF t.tail # NIL THEN Wr.PutChar(wr, '.') END;
      t := t.tail
    END;
    RETURN TextWr.ToText(wr)
  END ReAssembleWithDots;

BEGIN END MagSlasher.
