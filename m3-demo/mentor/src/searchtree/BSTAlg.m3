(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Wed Jun 15 13:12:34 PDT 1994 by heydon                   *)
(*      modified on Tue May  3 13:50:09 PDT 1994 by najork                   *)

MODULE BSTAlg;

IMPORT FormsVBT, Random, Text, VBT;

<* FATAL FormsVBT.Error, FormsVBT.Unimplemented *>

VAR
  lastIndex: INTEGER := 0;		 (* index of next new Node *)

PROCEDURE GetPanelData(panel: VBT.T): PanelData =
  VAR pd := NEW (PanelData); BEGIN
    LOCK VBT.mu DO
      pd.nodeCnt := FormsVBT.GetInteger(panel, "nodeCnt");
      pd.inputType := FormsVBT.GetChoice(panel, "inputtype");
      pd.deleteType := FormsVBT.GetChoice(panel, "deletetype");
      IF FormsVBT.GetBoolean(panel, "fixedRand")
        THEN pd.rand := NEW(Random.Default).init(fixed := TRUE);
        ELSE pd.rand := NEW(Random.Default).init();
      END;
    END;
    (* chop off initial "del_" from "pd.deleteType" *)
    pd.deleteType := Text.Sub(pd.deleteType, 4);
    RETURN pd
  END GetPanelData;

PROCEDURE NewKeys(data: PanelData; input := TRUE): Keys =
  VAR key := NEW(Keys, data.nodeCnt); kind: TEXT; BEGIN
    IF input THEN kind := data.inputType ELSE kind := data.deleteType END;
    IF Text.Equal(kind, "rand") THEN
      (* construct a random permutation of [1..cnt] *)
      FOR i := 0 TO LAST(key^) DO key[i] := i + 1 END;
      VAR slot: CARDINAL; temp: Key; BEGIN
        WITH last = LAST(key^) DO
          FOR i := 0 TO last - 1 DO
            slot := data.rand.integer (i, last);
            temp := key[i]; key[i] := key[slot]; key[slot] := temp
          END
        END
      END
    ELSIF Text.Equal(kind, "inc") THEN
      FOR i := 0 TO LAST(key^) DO key[i] := i + 1 END
    ELSE
      FOR i := 0 TO LAST(key^) DO key[i] := data.nodeCnt - i END
    END;
    RETURN key
  END NewKeys;

PROCEDURE NewIndex(): INTEGER =
  BEGIN
    INC(lastIndex);
    RETURN lastIndex
  END NewIndex;

PROCEDURE GetChild(node: Node; side: Side): Node =
  BEGIN
    CASE side OF
    | Side.Left => RETURN node.left
    | Side.Right => RETURN node.right
    END
  END GetChild;

PROCEDURE SetChild(node: Node; side: Side; val: Node) =
  BEGIN
    CASE side OF
    | Side.Left => node.left := val
    | Side.Right => node.right := val
    END
  END SetChild;

PROCEDURE Rotate(t: Tree; x: Node; side: Side) =
  VAR
    other := OtherSide[side];
    y := GetChild(x, other);
    y_side := GetChild(y, side);
  BEGIN
    SetChild(x, other, y_side);
    IF y_side # NIL THEN y_side.parent := x END;
    y.parent := x.parent;
    IF x.parent = NIL THEN
      t.root := y
    ELSE
      IF x.parent.left = x THEN
        x.parent.left := y
      ELSE
        x.parent.right := y
      END
    END;
    SetChild(y, side, x);
    x.parent := y
  END Rotate;

BEGIN
END BSTAlg.
