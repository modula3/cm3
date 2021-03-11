(* $Id$ *)

MODULE LabelFinder;

IMPORT MagCell, MagCellExtendable, TextSet, MagLabelList AS LabelList, 
       MagCellInstance, MagTransform;
IMPORT Debug;
IMPORT NodeNameSplitter;
IMPORT Text;
FROM TextUtils IMPORT ReplaceChar;

PROCEDURE TransformLabelList(l : LabelList.T; t : MagTransform.T) : LabelList.T=
  VAR
    res : LabelList.T := NIL;
  BEGIN
    WHILE l # NIL DO
      res := LabelList.Cons(l.head, res);
      res.head.rect := MagTransform.Rect(res.head.rect, t);
      l := l.tail
    END;
    RETURN res
  END TransformLabelList;

PROCEDURE FindTextSetLabels(layout : MagCell.T;
                            aliasSet : TextSet.T;
                            ignoreEnclosingQuotes : BOOLEAN) : LabelList.T =
  VAR
    instances := layout.flatten();
    myLabels : LabelList.T := NIL;
    setIter := aliasSet.iterate();
    alias : TEXT;
  BEGIN
    WHILE setIter.next(alias) DO
      IF ignoreEnclosingQuotes AND Text.GetChar(alias,0) = '"' AND Text.GetChar(alias,Text.Length(alias) - 1) = '"' THEN
        alias := Text.Sub(alias,1,Text.Length(alias)-2)
      END;
      VAR
        nameIter := NEW(NodeNameSplitter.T).init(alias);
        cellName, nodeName : TEXT;
        cellInst : MagCellInstance.T;
      BEGIN
        Debug.Out("Looking for " & alias, 100);
        LOOP
          IF NOT nameIter.next(cellName,nodeName) THEN EXIT END;
          
          cellName := ReplaceChar(ReplaceChar(cellName, '(', '['), ')', ']');
          
          Debug.Out("Trying (" & cellName & "," & nodeName & ")", 100);
          
          IF instances.get(cellName, cellInst) THEN
            Debug.Out("Checking cell \"" & cellName & "\"", 100);
            VAR
              labels := NARROW(cellInst.getDef(), 
                               MagCell.Labelled).getLabels(nodeName);
            BEGIN
              labels := TransformLabelList(labels, cellInst.getTransform());
              IF labels # NIL THEN
                Debug.Out("Found labels for node \"" & nodeName & 
                  "\" in cell \"" & cellName & "\"", 100)
              END;
              
              myLabels := LabelList.Append(labels, myLabels);
              EXIT (* we can only have one successful node lookup *)
            END
            
          END
        END (* LOOP *)
      END
    END; (* WHILE setIter *)
    RETURN myLabels
  END FindTextSetLabels;

BEGIN END LabelFinder.
