(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Aug 18 14:19:10 PDT 1994 by heydon                   *)
(*      modified on Wed Jun 15 11:28:07 PDT 1994 by shillner                 *)

MODULE AlgSimple;

IMPORT UnionFindAlgClass, UnionFindIE;
IMPORT UFInput, TreeNode;
IMPORT Algorithm, ZeusPanel;
IMPORT VBT;
IMPORT Thread, Fmt;

TYPE
  T = UnionFindAlgClass.T BRANDED OBJECT OVERRIDES
    run := Run
  END;
  Nodes = REF ARRAY OF TreeNode.T;

PROCEDURE Run(alg: T) RAISES {Thread.Alerted} =
  VAR ufin: UFInput.T; nodes: Nodes; cmd: UFInput.Cmd; BEGIN
    TRY
      ufin := UFInput.New(alg.data);
      IF ufin # NIL THEN
        nodes := Setup(alg, ufin);
    	WHILE ufin.next(cmd) DO
    	  TYPECASE cmd OF <* NOWARN *>
    	    UFInput.FindCmd (find) =>    EVAL Find(alg, nodes, find)
    	  | UFInput.UnionCmd (union) =>  EVAL Union(alg, nodes, union)
    	  END
    	END
      END
    EXCEPT
      UFInput.Error (txt) =>
        LOCK VBT.mu DO
          ZeusPanel.ReportError("Bad input: " & txt & "\n")
        END
    END
  END Run;

PROCEDURE Setup(alg: T; ufin: UFInput.T): Nodes
    RAISES {UFInput.Error, Thread.Alerted} =
  VAR numSets := 0; cmd: UFInput.Cmd; res: Nodes; BEGIN
    UnionFindIE.Setup(alg);
    WHILE ufin.next(cmd) DO
      TYPECASE cmd OF <* NOWARN *>
        UFInput.NewSetCmd (newSet) =>
          UnionFindIE.NewSet(alg, newSet.arg1); INC(numSets)
      | UFInput.FinishedSetsCmd =>
          UnionFindIE.FinishedSets(alg, numSets, usesRanks := FALSE); EXIT
      END
    END;
    res := NEW(Nodes, numSets);
    FOR i := 0 TO numSets - 1 DO
      res[i] := NEW(TreeNode.T, id := i)
    END;
    RETURN res
  END Setup;

PROCEDURE Find(alg: T; nodes: Nodes; cmd: UFInput.FindCmd): TreeNode.T
    RAISES {UFInput.Error, Thread.Alerted} =
  VAR root: TreeNode.T; BEGIN
    IF cmd.arg1 > LAST(nodes^) THEN
      RAISE UFInput.Error("Find: set " &Fmt.Int(cmd.arg1)& " does not exist")
    END;
    WITH node = nodes[cmd.arg1] DO
      UnionFindIE.StartFind(alg, node.id);
      root := DoFind(alg, node);
    END;
    UnionFindIE.EndFind(alg, root.id);
    RETURN root
  END Find;

PROCEDURE Union(alg: T; nodes: Nodes; cmd: UFInput.UnionCmd): TreeNode.T
    RAISES {UFInput.Error, Thread.Alerted} =
  BEGIN
    IF cmd.arg1 > LAST(nodes^) THEN
      RAISE UFInput.Error("Union: set " &Fmt.Int(cmd.arg1)& " does not exist")
    ELSIF cmd.arg2 > LAST(nodes^) THEN
      RAISE UFInput.Error("Union: set " &Fmt.Int(cmd.arg2)& " does not exist")
    END;
    VAR node1 := nodes[cmd.arg1]; node2 := nodes[cmd.arg2]; BEGIN
      UnionFindIE.StartUnion(alg, node1.id, node2.id, cmd.bothRoots);
      IF NOT cmd.bothRoots THEN
      	node1 := DoFind(alg, node1);
      	UnionFindIE.FoundFirst(alg, node1.id);
      	node2 := DoFind(alg, node2)
      END;
      IF node1 # node2 THEN
        node2.up := node1;
        node1.rank := node1.rank + 1;
        UnionFindIE.Unite(alg, node2.id, node1.id, pRank := -1) 
      END;
      UnionFindIE.EndUnion(alg);
      RETURN node2
    END
  END Union;

PROCEDURE DoFind(alg: T; node: TreeNode.T): TreeNode.T
    RAISES {Thread.Alerted} =
  VAR root: TreeNode.T;
  BEGIN
    UnionFindIE.StartDoFind(alg, node.id);
    root := DoFind2(alg, node);
    UnionFindIE.EndDoFind(alg, node.id);
    RETURN root
  END DoFind;

PROCEDURE DoFind2(alg: T; node: TreeNode.T): TreeNode.T
    RAISES {Thread.Alerted} =
  BEGIN
    IF node.up # NIL THEN
      UnionFindIE.StepUp(alg, node.id, node.up.id);
      RETURN DoFind2(alg, node.up)
    ELSE
      UnionFindIE.Found(alg, node.id);
      RETURN node
    END
  END DoFind2;

PROCEDURE New(): Algorithm.T =
  BEGIN
    RETURN NEW(T, data := ZeusPanel.NewForm("UnionFind.fv")).init()
  END New;

BEGIN
  ZeusPanel.RegisterAlg(New, "Tree, No Heuristics", "UnionFind")
END AlgSimple.
