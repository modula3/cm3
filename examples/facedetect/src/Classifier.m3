(* Copyright (C) 2017 Peter McKinna. All rights reserved. *)
(* See file COPYRIGHT-BSD for details. *)

MODULE Classifier;

IMPORT Rd,Lex,Thread,FloatMode;
IMPORT IO;

PROCEDURE ReadInfo(fileName : TEXT) : RefInt RAISES {ClassError} =
  VAR
    rd : Rd.T;
    info : RefInt;
    stages : INTEGER := 0;
  BEGIN
    rd := IO.OpenRead(fileName);
    IF rd = NIL THEN
      IO.Put("Info File does not exist\n");
      RAISE ClassError;
    END;

    TRY
      stages := Lex.Int(rd);
      info := NEW(RefInt,stages);

      FOR i := 0 TO stages - 1 DO
        info[i] := Lex.Int(rd);
        Lex.Skip(rd);
      END;
      Rd.Close(rd);
    EXCEPT
    (* if hit eof or anything its an error *)
    | Rd.Failure,Lex.Error,Thread.Alerted,FloatMode.Trap => RAISE ClassError;
    END;
    
    RETURN info;
  END ReadInfo;
  
PROCEDURE TotalNodes(info : RefInt) : INTEGER =
  VAR tot : INTEGER := 0;
  BEGIN
    FOR i := 0 TO NUMBER(info^) - 1 DO
      INC(tot,info[i]);
    END;
    RETURN tot;
  END TotalNodes;
  
PROCEDURE ReadClassifiers(infoFile,classFile : TEXT) : Class RAISES {ClassError} =
  VAR
    ndIndex : INTEGER := 0;
    rd : Rd.T;
    cl : Class;
  BEGIN

    cl.stages := ReadInfo(infoFile);
    cl.stageCnt := NUMBER(cl.stages^);
    cl.nodeCnt := TotalNodes(cl.stages);

    cl.nodes := NEW(RefNode,cl.nodeCnt);
    cl.stagesThresh := NEW(RefInt,cl.stageCnt);

    (******************************************
    * Read the filter parameters in class.txt
    *
    * Each stage of the cascaded filter has:
    * 18 parameter per filter x filter per stage
    * + 1 threshold per stage
    *
    * For example, 
    * the first stage has 9 filters,
    * the first stage is specified using
    * 18 * 9 + 1 = 163 parameters
    * They are line 1 to 163 of class.txt
    *
    * The 18 parameters for each filter are:
    * 1 to 4: coordinates of rectangle 1
    * 5: weight of rectangle 1
    * 6 to 9: coordinates of rectangle 2
    * 10: weight of rectangle 2
    * 11 to 14: coordinates of rectangle 3
    * 15: weight of rectangle 3
    * 16: threshold of the filter
    * 17: alpha 1 of the filter
    * 18: alpha 2 of the filter
    ******************************************)
    
    rd := IO.OpenRead(classFile);
    IF rd = NIL THEN
      IO.Put("Class File does not exist\n");
      RAISE ClassError;
    END;
    
    TRY
      (* loop over n of stages *)
      FOR i := 0 TO cl.stageCnt - 1 DO
        (* loop over n of trees *)
        FOR j := 0 TO cl.stages[i] - 1 DO
          (* loop over num of rectangular features *)
          FOR k := 0 TO NumFeatures - 1 DO
            WITH tr = cl.nodes[ndIndex].feature[k] DO
              tr.rect.x := Lex.Int(rd);
              tr.rect.y := Lex.Int(rd);
              tr.rect.width := Lex.Int(rd);
              tr.rect.height := Lex.Int(rd);
              tr.weight := Lex.Int(rd);
            END;
          END;
          cl.nodes[ndIndex].treeThresh := Lex.Int(rd);
          cl.nodes[ndIndex].alpha1 := Lex.Int(rd);
          cl.nodes[ndIndex].alpha2 := Lex.Int(rd);
          INC(ndIndex);

          IF j = cl.stages[i] - 1 THEN
            cl.stagesThresh[i] := Lex.Int(rd);        
          END;
        END
      END;
    
    EXCEPT
    | Rd.Failure,Lex.Error,Thread.Alerted,FloatMode.Trap => RAISE ClassError;
    END;
    
    RETURN cl;
    
  END ReadClassifiers;
  
BEGIN
END Classifier.