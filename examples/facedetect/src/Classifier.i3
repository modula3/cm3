(* Copyright (C) 2017 Peter McKinna. All rights reserved. *)
(* See file COPYRIGHT-BSD for details. *)

INTERFACE Classifier;

IMPORT Rectangles AS R;

CONST
  NumFeatures = 3;

EXCEPTION ClassError;
  
TYPE

  RefInt = REF ARRAY OF INTEGER;

  Feature = RECORD
    rect : R.Rect;
    weight : INTEGER;
  END;
  
  FeatureArr = ARRAY [0..NumFeatures-1] OF Feature;
  Node = RECORD
    feature : FeatureArr;
    alpha1,alpha2,treeThresh : INTEGER;
  END;
  (* not sure node is a good name *)
  RefNode = REF ARRAY OF Node;
  Class = RECORD
    stageCnt,nodeCnt : INTEGER;
    nodes : RefNode;
    stages,stagesThresh : RefInt;
  END;
  
PROCEDURE ReadClassifiers(infoFile,classFile : TEXT) : Class RAISES {ClassError};

END Classifier.