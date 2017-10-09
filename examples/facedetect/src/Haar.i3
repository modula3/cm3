(* Copyright (C) 2017 Peter McKinna. All rights reserved. *)
(* See file COPYRIGHT-BSD for details. *)

INTERFACE Haar;

(* This is an implementation of the Viola-Jones object detection system based on
   a system of weak classifiers. In this case the classifier is trained to detect
   faces. See their paper for details of the theory.
*)

IMPORT Image,RefSeq;

TYPE

  T <: Public;
  Public = OBJECT
  METHODS
    init(infoFile,classFile : TEXT; minSize,maxSize : Size := Size{0,0}) : T;
    (* detect all the objects *)
    detectObjects(READONLY image : Image.Image; 
                  scaleFactor : REAL; 
                  minNeighbors : INTEGER) : RefSeq.T;
  END;
  
  Size = RECORD
    width,height : INTEGER;
  END;

END Haar.
