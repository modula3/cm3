(* Copyright (C) 2017 Peter McKinna. All rights reserved. *)
(* See file COPYRIGHT-BSD for details. *)

MODULE Main;

IMPORT Image,Rectangles,Haar,RefSeq;
IMPORT Params,IO,Fmt;

CONST
  InfoFile = "../src/info.txt";
  ClassFile = "../src/class.txt";
  (* detection parameters *)
  MinSize = Haar.Size{20,20};
  MaxSize = Haar.Size{0,0};
  ScaleFactor = 1.2E0;
  MinNeighbours = 1;  
  usage = "Usage: haar imagefile.pgm\n";
    
VAR
  h : Haar.T;
  img : Image.Image;
  file : TEXT;
  
PROCEDURE ProcImage(img : Image.Image) =  
  VAR
    solRects : RefSeq.T;
    rect : Rectangles.RefRect;
  BEGIN
    solRects := h.detectObjects(img,ScaleFactor,MinNeighbours);
    IO.Put("candidates " & Fmt.Int(solRects.size()) & "\n");

    FOR i := 0 TO solRects.size() - 1 DO
      rect := solRects.get(i);
      Rectangles.DrawRectangle(img, rect^);
    END;
  
    TRY
      Image.WritePgm("Out.pgm",img);
    EXCEPT
      Image.Error(x) => IO.Put(x & "\n");
    END;
  END ProcImage;
  
BEGIN

  IF Params.Count < 2 THEN
    IO.Put(usage);
  ELSE
    h := NEW(Haar.T).init(InfoFile,ClassFile,MinSize,MaxSize);
    IF h = NIL THEN
      <*ASSERT FALSE*>
    END;
    
    file := Params.Get(1);
    TRY
      Image.ReadPgm(file,img);
      ProcImage(img);
    EXCEPT
      Image.Error(x) => 
        IO.Put(x & "\n");
        IO.Put(usage);
    END;
  END;
  
END Main.
