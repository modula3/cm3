(* Copyright (C) 2017 Peter McKinna. All rights reserved. *)
(* See file COPYRIGHT-BSD for details. *)

INTERFACE Image;

TYPE

  RefArr = REF ARRAY OF ARRAY OF INTEGER;
  
  Image = RECORD
    width,height,maxgrey : INTEGER;
    data : RefArr;
    flag : INTEGER;
  END;

EXCEPTION Error(TEXT);

PROCEDURE CreateImage(width,height : INTEGER) : Image;
PROCEDURE SetImage(width,height : INTEGER; VAR image : Image);
PROCEDURE ReadPgm(fileName : TEXT; VAR image : Image) RAISES {Error};
PROCEDURE WritePgm(fileName : TEXT; READONLY image : Image) RAISES {Error};

END Image.
