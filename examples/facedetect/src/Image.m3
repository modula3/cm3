(* Copyright (C) 2017 Peter McKinna. All rights reserved. *)
(* See file COPYRIGHT-BSD for details. *)

MODULE Image;

IMPORT Rd,Wr,IO,Fmt,Lex,Thread,FloatMode;
  
PROCEDURE CreateImage(width,height : INTEGER) : Image =
  VAR image : Image;
  BEGIN
    image.width := width;
    image.height := height;
    image.flag := 1;  
    image.data := NEW(RefArr, height, width);
    RETURN image;
  END CreateImage;

PROCEDURE SetImage(width,height : INTEGER; VAR image : Image) =
  BEGIN
    image.width := width;
    image.height := height;  
  END SetImage;

PROCEDURE ReadPgm(fileName : TEXT; VAR image : Image) RAISES {Error} =
  VAR
    rd : Rd.T;
    ch : CHAR;
    str : TEXT;
    type : INTEGER;
  BEGIN
    rd := IO.OpenRead(fileName);
    IF rd = NIL THEN
      RAISE Error("File does not exist");
    END;
    
    TRY
      ch := Rd.GetChar(rd);
      IF ch # 'P' THEN
        RAISE Error("Illegal image type");
      END;
      ch := Rd.GetChar(rd);
      type := ORD(ch) - 48;
      IF type # 5 THEN
        RAISE Error("Illegal image type");
      END;
      
      Lex.Skip(rd);
      ch := Rd.GetChar(rd);

      (* skip comments *)
      IF ch = '#' THEN
        str := Rd.GetLine(rd);
      ELSE
        Rd.UnGetChar(rd);
      END;

      image.width := Lex.Int(rd);
      image.height := Lex.Int(rd);
      Lex.Skip(rd);
      image.maxgrey := Lex.Int(rd);
      Lex.Skip(rd);

      (* debug *)
      IO.Put("width " & Fmt.Int(image.width) & " height " & Fmt.Int(image.height) & " grey " & Fmt.Int(image.maxgrey) & "\n");

      image.flag := 1;
      image.data := NEW(RefArr, image.height, image.width);

      FOR i := 0 TO image.height - 1 DO
        FOR j := 0 TO image.width - 1 DO
          image.data[i,j] := ORD(Rd.GetChar(rd));
        END;
      END;     
      Rd.Close(rd);
    EXCEPT
    | Rd.EndOfFile,Rd.Failure => RAISE Error("Rd Failure");
    | Thread.Alerted => RAISE Error("Thread Alerted");
    | FloatMode.Trap => RAISE Error("Float trap");
    | Lex.Error => RAISE Error("Lex error");
    END;
  END ReadPgm;
  
PROCEDURE WritePgm(fileName : TEXT; READONLY image : Image) RAISES {Error} =
  VAR
    wr : Wr.T;
    p5 := "P5";
  BEGIN
    IF image.flag = 0 THEN
      RAISE Error("Protocol error");
    END;
    wr := IO.OpenWrite(fileName);
    IF wr = NIL THEN
      RAISE Error("Output file can not be opened (" & fileName & ")");
    END;
    
    TRY
      Wr.PutText(wr,p5 & "\n");
      Wr.PutText(wr,Fmt.Int(image.width) & " ");
      Wr.PutText(wr,Fmt.Int(image.height) & "\n");
      Wr.PutText(wr,Fmt.Int(image.maxgrey) & "\n");
      FOR i := 0 TO image.height - 1 DO
        FOR j := 0 TO image.width - 1 DO
          Wr.PutChar(wr,VAL(image.data[i,j],CHAR));
        END;
      END;      
      Wr.Close(wr);
    EXCEPT
    | Wr.Failure, Thread.Alerted => RAISE Error("Write failure");
    END;
  END WritePgm;

BEGIN
END Image.
