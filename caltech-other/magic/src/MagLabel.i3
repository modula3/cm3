(* $Id$ *)
INTERFACE MagLabel;

IMPORT MagRect, TextReader;
IMPORT Word;

TYPE
  Position = [ 0..8 ];

  T = RECORD
    rect : MagRect.T;
    layer : TEXT;
    direction : Position;
    name : TEXT;
  END;

CONST
  Brand = "MagLabel";

EXCEPTION
  ParseError;

PROCEDURE ParseFromReader(reader : TextReader.T; 
                          (* OUT *) VAR res : T) RAISES { ParseError };

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN;
PROCEDURE Hash(READONLY a : T) : Word.T;
END MagLabel.
