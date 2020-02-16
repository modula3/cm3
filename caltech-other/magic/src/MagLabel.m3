(* $Id$ *)

MODULE MagLabel;
IMPORT MagRect, TextReader;
IMPORT Scan, Text;
IMPORT FloatMode, Lex;
IMPORT Word;

PROCEDURE ParseFromReader(reader : TextReader.T; 
                          (* OUT *) VAR res : T) RAISES { ParseError } =
  BEGIN
    TRY
      res.layer := reader.nextE(" ", skipNulls := TRUE);
      MagRect.ParseFromReader(reader, res.rect);
      res.direction := Scan.Int(reader.nextE(" ", skipNulls := TRUE));
      res.name := reader.nextE(" ", skipNulls := TRUE)
    EXCEPT
      MagRect.ParseError, TextReader.NoMore, FloatMode.Trap, Lex.Error 
        => RAISE ParseError
    END
  END ParseFromReader;

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN = 
  BEGIN 

    (* the layer equality should really allow that m1 = blue = metal1, f.ex.*)
    RETURN 
      a.rect = b.rect AND 
      Text.Equal(a.layer, b.layer) AND 
      Text.Equal(a.name, b.name) 
    (* AND  a.direction = b.direction *) (* we don't care about dir *)
  END Equal;

PROCEDURE Hash(READONLY a : T) : Word.T =
  BEGIN
    RETURN Word.Plus(MagRect.Hash(a.rect), 
                     Word.Plus(Text.Hash(a.layer), 
                               Word.Plus(a.direction,
                                         Text.Hash(a.name))))
  END Hash;

BEGIN END MagLabel.
