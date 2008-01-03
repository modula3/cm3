UNSAFE MODULE Test EXPORTS Main;

IMPORT BinIO, IO, Wr, Rd, TextWr, TextRd, Fmt ;

VAR
  test_var : BinIO.Byte := 16_78 ;
  new_var  : BinIO.Byte := 0 ;

(* Note, under Windows 95/NT you cannot open the same file for reading
   and writing
*)
  wr       := IO.OpenWrite("tmp_file") ;
  rd       : Rd.T ;

(*
  wr := TextWr.New() ;
  rd : TextRd.T ;
*)

BEGIN
  
  BinIO.PutByte(test_var, wr) ;
  BinIO.PutByte(test_var, wr, BinIO.Endian.Little) ;
  BinIO.PutByte(test_var, wr, BinIO.Endian.Big) ;

  Wr.Close(wr) ;

  (* rd := TextRd.New(TextWr.ToText(wr)) ; *)
  rd := IO.OpenRead("tmp_file") ;

  IO.Put(Wr.EOL) ;
  new_var := BinIO.GetByte(rd) ;
  IO.PutInt(new_var) ;
  IO.Put(Wr.EOL) ;

  new_var := BinIO.GetByte(rd, BinIO.Endian.Little) ;
  IO.PutInt(new_var) ;
  IO.Put(Wr.EOL) ;

  IO.Put(Fmt.Bool(test_var = new_var)) ;
  IO.Put(Wr.EOL) ;


  new_var := BinIO.GetByte(rd, BinIO.Endian.Big) ;
  IO.PutInt(new_var) ;
  IO.Put(Wr.EOL) ;

  Rd.Close(rd) ;
END Test.
