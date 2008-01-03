INTERFACE SeekRdClass;
IMPORT FileRd;
TYPE
  T <: Public;
  Public = FileRd.T OBJECT
  METHODS
    (* init(h: File.T): FileRd.T
       If h is seekable, return an ordinary FileRd.T  *)

    discardPrevious();
    (* assert that there will be no seeking to positions before cur(rd) *)
    
    lineNo(): INTEGER;
    (* number of newLines before cur(rd), plus one *)
  END;
END SeekRdClass.
