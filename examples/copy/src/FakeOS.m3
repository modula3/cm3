
MODULE FakeOS;
IMPORT Rd, Wr, FileRd, FileWr;

(* As the implementation of "FakeOS" interface, this
   module supplies the body of the "Copy" procedure. The "Copy"
   procedure creates new reader and writer streams from the
   input and output files, reads the contents from the 
   input, and writes it to the output. 

   The procedures "OpenRead" and "OpenWrite" from "IO" are 
   used for reading and writing files, "Rd.GetText" 
   and "Wr.PutText" or input and output, and "Wr.Close" 
   and "Rd.Close" to close the I/O streams. 

|  Open a reader and a writer to the source and destination
|  Read all the contents of the reader 
|  Write the contents into the writer
|  Flush and close the reader and the writer

*)

PROCEDURE Copy(src, dest: TEXT) =
  VAR
    rd: Rd.T;
    wr: Wr.T;
  BEGIN
    rd := FileRd.Open (src);
    wr := FileWr.Open (dest);
    WITH contents = Rd.GetText (rd, LAST(INTEGER)) DO
      Wr.PutText (wr, contents);
    END;
    Rd.Close (rd);
    Wr.Close(wr);
  END Copy;

BEGIN
END FakeOS.

