
MODULE CallingC EXPORTS Main;
IMPORT IO, Lib;

BEGIN
  WITH pwd = Lib.GetCWD() DO
    IO.Put (pwd & "\n");
  END
END CallingC.
    
