
MODULE Copy EXPORTS Main;
IMPORT FakeOS, Params, Process, IO;

(* Copy "contents" from "source" into "destination". *)

BEGIN
  IF Params.Count # 3 THEN
    IO.Put ("Syntax: copy <source> <destination>\n");
    Process.Exit (2);
  END;

  WITH source = Params.Get(1), destination = Params.Get(2) DO
    TRY
      FakeOS.Copy (source, destination);
    EXCEPT
    | FakeOS.Error (msg) => IO.Put ("oops " & msg & "\n");
    END
  END;

END Copy.
