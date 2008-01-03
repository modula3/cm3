MODULE Hello EXPORTS Main;

IMPORT Trestle, TextVBT;

VAR
  (* construct a VBT to be installed as the top level window *)
  main := TextVBT.New("Hello World");

BEGIN
  (* Install it as a top level window *)
  Trestle.Install(main);

  (* Wait until the installed VBT is deleted *)
  Trestle.AwaitDelete(main)
END Hello.
