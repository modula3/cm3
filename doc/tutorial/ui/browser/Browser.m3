MODULE Browser EXPORTS Main;

IMPORT Trestle, FileBrowserVBT, Shadow, HVBar, HVSplit, Axis, Font;
FROM Colors IMPORT royal, white, lgrey, dgrey;

CONST
  margin      = 1.0;             (* points *)
  shadow_size = 10.0;            (* points *)
  times14     = "-*-times-*-r-*-*-14-*-*-*-*-*-*-*";

VAR
  font    := Font.FromName(ARRAY OF TEXT{times14});
  sh      := Shadow.New(shadow_size, royal, white, lgrey, dgrey);
  helper  := NEW(FileBrowserVBT.Helper).init(margin, margin, font, sh);
  dirmenu := NEW(FileBrowserVBT.DirMenu).init(font, sh);
  v       := NEW(FileBrowserVBT.T).init();
  main    := HVSplit.Cons(Axis.T.Ver, helper, HVBar.New(), v);

BEGIN
  FileBrowserVBT.Set(v, "./");   (* Set the initial display path *)
  FileBrowserVBT.SetHelper(v, helper); (* Add helper to File Browser *)
  FileBrowserVBT.SetDirMenu(v, dirmenu); (* Add DirMenu to File Browser *)
  Trestle.Install(main);
  Trestle.AwaitDelete(main);
END Browser.
