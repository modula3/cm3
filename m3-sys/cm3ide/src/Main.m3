(* Copyright 1995-96 Critical Mass, Inc. All rights reserved.    *)

MODULE Main;

IMPORT Thread;
IMPORT ClassDir, Config, BrowserDB, Decl, Default, Derived, Dir, Display, Fixed;
IMPORT Form, NewPkg, Node, Pkg, PkgRoot, Rescan, Roots, Source, Type, WebServer;

PROCEDURE GrowHeap () =
  (* Get a decent sized heap.  We do this in a procedure to
     make sure the ref can be immediately collected. *)
  BEGIN
    EVAL NEW (REF ARRAY OF CHAR, 2000000);
  END GrowHeap;

BEGIN
  Thread.IncDefaultStackSize (Thread.GetDefaultStackSize ());
  GrowHeap ();

  Node.Init ();
  Default.Init ();

  (* forms *)
  Form.Init ();
  Config.Init ();
  Rescan.Init ();
  NewPkg.Init ();

  (* node classes *)
  Fixed.Init ();
  Dir.Init ();
  PkgRoot.Init ();
  Pkg.Init ();
  Source.Init ();
  Derived.Init ();
  Type.Init ();
  ClassDir.Init ();
  Decl.Init ();

  Roots.Init ();

  BrowserDB.Init ();
  WebServer.Run ();
  Display.Start ();
END Main.

