(* Copyright 1996 Critical Mass, Inc. All rights reserved.    *)

INTERFACE Roots;

IMPORT Node;

VAR (* READONLY *)
  PkgRootRoot    : Node.T;
  AnyPkgRoot     : Node.T;
  ResourceRoot   : Node.T;
  TypeRoot       : Node.T;
  TypeUIDRoot    : Node.T;
  InterfaceRoot  : Node.T;
  ModuleRoot     : Node.T;
  GenIntfRoot    : Node.T;
  GenImplRoot    : Node.T;
  CsourceRoot    : Node.T;
  HsourceRoot    : Node.T;
  AnyUnitRoot    : Node.T;
  ImporterRoot   : Node.T;
  ExporterRoot   : Node.T;
  LibraryRoot    : Node.T;
  ProgramRoot    : Node.T;
  BuildCacheRoot : Node.T;
  TutorialRoot   : Node.T;
  HelpRoot       : Node.T;
  RefManualRoot  : Node.T;
  SRCReportRoot  : Node.T;
  ExampleRoot    : Node.T;
  ConsoleLogRoot : Node.T;
  UserHomeDir    : Node.T;

PROCEDURE Init ();
(* Registers the root nodes of the browser graph *)

END Roots.
