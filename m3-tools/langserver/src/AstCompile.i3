(* Copyright (C) 2023, Peter McKinna *)
(* All rights reserved.              *)
(* Licensed under the MIT license.   *)

INTERFACE AstCompile;

IMPORT M3ToolFrame, M3Context, RefSeq;

TYPE
  CompType = {Interface,Module,GenInt,GenMod};

  WorkerClosure <: WorkerClosurePublic;
  WorkerClosurePublic = M3ToolFrame.Worker OBJECT
    rootPath : TEXT;
  METHODS
    init() : WorkerClosure;
    getContext() : M3Context.T; 
    fileName() : TEXT;
    path(uri : TEXT) : TEXT;
    getUnit(uri : TEXT) : TEXT;
    compile(uri, text : TEXT) : RefSeq.T;
  END;

  (* equiv to NEW(WorkerClosure).init(); *)
  PROCEDURE New() : WorkerClosure;

VAR
  wc : WorkerClosure;

END AstCompile.
