(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE AstToType;

IMPORT Atom, Wr;
IMPORT AST, ASTWalk, M3Context;
IMPORT Type;

TYPE
  Handle <: Public;
  Public = ASTWalk.Closure OBJECT
    wr: Wr.T;
    intf: Atom.T;
    context: M3Context.T;
  END;

PROCEDURE NewHandle(wr: Wr.T; intf: TEXT; c: M3Context.T): Handle; (*
Creates a new handle suitable for passing to "ASTWalk.VisitNodes", or
directly to "Node", from another walk. The "callback" method is set to
"Node".  If NetObj.T is not defined in context c, returns NIL *)

PROCEDURE ProcessTypes(c: M3Context.T; qids: REF ARRAY OF Type.Qid; 
                       name: TEXT; wr: Wr.T): INTEGER; 
(* Generate code for the shared object types in "qids".  "c" is the
current compilation context. "name" is the base output file name.
Errors and info output goes to "wr".  A non-zero returned value
indicates an error *)

PROCEDURE Node(h: Handle; n: AST.NODE; vm: ASTWalk.VisitMode);
(* If "n" is a "Proc_decl" node, output its name on "h.wr". *)

END AstToType.
