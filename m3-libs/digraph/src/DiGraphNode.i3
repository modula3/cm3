(* Copyright (C) 1996, Digital Equipment Corporation            *)
(* All rights reserved.                                         *)
(* See the file COPYRIGHT for a full description.               *)
(* Last modified on Thu Aug 22 16:28:55 PDT 1996 by detlefs     *)

GENERIC INTERFACE DiGraphNode(NodeVal);

IMPORT List;

TYPE
  T = REF RECORD
    value: NodeVal.T;
    succ, pred: List.T (* Of Edge *);
  END (* RECORD *);

END DiGraphNode.
