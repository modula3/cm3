(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: MxRep.i3                                              *)
(* Last Modified On Tue Aug  2 07:36:25 PDT 1994 By kalsow     *)

INTERFACE MxRep;

IMPORT Mx, MxMap, MxVSSet, MxVS;
 
REVEAL
  Mx.LinkSet = BRANDED "Mx.LinkSet 1.0" REF LinkRec;

TYPE
  LinkRec = RECORD
    interfaces    : MxMap.T   := NIL; (* name -> Unit *)
    modules       : MxMap.T   := NIL; (* name -> Unit *)
    virtuals      : MxMap.T   := NIL; (* name -> Unit *)
    clients       : MxMap.T   := NIL; (* name -> SET OF Unit *)
    vs_exports    : MxVSSet.T := NIL; (* vs -> vs *)
    vs_impls      : MxVSSet.T := NIL; (* vs -> vs *)
    exported_types: MxMap.T   := NIL; (* type name -> BOOLEAN *)
  END;

PROCEDURE UnitName (u: Mx.Unit): TEXT;

PROCEDURE GetVirtualUnit (x: Mx.LinkSet;  nm: Mx.Name;
                          client: Mx.Unit): Mx.Unit;

(**
PROCEDURE AddVirtualInfo (u: Mx.Unit;  VAR z: Mx.InfoList;  i: INTEGER);
**)

PROCEDURE GetStamp  (x     : Mx.LinkSet;
                     set   : MxVSSet.T;
                     vs    : MxVS.T;
          VAR(*OUT*) unit  : Mx.Unit;
          VAR(*OUT*) stamp : MxVS.T);

PROCEDURE GetExportedObject (x       : Mx.LinkSet;
                             o       : Mx.ObjectType;
                  VAR(*OUT*) unit    : Mx.Unit;
                  VAR(*OUT*) object  : Mx.ObjectType);

PROCEDURE GetExportedRevelation (x    : Mx.LinkSet;
                                 r    : Mx.Revelation;
                      VAR(*OUT*) unit : Mx.Unit;
                      VAR(*OUT*) rev  : Mx.Revelation);

END MxRep.

