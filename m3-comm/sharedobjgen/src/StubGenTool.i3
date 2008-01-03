(*                            -*- Mode: Modula-3 -*- 
 * 
 * For information about this program, contact Blair MacIntyre            
 * (bm@cs.columbia.edu) or Steven Feiner (feiner@cs.columbia.edu)         
 * at the Computer Science Dept., Columbia University,                    
 * 1214 Amsterdam Ave. Mailstop 0401, New York, NY, 10027.                
 *                                                                        
 * Copyright (C) 1995, 1996 by The Trustees of Columbia University in the 
 * City of New York.  Blair MacIntyre, Computer Science Department.       
 * See file COPYRIGHT-COLUMBIA for details.
 * 
 * Author          : Tobias Hoellerer (htobias)
 * Created On      : Fri Nov 10 17:37:04 EST 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Thu Sep 25 08:57:57 1997
 * Update Count    : 11
 * 
 * $Source: /opt/cvs/cm3/m3-comm/sharedobjgen/src/StubGenTool.i3,v $
 * $Date: 2001-12-03 17:23:37 $
 * $Author: wagner $
 * $Revision: 1.2 $
 * 
 * $Log: not supported by cvs2svn $
 * Revision 1.1.1.1  2001/12/02 13:15:54  wagner
 * Blair MacIntyre's sharedobjgen package
 *
 * Revision 1.4  1997/10/22 14:45:14  bm
 * Bug fix.  Naming conflicts.
 *
 * Revision 1.3  1997/08/11 20:36:42  bm
 * Various fixes
 *
 * 
 * HISTORY
 *)

(* Based on StubGenTool.i3 from the netobj stubgen package     *)
(*                                                             *)
(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE StubGenTool;

IMPORT M3Context, M3Args, M3AST_AS;
IMPORT Type;

VAR tool_g: M3Args.T;

CONST
  SOOutputName_Arg = "Outputname";
  SOTypes_Arg = "SharedObjects";
  SOExists_Arg = "UseTypes";
  SOPerf_Arg = "PerfMon";
  SOObliq_Arg = "ObliqCode";
  SOProxy_Arg = "Proxy";

PROCEDURE Init();
(* Register the stub generator with the compiler front end *)

PROCEDURE GetArgs(tool: M3Args.T);
(* Get command line arguments to initialize stubTypes, perfMon, useTypes *)

VAR (* command-line arguments *)
  outputName : TEXT;
  sharedTypes: REF ARRAY OF Type.Qid;
  useTypes: REF ARRAY OF Type.Qid;
  interfaces: REF ARRAY OF TEXT;
  
PROCEDURE Set(context: M3Context.T; cu: M3AST_AS.Compilation_Unit);
(* Stub generate "cu" *)

END StubGenTool.
