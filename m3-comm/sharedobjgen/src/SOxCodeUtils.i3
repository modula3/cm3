(*                            -*- Mode: Modula-3 -*- 
 * 
 * For information about this program, contact Blair MacIntyre            
 * (bm@cs.columbia.edu) or Steven Feiner (feiner@cs.columbia.edu)         
 * at the Computer Science Dept., Columbia University,                    
 * 1214 Amsterdam Ave. Mailstop 0401, New York, NY, 10027.                
 *                                                                        
 * Copyright (C) 1995, 1996 by The Trustees of Columbia University in the 
 * City of New York.  Blair MacIntyre, Computer Science Department.       
 * 
 * Author          : Tobias Hoellerer (htobias)
 * Created On      : Fri Nov 10 17:37:04 EST 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Thu Sep 25 08:59:43 1997
 * Update Count    : 9
 * 
 * $Source$
 * $Date$
 * $Author$
 * $Revision$
 * 
 * $Log$
 * Revision 1.4  1997/10/22 14:45:10  bm
 * Bug fix.  Naming conflicts.
 *
 * Revision 1.3  1997/08/11 20:36:29  bm
 * Various fixes
 *
 * 
 * HISTORY
 *)

(* Based on GenTypeCode.i3 from the stablegen package       *)
(*                                                          *)
(* Copyright (C) 1989, Digital Equipment Corporation        *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(* Created by Susan Owicki                                  *)
(* Last modified on Wed Sep 21 09:46:56 PDT 1994 by weich   *)
(*      modified On Fri Feb 18 17:30:06 PST 1994 by kalsow  *)
(*      Modified On Mon May 17 13:26:35 PDT 1993 by mjordan *)
(*      Modified On Thu Apr 22 11:43:51 PDT 1993 by owicki  *)

(* Provide a procedure to produce Modula-3 code out of type
   structures. Provide a procedure to generate procedure
   header Modula-3 code. *)

INTERFACE SOxCodeUtils;

IMPORT Formatter, Type, SOxCodeFiles;

EXCEPTION Error(TEXT);
EXCEPTION Failure;

VAR
  perfMon: BOOLEAN;
  genObliqCode: BOOLEAN;
  genProxyCode: BOOLEAN;

PROCEDURE Message(text: TEXT);
(* Write "text" on  writer "stubchatter", preceeded by "stubgen: " and
   followed by a newline  *)

PROCEDURE SetPerfMon(flag: BOOLEAN);
(* Set flag to indicate whether stubs should include performance monitoring
   code   *)

PROCEDURE SetObliqCode(flag: BOOLEAN);
(* Set flag to indicate whether stubs should be generated for the
   obliq linkage code *)

PROCEDURE SetProxyCode(flag: BOOLEAN);
(* Set flag to indicate whether stubs should be generated for the
   proxy code *)

PROCEDURE HeaderComment(writer: Formatter.T; fileName: TEXT);
(* Write a file header comment for a file to be generated. The file is
   denoted by the typeName and the filetype (i.e. CB_I3, CB_M3, SO_I3
   ...)  *)   

PROCEDURE FileName(baseName: TEXT; filetype: SOxCodeFiles.T): TEXT;

PROCEDURE FileExtension(filetype: SOxCodeFiles.T): TEXT;

PROCEDURE DoFile(filetype: SOxCodeFiles.T): BOOLEAN;

PROCEDURE BaseName(type: Type.Qid): TEXT;

END SOxCodeUtils.

