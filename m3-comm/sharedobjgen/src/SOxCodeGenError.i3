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
 * Last Modified On: Sat Aug  9 13:52:46 1997
 * Update Count    : 7
 * 
 * $Source: /opt/cvs/cm3/m3-comm/sharedobjgen/src/SOxCodeGenError.i3,v $
 * $Date: 2001-12-02 13:15:54 $
 * $Author: wagner $
 * $Revision: 1.1.1.1 $
 * 
 * $Log: not supported by cvs2svn $
 * Revision 1.3  1997/08/11 20:36:27  bm
 * Various fixes
 *
 * 
 * HISTORY
 *)

(* Based on StablegenError.i3 from the stablegen package       *)
(*                                                             *)
(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Created by Carsten Weich                                    *)
(* Last modified on Fri Sep 23 15:24:54 PDT 1994 by weich      *)

(* This module declares the error exception used by the shared stub
   generator.  There is an exception "E" which is used to propagate
   error messages to the top level procedure in "Main.m3". "Warning()"
   and "Failure()" are used to print out warnings to the standard
   error stream. They do not halt the generation of stub code. Further
   there is a "Fatal()" procedure which halts the stub generator in
   case of an unexpected error.  *)

INTERFACE SOxCodeGenError;

EXCEPTION E(TEXT);

PROCEDURE Warning (msg: TEXT);
(* Write "msg" to "Stdio.stderr", preceeded by
   ''SOCodeGen (warning):`` and followed by a newline *)

PROCEDURE Failure (msg: TEXT);
(* Write "msg" to "Stdio.stderr", preceeded by
   ''SOCodeGen error:`` and followed by a newline *)

PROCEDURE Fatal (msg: TEXT);
(* Write "msg" to "Stdio.stderr", preceeded by ''SOCodeGen:
   ***fatal error***`` and and halt the program
   ("Process.Exit()") *)

END SOxCodeGenError.
