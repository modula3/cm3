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
 * Last Modified On: Sat Aug  9 13:52:59 1997
 * Update Count    : 8
 * 
 * $Source$
 * $Date$
 * $Author$
 * $Revision$
 * 
 * $Log$
 * Revision 1.1.1.1  2001/12/02 13:15:54  wagner
 * Blair MacIntyre's sharedobjgen package
 *
 * Revision 1.3  1997/08/11 20:36:28  bm
 * Various fixes
 *
 * 
 * HISTORY
 *)

(* Based on StablegenError.m3 from the stablegen package       *)
(*                                                             *)
(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Created by Carsten Weich                                    *)
(* Last modified on Fri Sep 23 15:25:36 PDT 1994 by weich      *)

MODULE SOxCodeGenError;

IMPORT Wr, Thread, Stdio, Process;

<*FATAL Thread.Alerted, Wr.Failure*>

PROCEDURE Warning (msg: TEXT) =
  BEGIN
    Wr.PutText(Stdio.stderr, "SOCodeGen (warning): ");
    Wr.PutText(Stdio.stderr, msg&"\n");
    Wr.Flush(Stdio.stderr);
  END Warning;

PROCEDURE Failure (msg: TEXT) =
  BEGIN
    Wr.PutText(Stdio.stderr, "SOCodeGen error: ");
    Wr.PutText(Stdio.stderr, msg&"\n");
    Wr.Flush(Stdio.stderr);
  END Failure;

PROCEDURE Fatal (msg: TEXT) =
  BEGIN
    Wr.PutText(Stdio.stderr, "SOCodeGen **fatal error**: ");
    Wr.PutText(Stdio.stderr, msg&"\n");
    Wr.PutText(Stdio.stderr, "SOCodeGen: aborted\n ");
    Process.Exit(1);
  END Fatal;
BEGIN
END SOxCodeGenError.
