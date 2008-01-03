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
 * Author          : Blair MacIntyre
 * Created On      : Thu May 11 09:58:54 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Thu Nov 21 17:51:40 1996
 * Update Count    : 10
 * 
 * $Source: /opt/cvs/cm3/m3-comm/events/src/Work.i3,v $
 * $Date: 2001-12-02 00:20:38 $
 * $Author: wagner $
 * $Revision: 1.2 $
 * 
 * $Log: not supported by cvs2svn $
 * Revision 1.1.1.1  2001/12/02 00:06:45  wagner
 * Blair MacIntyre's events library
 *
 * Revision 1.2  1996/11/21 22:51:45  bm
 * fixed header
 *
 * 
 * HISTORY
 *)

(* A Work.T is a piece of data representing a unit of work consumed by
   a Worker.T.  It has a separate interface so it can be used as a
   parameter to generic data structure modules.
*)

INTERFACE Work;

IMPORT Thread;

CONST Brand = "Work";

TYPE
  T = OBJECT
    METHODS
      handle() RAISES {Thread.Alerted};
    END;

END Work.
