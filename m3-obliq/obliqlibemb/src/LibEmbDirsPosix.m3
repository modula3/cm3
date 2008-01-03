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
 * Author          : Blair MacIntyre
 * Created On      : Fri Oct  6 12:27:53 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Wed Mar 12 12:14:59 1997
 * Update Count    : 4
 * 
 * $Source: /opt/cvs/cm3/m3-obliq/obliqlibemb/src/LibEmbDirsPosix.m3,v $
 * $Date: 2001-01-24 21:52:38 $
 * $Author: wagner $
 * $Revision: 1.1.1.1 $
 * 
 * $Log: not supported by cvs2svn $
 * Revision 1.2  1997/03/12 21:34:55  bm
 * Moved sharedobj from coterie
 *
 * 
 * HISTORY
 *)

MODULE LibEmbDirsPosix EXPORTS LibEmbDirs;

IMPORT TextSeq;

BEGIN
  dirs := NEW(TextSeq.T).init();
  dirs.addhi("/proj/graphics/arm3/obliqlibemb/src");
END LibEmbDirsPosix. 
