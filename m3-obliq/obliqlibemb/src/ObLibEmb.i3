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
 * Created On      : Thu Aug 10 09:30:55 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Mon Jun 30 18:15:53 1997
 * Update Count    : 6
 * 
 * $Source$
 * $Date$
 * $Author$
 * $Revision$
 * 
 * $Log$
 * Revision 1.3  1997/07/11 17:37:22  bm
 * Potential release version
 *
 * Revision 1.2  1997/03/12 21:34:57  bm
 * Moved sharedobj from coterie
 *
 * 
 * HISTORY
 *)

INTERFACE ObLibEmb;

IMPORT ObLoader, SynWr;

PROCEDURE PackageSetup(wr: SynWr.T) : ObLoader.T; 

(* To be called at least once before any other use of the obliq
  embedding package.  Any package that builds on this library should use
  this loader to initialize their new loaders so that the environment is
  passed on. *)

END ObLibEmb. 
