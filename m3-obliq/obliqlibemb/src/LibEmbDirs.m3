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
 * Created On      : Wed Sep 24 11:56:56 1997
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Wed Sep 24 12:08:34 1997
 * Update Count    : 2
 * 
 * $Source$
 * $Date$
 * $Author$
 * $Revision$
 * 
 * $Log$
 * Revision 1.1  1998/05/13 17:24:40  bm
 * new files
 *
 * 
 * HISTORY
 *)

MODULE LibEmbDirs;

IMPORT TextSeq, Pathname;

BEGIN
  WITH dir = Env.Get("M3PACKAGEDIR") DO
    IF dir # NIL THEN
      dirs := NEW(TextSeq.T).init();
      dirs.addhi(Pathname.Join(dir,"obliqlibemb/src",NIL));
    ELSE
      dirs := NIL;
    END;
  END;
END LibEmbDirsPosix. 
