(* 
 * For information about this program, contact Blair MacIntyre            
 * (bm@cs.columbia.edu) or Steven Feiner (feiner@cs.columbia.edu)         
 * at the Computer Science Dept., Columbia University,                    
 * 1214 Amsterdam Ave. Mailstop 0401, New York, NY, 10027.                
 *                                                                        
 * Copyright (C) 1995, 1996 by The Trustees of Columbia University in the 
 * City of New York.  Blair MacIntyre, Computer Science Department.       
 *
 * This file is released under the same conditions as Pickle.m3. See COPYRIGHT.
 *
 *)

INTERFACE PickleWr;

IMPORT RTPacking, Pickle2 AS Pickle, ConvertPacking;

REVEAL
  Pickle.Writer <: Private;

TYPE
  Private = Pickle.WriterPublic OBJECT
      packing: RTPacking.T;
      widecharConvKind: ConvertPacking.CPKind;
    (* WIDECHAR is the only type we will do any write conversion on, 
       specifically, 32-bit to WC21, if writing on a 32-bit WIDECHAR system. *) 
    END;

END PickleWr.
