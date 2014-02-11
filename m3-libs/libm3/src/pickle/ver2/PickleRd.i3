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

UNSAFE INTERFACE PickleRd;

IMPORT RTPacking, Pickle2 AS Pickle, ConvertPacking;

REVEAL
  Pickle.Reader <: Private;

TYPE
  Private = Pickle.ReaderPublic OBJECT
      packing: RTPacking.T;
      packingCode: INTEGER;
      wordConvKind: ConvertPacking.CPKind;
      longConvKind: ConvertPacking.CPKind;
      widecharConvKind: ConvertPacking.CPKind;
    END;

VAR myPacking: RTPacking.T;       (* our local packing. *)

END PickleRd.
