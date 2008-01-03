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
 * Last Modified On: Thu Sep 25 09:14:03 1997
 * Update Count    : 17
 * 
 * $Source: /opt/cvs/cm3/m3-comm/sharedobjgen/src/SOxCodeFiles.m3,v $
 * $Date: 2001-12-03 17:23:37 $
 * $Author: wagner $
 * $Revision: 1.2 $
 * 
 * $Log: not supported by cvs2svn $
 * Revision 1.1.1.1  2001/12/02 13:15:54  wagner
 * Blair MacIntyre's sharedobjgen package
 *
 * Revision 1.5  1997/10/22 14:45:09  bm
 * Bug fix.  Naming conflicts.
 *
 * Revision 1.4  1997/08/11 20:36:27  bm
 * Various fixes
 *
 * 
 * HISTORY
 *)

MODULE SOxCodeFiles;

IMPORT SOxIntfCBCode, SOxModuleCBCode, SOxIntfProxyCode, SOxIntfCBProxyCode,
       SOxModuleSOCode, SOxDummyCode, SOxIntfPklCode;

BEGIN
  coderTable[T.CB_I3] := NEW(SOxIntfCBCode.T);
  coderTable[T.CB_M3] := NEW(SOxModuleCBCode.T);
  coderTable[T.PRX_I3] := NEW(SOxIntfProxyCode.T);
  coderTable[T.CBPRX_I3] := NEW(SOxIntfCBProxyCode.T);
  coderTable[T.SO_M3] := NEW(SOxModuleSOCode.T);
  coderTable[T.PKL_I3] := NEW(SOxIntfPklCode.T);

  coderTable[T.OB_I3] := NEW(SOxDummyCode.T);
  coderTable[T.OB_M3] := NEW(SOxDummyCode.T);
  coderTable[T.OBCB_I3] := NEW(SOxDummyCode.T);
  coderTable[T.OBCB_M3] := NEW(SOxDummyCode.T);
  coderTable[T.OB_OBL] := NEW(SOxDummyCode.T);
  coderTable[T.OB_HLP] := NEW(SOxDummyCode.T);
  coderTable[T.OBCB_OBL] := NEW(SOxDummyCode.T); 
  coderTable[T.OBCB_HLP] := NEW(SOxDummyCode.T);
END SOxCodeFiles.

      
