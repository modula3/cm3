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
 * Created On      : Wed Oct 16 09:25:53 1996
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Thu Feb 20 11:25:37 1997
 * Update Count    : 24
 * 
 * $Source: /opt/cvs/cm3/m3-obliq/obliqrt/src/ObValueRep.i3,v $
 * $Date: 2001-01-24 21:52:39 $
 * $Author: wagner $
 * $Revision: 1.1.1.1 $
 * 
 * $Log: not supported by cvs2svn $
 * Revision 1.2  1997/03/12 21:48:05  bm
 * added commands for pickling
 *
 * Revision 1.1  1996/11/28 15:48:15  bm
 * New files needed for Obliq*
 *
 * 
 * HISTORY
 *)

(* The revelation of the RemObjServer object, which must be in an
   interface so the shared object code generator can retrieve it. *)

INTERFACE ObValueRep;

FROM ObValue IMPORT ReplObj, ReplObjPublic, ValReplObj,
                    ObjFields, ServerError, Exception, Error, Val, Vals;
IMPORT SharedObj;

REVEAL
  ReplObj = ReplObjPublic BRANDED "ReplObjServerRep" OBJECT
    who: TEXT; 
    self: ValReplObj;
    protected: BOOLEAN;
    fields: REF ObjFields;

    pickleIn: REF ObjFields := NIL;
    pickleOut: REF ObjFields := NIL;
  METHODS
    InvokeUpdate(label: TEXT; argNo: INTEGER; READONLY args: Vals;
                 VAR hint: INTEGER): Val
          RAISES {Error, Exception, ServerError, SharedObj.Error} :=
              ReplObjInvokeUpdate;
  OVERRIDES
    init := ReplObjInit;
    Who := ReplObjWho;
    Select := ReplObjSelect;
    Invoke := ReplObjInvoke;
    Update := ReplObjUpdate;
    Has := ReplObjHas;
    Obtain := ReplObjObtain;
  END;

PROCEDURE ReplObjInit(self: ReplObj): ReplObj;

PROCEDURE ReplObjWho(self: ReplObj; VAR protected: BOOLEAN): TEXT;

PROCEDURE ReplObjSelect(self: ReplObj; 
                        label: TEXT;
                        VAR hint: INTEGER): Val 
  RAISES {Error, Exception, ServerError, SharedObj.Error};

PROCEDURE ReplObjInvoke(self: ReplObj; 
                        label: TEXT; argNo: INTEGER; READONLY args: Vals;
                        VAR hint: INTEGER): Val
  RAISES {Error, Exception, ServerError, SharedObj.Error};

PROCEDURE ReplObjInvokeUpdate(self: ReplObj; 
                              label: TEXT; argNo: INTEGER; READONLY args: Vals;
                              VAR hint: INTEGER): Val
  RAISES {Error, Exception, ServerError};

PROCEDURE ReplObjUpdate(self: ReplObj; 
                        label: TEXT; val: Val; internal: BOOLEAN; 
                        VAR hint: INTEGER) RAISES {ServerError};

PROCEDURE ReplObjHas(self: ReplObj; 
                     label: TEXT; VAR hint: INTEGER): BOOLEAN;

PROCEDURE ReplObjObtain(self: ReplObj; 
                        internal: BOOLEAN): REF ObjFields RAISES {ServerError};

END ObValueRep.
