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
 * Last Modified On: Mon Apr  6 20:15:22 1998
 * Update Count    : 47
 * 
 * $Source$
 * $Date$
 * $Author$
 * $Revision$
 * 
 * $Log$
 * Revision 1.6  1998/05/11 02:28:42  bm
 * Integrating Repo properly including help and scripts
 *
 * Revision 1.5  1997/10/22 14:27:01  bm
 * Changed "node" to "site" in a bunch of the replicated object functions
 * Added IsNaN to "real"
 * Allow aliases in replicated objects (why not?)
 *
 * Revision 1.4  1997/08/04 20:16:08  bm
 * Fixed BRANDs
 *
 * Revision 1.3  1997/07/11 17:37:53  bm
 * Potential release version
 *
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
                    ReplVar, ReplVarPublic, ObjFieldTypes,
                    ReplArray, ReplArrayPublic, ValArray,
                    ObjFields, ServerError, Exception, Error, Val, Vals;
IMPORT SharedObj, SynWr;

REVEAL
  ReplObj = ReplObjPublic BRANDED "ObValueRep.ReplObjServerRep" OBJECT
    who: TEXT; 
    self: ValReplObj;
    protected: BOOLEAN;
    fields: REF ObjFields;

    pickleIn: REF ObjFields := NIL;
    pickleOut: REF ObjFields := NIL;
  METHODS
    InvokeUpdate(swr: SynWr.T; label: TEXT; argNo: INTEGER; 
                 READONLY args: Vals; VAR hint: INTEGER): Val
          RAISES {Error, Exception, ServerError, SharedObj.Error} :=
              ReplObjInvokeUpdate;
    RedirectFields(newFields: REF ObjFields) RAISES {SharedObj.Error} :=
              ReplObjRedirectFields;
  OVERRIDES
    init := ReplObjInit;
    Who := ReplObjWho;
    Select := ReplObjSelect;
    Invoke := ReplObjInvoke;
    Update := ReplObjUpdate;
    Redirect := ReplObjRedirect;
    Has := ReplObjHas;
    Obtain := ReplObjObtain;
    ObtainField := ReplObjObtainField;
    ObtainDescriptions := ReplObjObtainDescriptions;
    Describe := ReplObjDescribe;
  END;

  ReplVar = ReplVarPublic BRANDED "ObValueRep.ReplVarServerRep" OBJECT
    val: Val;
  OVERRIDES
    init := ReplVarInit;
    Set  := ReplVarSet;
    Get  := ReplVarGet;
  END;

  ReplArray = ReplArrayPublic BRANDED "ObValueRep.ReplArrayServerRep" OBJECT
    array: REF Vals;
  OVERRIDES
    init   := ReplArrayInit;
    Size   := ReplArraySize;
    Get    := ReplArrayGet;
    Set    := ReplArraySet;
    Sub    := ReplArraySub;
    Upd    := ReplArrayUpd;
    Obtain := ReplArrayObtain;
  END;

PROCEDURE ReplObjInit(self: ReplObj): ReplObj;

PROCEDURE ReplObjWho(self: ReplObj; VAR protected: BOOLEAN): TEXT;

PROCEDURE ReplObjSelect(self: ReplObj; 
                        swr: SynWr.T; 
                        label: TEXT;
                        VAR hint: INTEGER): Val 
  RAISES {Error, Exception, ServerError, SharedObj.Error};

PROCEDURE ReplObjInvoke(self: ReplObj; 
                        swr: SynWr.T; 
                        label: TEXT; argNo: INTEGER; READONLY args: Vals;
                        VAR hint: INTEGER): Val
  RAISES {Error, Exception, ServerError, SharedObj.Error};

PROCEDURE ReplObjInvokeUpdate(self: ReplObj; 
                              swr: SynWr.T; 
                              label: TEXT; argNo: INTEGER; READONLY args: Vals;
                              VAR hint: INTEGER): Val
  RAISES {Error, Exception, ServerError};

PROCEDURE ReplObjUpdate(self: ReplObj; 
                        label: TEXT; val: Val; internal: BOOLEAN; 
                        VAR hint: INTEGER) RAISES {ServerError};

PROCEDURE ReplObjRedirect(self: ReplObj; val: Val; internal: BOOLEAN) 
  RAISES {ServerError, SharedObj.Error};

PROCEDURE ReplObjRedirectFields (self: ReplObj; newFields: REF ObjFields)
  RAISES {SharedObj.Error};

PROCEDURE ReplObjHas(self: ReplObj; 
                     label: TEXT; VAR hint: INTEGER): BOOLEAN;

PROCEDURE ReplObjObtain(self: ReplObj; 
                        internal: BOOLEAN): REF ObjFields RAISES {ServerError};

PROCEDURE ReplObjObtainField(self: ReplObj;
                             label: TEXT; internal: BOOLEAN): Val
  RAISES {ServerError, SharedObj.Error};

PROCEDURE ReplObjObtainDescriptions(self: ReplObj): REF ObjFieldTypes
  RAISES {ServerError, SharedObj.Error};

PROCEDURE ReplObjDescribe(self: ReplObj; label: TEXT): TEXT
  RAISES {ServerError, SharedObj.Error};

PROCEDURE ReplVarInit(self: ReplVar): ReplVar;

PROCEDURE ReplVarGet(self: ReplVar): Val;

PROCEDURE ReplVarSet(self: ReplVar; val: Val);

PROCEDURE ReplArraySize (arr: ReplArray): INTEGER;

PROCEDURE ReplArrayGet (self: ReplArray; i: INTEGER): Val RAISES {ServerError};

PROCEDURE ReplArraySet (self: ReplArray; i: INTEGER; val: Val)
  RAISES {ServerError};

PROCEDURE ReplArraySub (self: ReplArray; start, size: INTEGER): ValArray
  RAISES {ServerError, SharedObj.Error};

PROCEDURE ReplArrayUpd (     self       : ReplArray;
                             start, size: INTEGER;
                    READONLY otherArr   : REF Vals        )
  RAISES {ServerError};

PROCEDURE ReplArrayObtain (self: ReplArray): REF Vals;

PROCEDURE ReplArrayInit (self: ReplArray): ReplArray;

END ObValueRep.
