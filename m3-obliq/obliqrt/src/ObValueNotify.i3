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
 * Created On      : Tue Oct 22 15:02:43 1996
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Wed Oct 23 20:14:30 1996
 * Update Count    : 11
 * 
 * $Source$
 * $Date$
 * $Author$
 * $Revision$
 * 
 * $Log$
 * Revision 1.1  1996/11/28 15:48:13  bm
 * New files needed for Obliq*
 *
 * 
 * HISTORY
 *)

INTERFACE ObValueNotify;
IMPORT ObValue, SynLocation;

TYPE
  ValObjCB <: ObjCBPublic;
  ObjCBPublic = ObValue.ValAnything OBJECT
    obj: ObValue.ValReplObj; 
    notifier: ObValue.ValSimpleObj;
  METHODS
    cancel();
  END;

PROCEDURE New(valObj: ObValue.ValReplObj; 
              notifyObj: ObValue.ValSimpleObj;
              loc : SynLocation.T := NIL): ObValue.Val
  RAISES {ObValue.Exception};
  (* Create a new callback notification object that causes updates
     arriving for "valObj" to trigger appropriate methods in
     "notifyObj". 

     If a update method "valObj.meth" is called at any replica of
     "valObj", then
     -> the method "pre`meth" will be called before the update is
        applied locally.
     -> the method "post`meth" will be called after the update is
        applied locally.
     The methods take the argument list "(valObj, arg1, arg2, ...)"
     where "arg1", "arg2", etc. are the args to "valObj.meth".
     
     If a field "valObj.field" is updated from outside an update
     method at any replica of "valObj", then
     -> the method "pre`field" will be called before the update is
        applied locally.
     -> the method "post`field" will be called after the update is
        applied locally.
     The methods take the argument list "(valObj, val)"
     where "val" is the value assigned to "valObj.field".
     
     If an update method of "valObj" is called from within another
     update method, or a field is assigned from within an update
     method, no callback will be called.

     The various methods of "notifyObj" should return a boolean,
     indicating if the handled the notification.  If the return value
     is false, or the method does not exist, a method "pre`anyChange"
     will be called for the "pre`" methods (and "post`anyChange" will
     be called for the "post`" methods) with the argument list
     "(valObj)".   This return value of this method is ignored.

     The various methods of "notifyObj" should not raise any
     exceptions.  All exceptions will be ignored.

     The "cancel" method is used to cancel the notification object, in
     the case where the programmer does not wish to wait until an
     unreferenced cancelation object is garbage collected.

  *)
     
END ObValueNotify.
