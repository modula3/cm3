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
 * Created On      : Tue Oct 22 15:05:13 1996
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Mon Aug  4 11:04:03 1997
 * Update Count    : 35
 * 
 * $Source$
 * $Date$
 * $Author$
 * $Revision$
 * 
 * $Log$
 * Revision 1.4  1997/08/04 20:16:07  bm
 * Fixed BRANDs
 *
 * Revision 1.3  1997/07/11 17:37:52  bm
 * Potential release version
 *
 * Revision 1.2  1997/06/29 18:19:37  bm
 * pickling modules
 *
 * Revision 1.1  1996/11/28 15:48:14  bm
 * New files needed for Obliq*
 *
 * 
 * HISTORY
 *)

MODULE ObValueNotify;
IMPORT SynLocation, ObValue, ObValueRep, ObValueCB, SynWr, SharedObj,
       Thread, NetObj, Obliq;

TYPE
  ReplObjCB = ObValueCB.ReplObjStd OBJECT 
    val : ValObjCB;
  OVERRIDES
    pre_anyChange := ReplObjCB_pre_anyChange;
    post_anyChange := ReplObjCB_post_anyChange;
    pre_InvokeUpdate := ReplObjCB_pre_InvokeUpdate;
    post_InvokeUpdate := ReplObjCB_post_InvokeUpdate;
    pre_init := ReplObjCB_pre_init;
    post_init := ReplObjCB_post_init;
    pre_Update := ReplObjCB_pre_Update;
    post_Update := ReplObjCB_post_Update;
  END;

REVEAL
  ValObjCB = ObjCBPublic BRANDED "ObValueNotify.ObjCBPublic" OBJECT 
    cb: ObValueCB.ReplObjStd;
  OVERRIDES
    cancel := ObjCBCancel;
    Copy := ObjCBCopy;
  END;    

PROCEDURE ObjCBCopy(self: ValObjCB; tbl: ObValue.Tbl; 
                    loc: SynLocation.T): ObValue.ValAnything 
  RAISES {ObValue.Error, NetObj.Error, SharedObj.Error, Thread.Alerted} =
  VAR cache: REFANY;
      newVal: ValObjCB;
  BEGIN
    IF ObValue.TblGet(tbl, self, (*out*)cache) THEN RETURN cache END;
    newVal:= NEW(ValObjCB, picklable := FALSE, obj := NIL,
                 notifier := NIL,
                 tag:="Object`Notifier",
                 what := "<a replicated object notifier>");
    ObValue.TblPut(tbl, self, newVal);
    newVal.obj := ObValue.CopyVal(self.obj, tbl, loc);
    newVal.notifier := ObValue.CopyVal(self.notifier, tbl, loc);
    TYPECASE self.obj.replica OF
    | ObValue.ReplObjStd(ro) =>
      newVal.cb := NEW(ReplObjCB, val := newVal).init(ro);
    ELSE ObValue.RaiseError("unexpected error copying notifier", loc);
      <*ASSERT FALSE*>
    END;
    RETURN newVal;
  END ObjCBCopy;

PROCEDURE New(valObj: ObValue.ValReplObj; 
              notifyObj: ObValue.ValSimpleObj;
              loc: SynLocation.T := NIL): ObValue.Val
  RAISES {ObValue.Exception} =
  VAR self:= NEW(ValObjCB, picklable := FALSE, obj := valObj,
                 notifier := notifyObj,
                 tag:="Object`Notifier",
                 what := "<a replicated object notifier>");
  BEGIN
     TYPECASE valObj.replica OF
     | ObValue.ReplObjStd(ro) =>
       self.cb := NEW(ReplObjCB, val := self).init(ro);
     ELSE ObValue.RaiseException(ObValue.sharedException,
				  "can only create notifiers on " &
				  "replicated objects", loc);
       <*ASSERT FALSE*>
     END;
     RETURN self;
   END New;

PROCEDURE ObjCBCancel(self: ValObjCB) =
  BEGIN
    self.cb.cancel();
  END ObjCBCancel;

 PROCEDURE ReplObjCB_pre_init (<*UNUSED*>self: ReplObjCB; 
			       <*UNUSED*>READONLY obj: ObValue.ReplObjStd): 
   BOOLEAN =
   BEGIN
     RETURN TRUE; (*Won't happen, and we don't care if it does! So there!*)
   END ReplObjCB_pre_init;

 PROCEDURE ReplObjCB_post_init (<*UNUSED*>self: ReplObjCB; 
				<*UNUSED*>READONLY obj: ObValue.ReplObjStd): 
   BOOLEAN =
   BEGIN
     RETURN TRUE; (*Won't happen, and we don't care if it does! So there!*)
   END ReplObjCB_post_init;

 PROCEDURE Invoke (self: ReplObjCB; 
		   READONLY meth: TEXT;
		   READONLY args: ObValue.Vals;
                   swr: SynWr.T): BOOLEAN =
   VAR hint: INTEGER;
  BEGIN
    TRY
      TRY
        IF NOT self.val.notifier.Has(meth, hint) THEN
          RETURN FALSE;
        END;
        TYPECASE self.val.notifier.Invoke (swr, meth, 
                                           NUMBER(args), args,
                                           FALSE, hint) OF
        | ObValue.ValBool(b) => RETURN b.bool;
        ELSE
          RETURN FALSE;
        END;
      EXCEPT
      | ObValue.ServerError(t) => 
        ObValue.RaiseException(ObValue.netException, meth & ": " & t, NIL);
      | SharedObj.Error(ec) => 
        ObValue.RaiseSharedException(meth & ": ", ec, NIL);
      | NetObj.Error(ec) => 
        ObValue.RaiseNetException(meth & ": ", ec, NIL);
      | Thread.Alerted => 
        ObValue.RaiseException(ObValue.threadAlerted, meth & ": ", NIL);
      END;
    EXCEPT
    | ObValue.Error(er) => ObValue.ErrorMsg(swr, er);
    | ObValue.Exception(ex) => ObValue.ExceptionMsg(swr, ex);
    END;
    RETURN FALSE;
  END Invoke;

PROCEDURE ReplObjCB_pre_anyChange (self: ReplObjCB; 
                                   READONLY obj: ObValue.ReplObjStd) =
  BEGIN
    WITH args = ObValue.Vals {obj.self} DO
      EVAL Invoke(self, "pre`anyChange", args, Obliq.Console());
    END;
  END ReplObjCB_pre_anyChange;

PROCEDURE ReplObjCB_post_anyChange (self: ReplObjCB; 
                                    READONLY obj: ObValue.ReplObjStd) =
  BEGIN
    WITH args = ObValue.Vals {obj.self} DO
      EVAL Invoke(self, "post`anyChange", args, Obliq.Console());
    END;
  END ReplObjCB_post_anyChange;

PROCEDURE ReplObjCB_pre_InvokeUpdate (         self: ReplObjCB;
                                      READONLY obj : ObValue.ReplObjStd;
                                       swr  : SynWr.T;
                                                 label: TEXT;
                                                 argNo: INTEGER;
                                      READONLY args: ObValue.Vals;
                                      <*UNUSED*> VAR hint: INTEGER):
  BOOLEAN =
  BEGIN
    WITH args1 = NEW(REF ObValue.Vals, argNo + 1) DO
      args1[0] := obj.self;
      SUBARRAY(args1^, 1, argNo) := SUBARRAY(args, 0, argNo);

      RETURN Invoke(self, "pre`" & label, args1^, swr);
    END;
  END ReplObjCB_pre_InvokeUpdate;

PROCEDURE ReplObjCB_post_InvokeUpdate (         self: ReplObjCB;
                                       READONLY obj : ObValue.ReplObjStd;
                                        swr  : SynWr.T;
                                                  label: TEXT;
                                                  argNo: INTEGER;
                                       READONLY args: ObValue.Vals;
                                       <*UNUSED*> VAR hint: INTEGER):
  BOOLEAN =
  BEGIN
    WITH args1 = NEW(REF ObValue.Vals, argNo + 1) DO
      args1[0] := obj.self;
      SUBARRAY(args1^, 1, argNo) := SUBARRAY(args, 0, argNo);

      RETURN Invoke(self, "post`" & label, args1^, swr);
    END;
  END ReplObjCB_post_InvokeUpdate;

PROCEDURE ReplObjCB_pre_Update (self: ReplObjCB; 
                                READONLY obj: ObValue.ReplObjStd;
                                label: TEXT;
                                val: ObValue.Val;
                                <*UNUSED*>internal: BOOLEAN;
                                <*UNUSED*>VAR hint: INTEGER): BOOLEAN =
  BEGIN
    WITH args = ObValue.Vals {obj.self, val} DO
      RETURN Invoke(self, "pre`" & label, args, Obliq.Console());
    END;
  END ReplObjCB_pre_Update;

PROCEDURE ReplObjCB_post_Update (self: ReplObjCB; 
                                 READONLY obj: ObValue.ReplObjStd;
                                  label: TEXT;
                                  val: ObValue.Val;
                                  <*UNUSED*>internal: BOOLEAN;
                                  <*UNUSED*>VAR hint: INTEGER): BOOLEAN =
  BEGIN
    WITH args = ObValue.Vals {obj.self, val} DO
      RETURN Invoke(self, "post`" & label, args, Obliq.Console());
    END;
  END ReplObjCB_post_Update;

BEGIN
END ObValueNotify.
