(* Copyright (C) 1993, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Created by Marc Najork                                                    *)
(* Last modified on Mon Jan 30 23:52:01 PST 1995 by najork                   *)


MODULE GO EXPORTS GO, GOPrivate;

IMPORT AnimServer, CB, GraphicsBase, GraphicsBasePrivate,
       KeyCB, KeyCBStack, Matrix4, MouseCB, MouseCBStack, PositionCB,
       PositionCBStack, Prop, PropList, PropPrivate, RealProp,
       RealPropPrivate, SurfaceGO, Text, TransformProp, TransformPropPrivate;

REVEAL 
  T = Private BRANDED OBJECT
  OVERRIDES
    init := Init;

    setProp   := SetProp;
    unsetProp := UnsetProp;
    getProp   := GetProp;

    setName  := SetName;
    getName  := GetName;
    findName := FindName;

    pushMouseCB   := PushMouseCB;
    popMouseCB    := PopMouseCB;
    removeMouseCB := RemoveMouseCB;
    invokeMouseCB := InvokeMouseCB;

    pushPositionCB   := PushPositionCB;
    popPositionCB    := PopPositionCB;
    removePositionCB := RemovePositionCB;
    invokePositionCB := InvokePositionCB;

    pushKeyCB   := PushKeyCB;
    popKeyCB    := PopKeyCB;
    removeKeyCB := RemoveKeyCB;
    invokeKeyCB := InvokeKeyCB;

    damageIfDependent    := DamageIfDependent;
    adjust               := Adjust;
    undamage             := Undamage;
  END;

PROCEDURE Init (self : T) : T =
  BEGIN
    self.props   := NIL;
    self.damaged := TRUE;
    self.name    := NIL;
    self.dl      := 0;

    self.mouseCBstack    := NEW (MouseCBStack.T).init ();
    self.positionCBstack := NEW (PositionCBStack.T).init ();
    self.keyCBstack      := NEW (KeyCBStack.T).init ();

    RETURN self;
  END Init;


(*****************************************************************************)
(* Property-related methods                                                  *)
(*****************************************************************************)


PROCEDURE SetProp (self : T; p : Prop.T) =
  VAR
    tmp : PropList.T;
  BEGIN
    (*** Must be protected from interference with the animation server ***)
    LOCK AnimServer.internalLock DO

      (* Damage the object and/or its descendents. The damage is propagated
         to the roots by the next call to "root.adjust (now)" *)
      p.n.damage (self);

      tmp := self.props;
      WHILE tmp # NIL DO
        IF tmp.head.n = p.n THEN
          tmp.head.v := p.v;
          RETURN;
        END;
        tmp := tmp.tail;
      END;
      self.props := PropList.Cons (p, self.props);
    END;
  END SetProp;


PROCEDURE UnsetProp (self : T; pn : Prop.Name) RAISES {PropUndefined} =
  VAR
    pv : Prop.Val;

  PROCEDURE Unset (VAR plist : PropList.T) RAISES {PropUndefined} =
    BEGIN
      IF plist = NIL THEN
        RAISE PropUndefined;
      ELSIF plist.head.n = pn THEN
        pv := plist.head.v;
        plist := plist.tail;
      ELSE
        Unset (plist.tail);
      END;  
    END Unset;

  BEGIN
    (*** Must be protected from interference with the animation server ***)
    LOCK AnimServer.internalLock DO

      (*** if (pn,pv) is a transmission coeff, void the cache ***)
      IF pn = SurfaceGO.TransmissionCoeff THEN
        self.trans := FIRST (REAL);
      END;
      Unset (self.props);

      (* Damage the object and/or its descendents. The damage is propagated
         to the roots by the next call to "root.adjust (now)" *)
      pn.damage (self);
    END;
  END UnsetProp;


PROCEDURE GetProp (self : T; pn : Prop.Name) : Prop.Val 
    RAISES {PropUndefined} =
  VAR
    tmp : PropList.T;
  BEGIN
    (* No interference with the animation server. Possible interference with 
       other client threads; client code has to provide adequate protection. *)

    tmp := self.props;
    WHILE tmp # NIL DO
      IF tmp.head.n = pn THEN
        RETURN tmp.head.v;
      END;
      tmp := tmp.tail;
    END;
    RAISE PropUndefined;
  END GetProp;


(*****************************************************************************)
(* Name-related methods                                                      *)
(*****************************************************************************)


PROCEDURE SetName (self : T; name : TEXT) =
  BEGIN
    self.name := name;
  END SetName;


PROCEDURE GetName (self : T) : TEXT =
  BEGIN
    RETURN self.name;
  END GetName;


PROCEDURE FindName (self : T; name : TEXT) : T =
  BEGIN
    IF self.name # NIL AND Text.Equal (self.name, name) THEN
      RETURN self;
    ELSE
      RETURN NIL;
    END;
  END FindName;

      
(*****************************************************************************)
(* Callback-related methods                                                  *)
(*****************************************************************************)


PROCEDURE PushMouseCB (self : T; cb : MouseCB.T) =
  BEGIN
    self.mouseCBstack.push (cb);
  END PushMouseCB;


PROCEDURE PopMouseCB (self : T) RAISES {StackError} =
  BEGIN
    self.mouseCBstack.pop ();
  END PopMouseCB;


PROCEDURE RemoveMouseCB (self : T; cb : MouseCB.T) RAISES {StackError} =
  BEGIN
    self.mouseCBstack.remove (cb);
  END RemoveMouseCB;


PROCEDURE InvokeMouseCB (self : T; mr : MouseCB.Rec) =
  BEGIN
    TRY
      self.mouseCBstack.invokeTop (mr);
    EXCEPT
      CB.BadMethod (msg) => AnimServer.ReportError (msg);
    END;
  END InvokeMouseCB;


PROCEDURE PushPositionCB (self : T; cb : PositionCB.T) =
  BEGIN
    self.positionCBstack.push (cb);
  END PushPositionCB;


PROCEDURE PopPositionCB (self : T) RAISES {StackError} =
  BEGIN
    self.positionCBstack.pop ();
  END PopPositionCB;


PROCEDURE RemovePositionCB (self : T; cb : PositionCB.T) RAISES {StackError} =
  BEGIN
    self.positionCBstack.remove (cb);
  END RemovePositionCB;


PROCEDURE InvokePositionCB (self : T; pr : PositionCB.Rec) =
  BEGIN
    TRY
      self.positionCBstack.invokeTop (pr);
    EXCEPT
      CB.BadMethod (msg) => AnimServer.ReportError (msg);
    END;
  END InvokePositionCB;


PROCEDURE PushKeyCB (self : T; cb : KeyCB.T) =
  BEGIN
    self.keyCBstack.push (cb);
  END PushKeyCB;


PROCEDURE PopKeyCB (self : T) RAISES {StackError} =
  BEGIN
    self.keyCBstack.pop ();
  END PopKeyCB;


PROCEDURE RemoveKeyCB (self : T; cb : KeyCB.T) RAISES {StackError} =
  BEGIN
    self.keyCBstack.remove (cb);
  END RemoveKeyCB;


PROCEDURE InvokeKeyCB (self : T; kr : KeyCB.Rec) =
  BEGIN
    TRY
      self.keyCBstack.invokeTop (kr);
    EXCEPT
      CB.BadMethod (msg) => AnimServer.ReportError (msg);
    END;
  END InvokeKeyCB;


(*****************************************************************************)
(* Rendering-related methods                                                 *)
(*****************************************************************************)


PROCEDURE DamageIfDependent (<* UNUSED *> self : T; 
                             <* UNUSED *> pn   : Prop.Name) =
  BEGIN
    (* by default, a GO is not interested in anything *)
  END DamageIfDependent;


PROCEDURE Adjust (self : T; time : LONGREAL) =
  VAR
    props := self.props;
  BEGIN
    self.trans := FIRST (REAL);
    WHILE props # NIL DO
      props.head.adjust (time, self);

      (* if there is a transmission coefficient attached, cache it *)
      IF props.head.n = SurfaceGO.TransmissionCoeff THEN
        self.trans := NARROW (props.head.v, RealProp.Val).val;
      END;

      props := props.tail;
    END;
  END Adjust;


PROCEDURE Undamage (self: T) =
  BEGIN
    self.damaged := FALSE;
  END Undamage;
    

(*****************************************************************************)
(* TransformPN                                                               *)
(*****************************************************************************)

TYPE 
  Transform_PN = TransformProp.Name OBJECT
  OVERRIDES
    damage  := DamageTransform;
    push    := PushTransform;
    pop     := PopTransform;
  END;


PROCEDURE DamageTransform (self : Transform_PN; caller : T) =
  BEGIN
    caller.damaged := TRUE;           (* damage the caller *)
    caller.damageIfDependent (self);  (* ... and maybe some of its children *)
  END DamageTransform;


PROCEDURE PushTransform (self  : Transform_PN; 
                         state : GraphicsBase.T; 
                         pv    : Prop.Val) =
  BEGIN
    WITH stack = NARROW (state.stacks[self.id], TransformPropPrivate.Stack),
         m = NARROW (pv, TransformProp.Val).val DO

      (*** push the matrix ***)
      state.pushMatrix (m);

      (* Push the composite transformation matrix onto the state stack.
         This matrix differs from the local transformation matrix on top 
         of the PEX matrix stack. *)
      stack.push (Matrix4.Multiply (stack.top, m));
    END;

  END PushTransform;


PROCEDURE PopTransform (self  : Transform_PN; state : GraphicsBase.T) =
  BEGIN
    (*** pop the matrix ***)
    state.popMatrix ();
    EVAL NARROW (state.stacks[self.id], TransformPropPrivate.Stack).pop();
  END PopTransform;


(*****************************************************************************)
(* Convenience procedures                                                    *)
(*****************************************************************************)

PROCEDURE GetTransform (o : T) : TransformProp.Val RAISES {PropUndefined} =
  BEGIN
    RETURN NARROW (o.getProp (Transform), TransformProp.Val);
  END GetTransform;


(*****************************************************************************)
(* Module body                                                               *)
(*****************************************************************************)

BEGIN
  Transform := NEW (Transform_PN).init (Matrix4.Id);
END GO.
