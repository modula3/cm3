(* Copyright (C) 1993, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Created by Marc Najork                                                    *)
(* Last modified on Fri Jul 14 11:29:59 PDT 1995 by najork                   *)


MODULE GroupGO EXPORTS GroupGO, GroupGOPrivate, GroupGOProxy;

IMPORT AnimServer, GO, GOPrivate, GraphicsBase, GraphicsBasePrivate, Prop;

REVEAL
  T = Private BRANDED OBJECT
  OVERRIDES
    init               := Init;                (* GroupGO.Public *)
    add                := Add;                 (* GroupGO.Public *)
    remove             := Remove;              (* GroupGO.Public *)
    flush              := Flush;               (* GroupGO.Public *)
    content            := Content;             (* GroupGO.Public *)
    findName           := FindName;            (* GO.Public *)
    draw               := Draw;                (* GO.Private *)
    needsTransparency  := NeedsTransparency;   (* GO.Private *)
    adjust             := Adjust;              (* GO.Private *)
    damageIfDependent  := DamageIfDependent;   (* GO.Private *)
    undamage           := Undamage;            (* GO.Private *)
  END;


PROCEDURE Init (self : T; initSize : INTEGER) : T =
  BEGIN
    (*** No protection needed ***)
    EVAL GO.T.init (self);
    self.children := NEW (REF ARRAY OF GO.T, initSize); (* grow as needed *)
    self.last := -1;
    self.damaged := FALSE;  (* groups are not damaged at creation *)

    IF MkProxyT # NIL THEN
      MkProxyT (self);
    END;

    RETURN self;
  END Init;


PROCEDURE New (initSize : INTEGER) : T =
  BEGIN
    RETURN NEW (T).init (initSize);
  END New;


PROCEDURE Add (self : T; o : GO.T) =
  BEGIN
    (*** Must be protected from interference with the animation server ***)
    LOCK AnimServer.internalLock DO
      self.damaged := TRUE;
      INC (self.last);
      IF self.last > LAST (self.children^) THEN
        WITH n   = NUMBER (self.children^), 
             tmp = NEW (REF ARRAY OF GO.T, 2 * n) DO
          SUBARRAY (tmp^, 0, n) := self.children^;
          self.children := tmp;
        END;
      END;
      self.children[self.last] := o;
    END;
  END Add;


PROCEDURE Remove(self : T; o : GO.T) RAISES {BadElement} =
  BEGIN
    (*** Must be protected from interference with the animation server ***)
    LOCK AnimServer.internalLock DO
      self.damaged := TRUE;

      WITH B = self.children^ DO
        FOR i := 0 TO self.last DO
          IF B[i] = o THEN
            B[i] := B[self.last];
            B[self.last] := NIL;
            DEC (self.last);
            RETURN;
          END;
        END;
      END;
      RAISE BadElement;
    END;
  END Remove;


PROCEDURE Flush (self : T) =
  BEGIN
    (*** Must be protected from interference with the animation server ***)
    LOCK AnimServer.internalLock DO
      self.damaged := TRUE;
      FOR i := 0 TO self.last DO
        self.children[i] := NIL;
      END;
      self.last := -1;
    END;
  END Flush;


PROCEDURE Content (self : T) : REF ARRAY OF GO.T =
  BEGIN
    WITH res = NEW (REF ARRAY OF GO.T, self.last + 1) DO
      res^ := SUBARRAY (self.children^, 0, self.last + 1);
      RETURN res;
    END;
  END Content;


PROCEDURE FindName (self : T; name : TEXT) : GO.T =
  BEGIN
    (* No interference with the animation server. Possible interference with 
       other client threads; client code has to provide adequate protection. *)
    IF GO.T.findName (self, name) # NIL THEN
      RETURN self;
    ELSE
      FOR i := 0 TO self.last DO
        WITH go = self.children[i].findName (name) DO
          IF go # NIL THEN
            RETURN go;
          END;
        END;
      END;
    END;
    RETURN NIL;
  END FindName;


(*****************************************************************************)
(* Private Methods:                                                          *)
(*                                                                           *)
(* These procedures are called only from the animation server thread.        *)
(* Their locking level is {AnimServer.externalLock,AnimServer.internalLock}  *)
(*****************************************************************************)


PROCEDURE Draw (self : T; state : GraphicsBase.T) =
  BEGIN
    state.push (self);

    FOR i := 0 TO self.last DO
      self.children[i].draw (state);
    END;

    state.pop (self);
  END Draw;


PROCEDURE NeedsTransparency (self : T; t : REAL) : BOOLEAN =
  BEGIN
    IF self.trans # FIRST(REAL) THEN
      t := self.trans;
    END;
    
    FOR i := 0 TO self.last DO
      IF self.children[i].needsTransparency (t) THEN
        RETURN TRUE;
      END;
    END;
    RETURN FALSE;
  END NeedsTransparency;


PROCEDURE Adjust (self : T; time : LONGREAL) = 
  BEGIN
    (*** adjust properties of self ***)
    GO.T.adjust (self, time);
    
    FOR i := 0 TO self.last DO
      self.children[i].adjust (time);
      IF self.children[i].damaged THEN
        self.damaged := TRUE;
      END;
    END;
  END Adjust;


PROCEDURE DamageIfDependent (self : T; pn : Prop.Name) =
  BEGIN
    FOR i := 0 TO self.last DO
      self.children[i].damageIfDependent (pn);
      IF self.children[i].damaged THEN
        self.damaged := TRUE;
      END;
    END;
  END DamageIfDependent;


PROCEDURE Undamage (self: T) =
  BEGIN
    FOR i := 0 TO self.last DO
      self.children[i].undamage ();
    END;
    self.damaged := FALSE;
  END Undamage;


BEGIN
END GroupGO.
