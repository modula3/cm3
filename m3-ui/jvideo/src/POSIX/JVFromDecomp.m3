(* Copyright (C) 1989, 1993 Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* Last modified on Tue Aug 17 23:33:04 PDT 1993 by sfreeman *)

MODULE JVFromDecomp;

IMPORT Jvs, JvsBuffer;

REVEAL
  Factory = FactoryPublic BRANDED OBJECT
            OVERRIDES
              init   := Init;
              newBuf := NewBuf;
            END;

PROCEDURE Init (f: Factory; jvs: Jvs.T): Factory =
  BEGIN
    RETURN JvsBuffer.Factory.init(f, jvs, Jvs.BufferType.Decompress);
  END Init;

PROCEDURE NewBuf (<*UNUSED *> f: Factory): JvsBuffer.T =
  BEGIN
    RETURN NEW(T);
  END NewBuf;

BEGIN
END JVFromDecomp.
