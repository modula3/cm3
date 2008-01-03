(* Copyright (C) 1989, 1993 Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* Last modified on Tue Aug 17 23:32:02 PDT 1993 by sfreeman *)

(* subclass of JvsBuffer for holding decompressed frames *)

INTERFACE JVFromDecomp;

IMPORT Jvs, JvsBuffer;

TYPE
  T = JvsBuffer.T BRANDED OBJECT
        params: Jvs.DcmpParams;
        cmap  : Jvs.ColormapInfo;
      END;

TYPE
  Factory <: FactoryPublic;
  FactoryPublic = JvsBuffer.Factory OBJECT
                  METHODS
                    init (jvs: Jvs.T): Factory;
                    (* the "jvs" is used to create shared memory buffers.
                       It should already have been opened *)
                  END;

END JVFromDecomp.
