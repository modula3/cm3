(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE StubCode;

IMPORT Type, Atom;

TYPE MethodData = RECORD name, intf: Atom.T; sig: Type.Signature END;
     MethodList = REF ARRAY OF MethodData;

PROCEDURE BrandsOK(t: Type.T;
                    existingSuper: Type.T := NIL): BOOLEAN;
(* RETURN "TRUE" if all branded types involved in the definition of t
   have explicity brands.  Print an error message for each type
   that doesn't.  *)


PROCEDURE GenStub(t: Type.Object; 
                    objName: Type.Qid;
                    existingSuper: Type.T := NIL;
                    existingSuperName: Type.Qid := NIL);
(* Generate stubs for the network object with (revealed) type
   t.  objName is the declared name of the object type.  If
   existingSuper is not NIL, it is a superType of t, and
   the stubs generated for t should use the surrogate methods
   and owner stubs of existingSuper.  existingSuperName is the
   name of existingSuper *)

PROCEDURE SigForStub(m: Type.Signature): Type.Signature;
(* Return a signature for the owner stub procedure for a method
   with signature m.  *)

PROCEDURE PragmasForStub(): REF ARRAY OF TEXT;
(* Return pragmas to be associated with arguments in the stub *)

END StubCode.
