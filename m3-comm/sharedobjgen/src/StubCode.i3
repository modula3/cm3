(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE StubCode;

IMPORT Type, ImportList, UpdateMethodsTbl;

PROCEDURE BrandsOK(t: Type.T): BOOLEAN;
(* RETURN "TRUE" if all branded types involved in the definition of t
   have explicity brands.  Print an error message for each type
   that doesn't.  *)

PROCEDURE GenCode(basename: TEXT;
                  t_array: REF ARRAY OF Type.Object; 
                  qid_array: REF ARRAY OF Type.Qid;
                  meth_array: REF ARRAY OF ImportList.MethodList;
                  umethodsTbl: UpdateMethodsTbl.T);
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
