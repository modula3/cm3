(* Copyright 1993 Digital Equipment Corporation.                             *)
(* Distributed only by permission.                                           *)
(*                                                                           *)
(* Last modified on Tue Jan 31 14:53:47 PST 1995 by kalsow                   *)
(*      modified on Wed Oct 13 16:45:56 PDT 1993 by heydon                   *)

MODULE ObliqExtension;

IMPORT VertexSizeAnim, Obliq, ObValue, ObLibAnim, GraphVBT;

PROCEDURE VertexSizeAnimLinear(<* UNUSED *> self: Closure;
                                   READONLY args: Obliq.Vals; 
                                            loc : Obliq.Location): Obliq.Val
  RAISES {ObValue.Error} =
  VAR v: GraphVBT.Vertex; newW, newH: REAL; BEGIN
    IF NUMBER(args) # 3 THEN 
      Obliq.RaiseError("VertexSizeAnim.Linear: Expecting 3 arguments", loc);
    END;
    TYPECASE args[0] OF ObLibAnim.ValVertex (oblV) => v := oblV.vertex
    ELSE Obliq.RaiseError("VertexSizeAnim.Linear: First arg not a vertex", loc)
    END;
    newW := FLOAT(Obliq.ToReal(args[1], loc)); (* Obliq uses LONGREAL *)
    newH := FLOAT(Obliq.ToReal(args[2], loc)); (* Obliq uses LONGREAL *)
    VertexSizeAnim.Linear(v, newW, newH);
    RETURN ObValue.valOk
  END VertexSizeAnimLinear;

TYPE
  Closure = Obliq.SysCallClosure OBJECT
  OVERRIDES
    SysCall := VertexSizeAnimLinear;
  END;

BEGIN
  Obliq.PackageSetup();
  Obliq.RegisterSysCall("VertexSizeAnim.Linear", NEW(Closure));
END ObliqExtension.
