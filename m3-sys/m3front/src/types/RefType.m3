(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: RefType.m3                                            *)
(* Last modified on Tue May 23 15:26:35 PDT 1995 by kalsow     *)
(*      modified on Thu Dec  5 17:20:18 PST 1991 by muller     *)

MODULE RefType;

IMPORT M3, M3ID, CG, Token, Type, TypeRep, Scanner, ObjectType, Target;
IMPORT Null, Reff, Addr, Error, Module, M3Buf, Brand;
IMPORT Revelation, OpenArrayType, TipeMap, TipeDesc, TypeFP;
IMPORT ProcType, ObjectAdr, Word, M3RT;
IMPORT RTIO, RTParams;
IMPORT MSIR, MSIRBuilder, MSIREmit, RecordType;

VAR debug := FALSE;

TYPE
  P = Type.T BRANDED "RefType.T"OBJECT
        brand      : Brand.T;
        target     : Type.T;
        isTraced   : BOOLEAN;
        user_name  : TEXT;
      OVERRIDES
        check       := Check;
        no_straddle := TypeRep.AddrNoStraddle;
        isEqual     := EqualChk;
        isSubtype   := Subtyper;
        compile     := Compiler;
        initCost    := InitCoster;
        initValue   := TypeRep.InitToZeros;
        mapper      := TypeRep.GenRefMap;
        gen_desc    := GenDesc;
        fprint      := FPrinter;
      END;

VAR root := M3ID.NoID;

(* EXPORTED *)
PROCEDURE Parse (): Type.T =
  VAR brand: Brand.T := NIL;  traced: BOOLEAN := TRUE;  super: Type.T := NIL;
  BEGIN
    IF (Scanner.cur.token = Token.T.tUNTRACED) THEN
      Scanner.GetToken (); (* UNTRACED *)
      IF (Scanner.cur.token = Token.T.tIDENT) THEN
        IF root = M3ID.NoID THEN root := M3ID.Add ("ROOT"); END;
        IF (Scanner.cur.id # root) THEN
          Error.ID (Scanner.cur.id, "expected UNTRACED ROOT");
        END;
        Scanner.GetToken (); (* IDENT *)
        super := ObjectAdr.T;
        IF (Scanner.cur.token # Token.T.tOBJECT)
          AND (Scanner.cur.token # Token.T.tBRANDED) THEN RETURN super END;
      END;
      traced := FALSE;
    END;
    brand := Brand.Parse ();
    IF (Scanner.cur.token = Token.T.tREF) THEN
      IF (super # NIL) THEN Error.Msg ("expected OBJECT declaration") END;
      Scanner.GetToken (); (* REF *)
      RETURN New (Type.Parse (), traced, brand);
    ELSE (* must be an object type *)
      IF (super = NIL) AND (NOT traced) THEN
        Error.Msg ("expected UNTRACED ROOT OBJECT");
      END;
      RETURN ObjectType.Parse (super, traced, brand);
    END;
  END Parse;

(* EXPORTED *)
PROCEDURE New (target: Type.T;  traced: BOOLEAN;  brand: Brand.T): Type.T =
  VAR p: P;
  BEGIN
    p := NEW (P);
    TypeRep.Init (p, Type.Class.Ref);
    p.isTraced   := traced;
    p.brand      := brand;
    p.target     := target;
    p.user_name  := NIL;

    IF debug THEN
      RTIO.PutText ("RefType.New:");
      RTIO.PutRef (p);
      RTIO.PutText (" target:");
      RTIO.PutRef (target);
      RTIO.PutText ("\n");
      RTIO.Flush ();
    END;

    RETURN p;
  END New;

PROCEDURE Reduce (t: Type.T): P =
  BEGIN
    LOOP
      t := Type.Strip (t); (* StripPacked? *)
      IF (t = NIL) THEN RETURN NIL END;
      IF (t.info.class = Type.Class.Ref) THEN RETURN t END;
      IF (t.info.class # Type.Class.Opaque) THEN RETURN NIL END;
      t := Revelation.LookUp (t);
    END;
  END Reduce;

(* EXPORTED *)
PROCEDURE Is (t: Type.T): BOOLEAN =
  BEGIN
    RETURN (Reduce (t) # NIL);
  END Is;
(* EXPORTED *)

PROCEDURE IsBranded (t: Type.T): BOOLEAN =
  VAR p := Reduce (t);
  BEGIN
    RETURN (p # NIL) AND (p.brand # NIL);
  END IsBranded;

(* EXPORTED *)
PROCEDURE Split (t: Type.T;  VAR target: Type.T): BOOLEAN =
  VAR p := Reduce (t);
  BEGIN
    IF (p = NIL) THEN RETURN FALSE END;
    target := p.target;
    RETURN TRUE;
  END Split;

(* EXPORTED *)
PROCEDURE ReduceToRef (t: Type.T): Type.T =
  BEGIN RETURN Reduce (t) END ReduceToRef;

(* Externally dispatched-to: *)
PROCEDURE Check (p: P) =
  VAR
    targetType: Type.T;
    targetAlign : INTEGER;
    hash: INTEGER := 839;
    targetInfo: Type.Info;
    cs := M3.OuterCheckState;
    targetIsTraced : BOOLEAN;
  BEGIN
    Brand.Check (p.brand, p, hash, cs);

    targetType := Type.Strip (p.target) (* Remove named. *);
    IF targetType # NIL THEN
      IF targetType.info.class = Type.Class.Packed THEN
        targetType := Type.StripPacked (targetType);
      END;
      hash := Word.Plus (Word.Times (hash, 43), ORD (targetType.info.class));
    END;

    p.info.size      := Target.Address.size;
    p.info.min_size  := Target.Address.size;
    p.info.alignment := Target.Address.align;
    p.info.mem_type  := CG.Type.Addr;
    p.info.stk_type  := CG.Type.Addr;
    p.info.class     := Type.Class.Ref;
    p.info.isTraced  := p.isTraced;
    p.info.isEmpty   := FALSE;
    p.info.isSolid   := TRUE;
    p.info.hash      := hash;

    targetAlign := Target.Word8.align;
    targetIsTraced := FALSE;
    INC (Type.recursionDepth); (*------------------------------------*)
      p.checked := TRUE;
      IF (p.target # NIL) THEN
        p.target := Type.CheckInfo (p.target, targetInfo);
        targetAlign := targetInfo.alignment;
        targetIsTraced := targetInfo.isTraced;
      END;
    DEC (Type.recursionDepth); (*------------------------------------*)

    p.info.addr_align := MAX (targetAlign, Target.Word8.align);
      (* ^Target's type-alignment could be < 8, if the target has
          packed elements or fields, but a pointer can point only
          to a whole byte. *)
    
    IF (NOT p.isTraced) AND (targetIsTraced) AND Module.IsSafe() THEN
      Error.Msg ("Unsafe: untraced ref type to a traced type (2.2.7).");
    END;
 (* EVAL Type.StraddleFreeScalars (p.target, 0, IsEltOrField := FALSE); *)
(* CHECK: ^Why is this here? rodney.m.bates@acm.org.
     1) It appears StraddleFreeScalars and all its many and recursive
        overrides are side-effect-free functions, so why EVAL?
     2) With an offset of 0, how could it fail?
*)
  END Check;

(* Externally dispatched-to: *)
PROCEDURE Compiler (p: P) =
  VAR typeid: CG.TypeUID;
      user_name: TEXT;
      target_typename := M3ID.NoID;
  BEGIN
    Type.Compile (p.target);
    typeid := Type.GlobalUID (p);
    user_name := p.user_name;
    IF user_name # NIL THEN
      target_typename := M3ID.Add (user_name);
    END;
    CG.Declare_pointer (typeid, Type.GlobalUID (p.target),
                        Brand.ToText (p.brand), p.isTraced, target_typename);
    IF user_name # NIL THEN
      CG.Declare_typename (typeid, target_typename);
    END;
  END Compiler;

(* EXPORTED *)
PROCEDURE NoteRefName (t: Type.T;  name: TEXT) =
  VAR p := Reduce (t);
  BEGIN
    IF (p # NIL) THEN p.user_name := name; END;
  END NoteRefName;

(* EXPORTED *)
PROCEDURE InitTypecell (t: Type.T;  offset, prev: INTEGER) =
  TYPE TKind = M3RT.TypeKind;
  CONST Kind = ARRAY BOOLEAN OF TKind { TKind.Ref, TKind.Array};
  VAR
    p           : P := t;
    brand       := Brand.Compile (p.brand);
    type_map    := GenTypeMap (p, refs_only := FALSE);
    gc_map      := GenTypeMap (p, refs_only := TRUE);
    type_desc   := GenTypeDesc (p);
    initProc    := GenInitProc (p);
    dims        : INTEGER;
    targetSize  : INTEGER;
    elemSize    : INTEGER;
    ta          : Type.T;
    isz         : INTEGER := Target.Integer.size;
    name_offs   : INTEGER := 0;
    fp          := TypeFP.FromType (p);
    globals     := Module.GlobalData (is_const := FALSE);
    consts      := Module.GlobalData (is_const := TRUE);
    targetInfo  : Type.Info;
  BEGIN
    EVAL Type.CheckInfo (p.target, targetInfo);
    ta := Type.Base (p.target);

    dims := OpenArrayType.OpenDepth (ta);
    IF (dims = 0) THEN (* Not an open array. *)
      targetSize := targetInfo.size;
      elemSize := 8 (*Dead*);
    ELSE (* target is an open array.  targetSize := dope size. *)
      targetSize := Target.Address.size (* Size of elements pointer. *);
      WITH ia = Target.Integer.align DO
        targetSize
          := ((targetSize + ia - 1) DIV ia) * ia (* Padding, ahead of shape *);
        INC (targetSize, Target.Integer.size * dims) (* The shape words. *);
      END;
      WITH aa = p.info.addr_align DO
        targetSize
          := ((targetSize + aa - 1) DIV aa) * aa (* Padding, ahead of elements *);
      END;
      elemSize := OpenArrayType.EltPack (ta);
      IF elemSize < Target.Byte THEN (* Sub-byte elements, 1, 2, or 4 bits. *)
        <* ASSERT Target.Byte MOD elemSize = 0 *>
        elemSize := Target.Byte;
(* FIXME: This is a quick hack for open arrays of element bitsize 1, 2, or 4.
          The RTS only knows byte counts for element sizes.  Until RTS can be
          fixed to understand bit sizes, this will at least make things work,
          at the cost of over-allocating a full byte for every element. *)
      ELSE
        <* ASSERT elemSize MOD Target.Byte = 0 *>
      END;
    END;

    IF (p.user_name # NIL) THEN
      name_offs := CG.EmitText (p.user_name, is_const := TRUE);
    END;

    (* generate my Type cell info *)
    CG.Init_intt   (offset + M3RT.TC_selfID, isz, Type.GlobalUID (p), FALSE);
    FOR i := FIRST (fp.byte) TO LAST (fp.byte) DO
      CG.Init_intt (offset + M3RT.TC_fp + i * 8, 8, fp.byte[i], FALSE);
    END;
    CG.Init_intt (offset + M3RT.TC_traced, 8, ORD (p.isTraced), FALSE);
    CG.Init_intt (offset + M3RT.TC_kind, 8, ORD (Kind[dims > 0]), FALSE);
    CG.Init_intt
      (offset + M3RT.TC_dataAlignment, 8, p.info.addr_align DIV Target.Byte, FALSE);
   (* ^VSee comments in RT0.i3, regarding dataAlignment and dataSize. *)

    CG.Init_intt
      (offset + M3RT.TC_dataSize, isz, targetSize DIV Target.Byte, FALSE);
    IF (type_map >= 0) THEN
      CG.Init_var (offset + M3RT.TC_type_map, consts, type_map, FALSE);
    END;
    IF (gc_map >= 0) THEN
      CG.Init_var (offset + M3RT.TC_gc_map, consts, gc_map, FALSE);
    END;
    IF (type_desc >= 0) THEN
      CG.Init_var (offset + M3RT.TC_type_desc, consts, type_desc, FALSE);
    END;
    IF (initProc # NIL) THEN
      CG.Init_proc (offset + M3RT.TC_initProc, initProc, FALSE);
    END;
    IF (brand >= 0) THEN
      CG.Init_var (offset + M3RT.TC_brand, consts, brand, FALSE);
    END;
    IF (p.user_name # NIL) THEN
      CG.Init_var (offset + M3RT.TC_name,  consts, name_offs, FALSE);
    END;
    IF (prev # 0) THEN
      CG.Init_var (offset + M3RT.TC_next,  globals, prev, FALSE);
    END;

    IF (dims > 0) THEN
      (* REF ARRAY specific extensions to the typecell *)
      CG.Init_intt (offset + M3RT.ATC_nDimensions, isz, dims, FALSE);
      CG.Init_intt
        (offset + M3RT.ATC_elementSize, isz, elemSize DIV Target.Byte, FALSE);
    END;

  END InitTypecell;

PROCEDURE InitTypecellMSIR (t: Type.T) =
  VAR
    info     : Type.Info;
    rinfo    : Type.Info;
    r        : Type.T;
    ta       : Type.T;
    dims     : INTEGER;
    elemSize : INTEGER;
    dopeSize : INTEGER;
  BEGIN
    IF NOT MSIREmit.IsEnabled () THEN RETURN END;
    EVAL Type.CheckInfo (t, info);
    IF NOT Split (t, r) THEN RETURN END;
    r := Type.StripPacked (r);
    EVAL Type.CheckInfo (r, rinfo);
    ta   := Type.Base (r);
    dims := OpenArrayType.OpenDepth (ta);
    IF dims > 0 THEN
      (* ATC: compute dope-vector size and element size exactly as InitTypecell does *)
      dopeSize := Target.Address.size;
      WITH ia = Target.Integer.align DO
        dopeSize := ((dopeSize + ia - 1) DIV ia) * ia;
        INC (dopeSize, Target.Integer.size * dims);
      END;
      WITH aa = info.addr_align DO
        dopeSize := ((dopeSize + aa - 1) DIV aa) * aa;
      END;
      elemSize := OpenArrayType.EltPack (ta);
      IF elemSize < Target.Byte THEN elemSize := Target.Byte END;
      EVAL MSIRBuilder.TypeDescValueForRefArray (
             t,
             dopeSize DIV Target.Char.size,
             rinfo.alignment DIV Target.Byte,
             dims,
             elemSize DIV Target.Byte,
             info.isTraced);
    ELSE
      EVAL MSIRBuilder.TypeDescValueForRef (
             t,
             rinfo.size DIV Target.Char.size,
             rinfo.alignment DIV Target.Byte,
             info.isTraced);
    END;
  END InitTypecellMSIR;

PROCEDURE GenInitProcMSIR (t: Type.T;  desc: MSIR.TypeDesc) =
(* MSIR analogue of GenInitProc: emit an internal helper procedure that
   applies the referent's language-defined initial values to a freshly
   allocated (zeroed) object, and register it as TC_initProc so RTAllocator
   (NewTraced / GetOpenArray -> InitArray) runs it — the same runtime
   contract as the C backend.  Invoked from MSIRBuilder's
   TypeDescValueForRef* new-desc path (SetRefInitProcGen callback), so every
   desc creator triggers generation exactly once per module.  The recursive
   type walk is RecordType.GenInitMSIR (records, fixed arrays, scalar
   subranges); open-array referents loop over the flat element region using
   the shape in the dope.  Helper-proc plumbing mirrors
   ObjectType.GenInitProcMSIR. *)
  VAR
    r        : Type.T;
    procName : TEXT;
    m        : MSIR.Module;
    proc     : MSIR.Proc;
    entry    : MSIR.Block;
    ptrT     := MSIR.TPtr (MSIR.TVoid ());
    intT     := MSIR.TI (Target.Integer.size);
    objParam : MSIR.Value;
    ndims    : INTEGER;
  BEGIN
    IF NOT MSIREmit.IsEnabled () THEN RETURN END;
    IF NOT Split (t, r) THEN RETURN END;
    r := Type.StripPacked (r);
    IF Type.InitCost (r, TRUE) <= 0 THEN RETURN END;
    m := MSIREmit.CurrentModule ();
    IF m = NIL THEN RETURN END;

    ndims := OpenArrayType.OpenDepth (Type.Base (r));
    IF ndims > 0 THEN
      (* Byte-aligned elements only; sub-byte element packs cannot carry
         non-zero defaults reachable here (records are byte-aligned and a
         packed scalar subrange excluding 0 in a NEW'd open array is not
         expressible without a record wrapper). *)
      IF OpenArrayType.EltPack (Type.Base (r)) MOD Target.Byte # 0 THEN
        RETURN;
      END;
    END;

    (* Module-prefixed so LLSymbol's ContainsDunder check skips re-prefixing. *)
    procName := MSIR.ModuleName (m) & "__" & Type.LinkName (t, "_INIT");
    MSIR.SetTypeDescInitProc (desc, procName);

    proc := MSIR.NewProc (procName,
              ARRAY OF MSIR.Param{
                MSIR.Param{name := "obj", type := ptrT,
                           mode := MSIR.ParamMode.ByValue}},
              MSIR.TVoid ());
    MSIR.ProcSetLinkage (proc, MSIR.Linkage.Internal);
    entry := MSIR.NewBlock ("entry", ARRAY OF MSIR.BlockParam{});
    MSIR.ProcAddBlock (proc, entry);

    VAR savedProc  := MSIRBuilder.CurrentProc ();
        savedBlock := MSIRBuilder.CurrentBlock ();
    BEGIN
      MSIRBuilder.BeginHelperProc (proc, entry);
      objParam := MSIR.ProcParam (proc, 0);

      IF ndims > 0 THEN
        (* Open-array referent: init each element of the flat region.
           Layout: data ptr at byte 0; sizes at AP + k*IP, k in [0..ndims). *)
        VAR
          ta       := Type.Base (r);
          eltType  := ta;
          eltPack  := OpenArrayType.EltPack (ta);
          apBytes  := Target.Address.size DIV Target.Byte;
          ipBytes  := Target.Integer.size DIV Target.Byte;
          b        := MSIRBuilder.CurrentBlock ();
          dataPtr  := MSIR.BuildLoad (b, "", ptrT,
                        MSIRBuilder.BuildPtrByteOff (b, "", objParam, 0));
          totalV   : MSIR.Value := NIL;
          idxA     : MSIR.Value;
          hdrB, bodyB, exitB: MSIR.Block;
        BEGIN
          FOR k := 1 TO ndims DO
            EVAL OpenArrayType.Split (eltType, eltType);
          END;
          FOR k := 0 TO ndims - 1 DO
            b := MSIRBuilder.CurrentBlock ();
            VAR dimV := MSIR.BuildLoad (b, "", intT,
                          MSIRBuilder.BuildPtrByteOff (b, "", objParam,
                                                       apBytes + ipBytes * k));
            BEGIN
              IF totalV = NIL
                THEN totalV := dimV;
                ELSE totalV := MSIR.BuildIMul (b, "", totalV, dimV);
              END;
            END;
          END;
          hdrB  := MSIRBuilder.NewBlock ("tcinit.hdr");
          bodyB := MSIRBuilder.NewBlock ("tcinit.body");
          exitB := MSIRBuilder.NewBlock ("tcinit.done");
          b     := MSIRBuilder.CurrentBlock ();
          idxA  := MSIR.BuildAlloca (b, "", intT);
          MSIR.BuildStore (b, MSIR.ConstInt (intT, 0), idxA);
          MSIR.BuildBr (MSIRBuilder.CurrentBlock (), hdrB,
                        ARRAY OF MSIR.Value{});
          MSIRBuilder.SetCurrentBlock (hdrB);
          VAR idx  := MSIR.BuildLoad (hdrB, "", intT, idxA);
              cond := MSIR.BuildICmp (hdrB, "", MSIR.CmpPred.Slt, idx, totalV);
          BEGIN
            MSIR.BuildCondBr (hdrB, cond, bodyB, ARRAY OF MSIR.Value{},
                              exitB, ARRAY OF MSIR.Value{});
            MSIRBuilder.SetCurrentBlock (bodyB);
            VAR off     := MSIR.BuildIMul (bodyB, "", idx,
                             MSIR.ConstInt (intT, eltPack DIV Target.Byte));
                eltAddr := MSIR.BuildGepByte (bodyB, "", dataPtr, off);
            BEGIN
              RecordType.GenInitMSIR (eltType, eltAddr);
            END;
            VAR b2 := MSIRBuilder.CurrentBlock ();
            BEGIN
              MSIR.BuildStore (b2,
                MSIR.BuildIAdd (b2, "", idx, MSIR.ConstInt (intT, 1)), idxA);
              MSIR.BuildBr (MSIRBuilder.CurrentBlock (), hdrB,
                            ARRAY OF MSIR.Value{});
            END;
          END;
          MSIRBuilder.SetCurrentBlock (exitB);
        END;
      ELSE
        (* Record / fixed array / scalar referent. *)
        RecordType.GenInitMSIR (r, objParam);
      END;

      MSIR.BuildRet (MSIRBuilder.CurrentBlock (), NIL);
      MSIRBuilder.EndHelperProc ();
      IF savedProc # NIL THEN
        MSIRBuilder.BeginHelperProc (savedProc, savedBlock);
      END;
    END;
    MSIR.ModuleAddProc (m, proc);
  END GenInitProcMSIR;

PROCEDURE GenTypeMap (p: P;  refs_only: BOOLEAN): INTEGER =
  (* generate my "TypeMap" (called by the garbage collector) *)
  BEGIN
    TipeMap.Start ();
    Type.GenMap (p.target, 0, -1, refs_only);
    RETURN TipeMap.Finish ("type map for ", Type.Name (p));
  END GenTypeMap;

PROCEDURE GenTypeDesc (p: P): INTEGER =
  (* generate my "TypeDesc" (called by the pickle machinery) *)
  BEGIN
    IF NOT p.isTraced THEN RETURN -1 END;
    TipeDesc.Start ();
    Type.GenDesc (p.target);
    RETURN TipeDesc.Finish ("type description for ", Type.Name (p));
  END GenTypeDesc;

PROCEDURE GenInitProc (p: P): CG.Proc =
  VAR name: TEXT;  proc: CG.Proc;  ref: CG.Var;
  BEGIN
    IF Type.InitCost (p.target, TRUE) <= 0 THEN RETURN NIL END;

    (* generate the procedure body *)

    CG.Gen_location (p.origin);
    name := Type.LinkName (p, "_INIT");
    CG.Comment (-1, FALSE, name & " (RefType)");
    Scanner.offset := p.origin;
    CG.Gen_location (p.origin);
    proc := CG.Declare_procedure (M3ID.Add (name), 1, CG.Type.Void,
                                  lev := 0, cc := Target.DefaultCall,
                                  exported := FALSE, parent := NIL);
    ref := CG.Declare_param (M3ID.NoID, Target.Address.size,
                             Target.Address.align, CG.Type.Addr,
                             Type.GlobalUID (p),
                             in_memory := FALSE, up_level := FALSE,
                             f := CG.Always);
    CG.Begin_procedure (proc);
    

    (* initialize the referent *)
    CG.Load_addr (ref, 0, p.info.addr_align);
    Type.InitValue (p.target, TRUE);

    CG.Exit_proc (CG.Type.Void);
    CG.End_procedure (proc);
    RETURN proc;
  END GenInitProc;

(* Externally dispatched-to: *)
PROCEDURE EqualChk (a: P;  t: Type.T;  x: Type.Assumption): BOOLEAN =
  VAR b: P := t;
  BEGIN
    RETURN (a.isTraced = b.isTraced)
       AND Brand.Equal (a.brand, b.brand)
       AND ((a.target = NIL AND b.target = NIL)
             OR Type.IsEqual (a.target, b.target, x));
  END EqualChk;

(* Externally dispatched-to: *)
PROCEDURE Subtyper (a: P;  b: Type.T): BOOLEAN =
  BEGIN
    IF Type.IsEqual (a, b, NIL) THEN RETURN TRUE END;

    IF Type.IsEqual (a, Null.T, NIL) THEN
      RETURN Type.IsSubtype (b, Reff.T)
          OR Type.IsSubtype (b, Addr.T)
          OR ProcType.Is (b);
    END;

    RETURN ((a.isTraced) AND Type.IsEqual (b, Reff.T, NIL))
        OR ((NOT a.isTraced) AND Type.IsEqual (b, Addr.T, NIL));
  END Subtyper;

(* Externally dispatched-to: *)
PROCEDURE InitCoster (<*UNUSED*>p: P;  zeroed: BOOLEAN): INTEGER =
  BEGIN
    IF NOT zeroed THEN RETURN 1 ELSE RETURN 0 END;
  END InitCoster;

(* Externally dispatched-to: *)
PROCEDURE GenDesc (p: P) =
  BEGIN
    IF Type.IsEqual (p, Reff.T, NIL) THEN
      EVAL TipeDesc.AddO (TipeDesc.Op.Refany, p);
    ELSIF Type.IsEqual (p, Addr.T, NIL) THEN
      EVAL TipeDesc.AddO (TipeDesc.Op.Address, p);
    ELSIF Type.IsEqual (p, Null.T, NIL) THEN
      EVAL TipeDesc.AddO (TipeDesc.Op.Null, p);
    ELSE
      TypeRep.GenRefDesc (p);
    END;
  END GenDesc;

(* Externally dispatched-to: *)
PROCEDURE FPrinter (p: P;  VAR x: M3.FPInfo) =
  BEGIN
    IF Type.IsEqual (p, Reff.T, NIL) THEN
      x.tag := "$refany";
      x.n_nodes := 0;
    ELSIF Type.IsEqual (p, Addr.T, NIL) THEN
      x.tag := "$address";
      x.n_nodes := 0;
    ELSIF Type.IsEqual (p, Null.T, NIL) THEN
      x.tag := "$null";
      x.n_nodes := 0;
    ELSE
      M3Buf.PutText (x.buf, "REF");
      IF (NOT p.isTraced) THEN M3Buf.PutText (x.buf, "-UNTRACED") END;
      Brand.GenFPrint (p.brand, x);
      x.n_nodes  := 1;
      x.nodes[0] := p.target;
    END;
  END FPrinter;

BEGIN
  debug := RTParams.IsPresent ("m3front-debug-reftype");
  MSIRBuilder.SetRefInitProcGen (GenInitProcMSIR);
END RefType.
