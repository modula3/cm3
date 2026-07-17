(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: TextExpr.m3                                           *)
(* Last modified on Fri Feb 24 16:47:35 PST 1995 by kalsow     *)
(*      modified on Sun Feb 24 04:07:17 1991 by muller         *)

MODULE TextExpr;

IMPORT M3, CG, Expr, ExprRep, M3String, Textt, Type, M3Buf;
IMPORT Target, Module, M3RT, M3WString, RunTyme, Procedure, Word;
IMPORT MSIR, MSIRBuilder, MSIREmit;

TYPE
  P = Expr.T OBJECT
        value8    : M3String.T;
        value32   : M3WString.T;
        cgOffset  : INTEGER := 0;  (* Module.Allocate offset; 0 = not yet allocated *)
      OVERRIDES
        typeOf       := ExprRep.NoType;
        repTypeOf    := ExprRep.NoType;
        check        := ExprRep.NoCheck;
        need_addr    := ExprRep.NotAddressable;
        prep         := ExprRep.NoPrep;
        compile      := Compile;
        prepLV       := ExprRep.NotLValue;
        compileLV    := ExprRep.NotLValueBool;
        prepBR       := ExprRep.NotBoolean;
        compileBR    := ExprRep.NotBoolean;
        evaluate     := ExprRep.Self;
        isEqual      := EqCheck;
        getBounds    := ExprRep.NoBounds;
        isWritable   := ExprRep.IsNever;
        isDesignator := ExprRep.IsNever;
        isZeroes     := IsZeroes;
        genFPLiteral := GenFPLiteral;
        prepLiteral  := PrepLiteral;
        genLiteral   := GenLiteral;
        genLiteralMSIR := GenLiteralMSIR;
        note_write   := ExprRep.NotWritable;
        exprAlign    := ExprRep.ExprAddrAlign;
        compileMSIR      := CompileMSIR;
        compileLValueMSIR := CompileLValueMSIR;
      END;

(* NOTE! These UIDs have nothing to do with the UIDs that are hashes
         and used many places. *)

VAR nextUID  : INTEGER := 0;
TYPE LiteralTable = REF ARRAY OF P;
      (* literals[uid] is the representative TextExpr.T for the uid-th literal.
         The CG allocate offset is in literals[uid](P).cgOffset.
         Use Split8/Split32 on it to extract string content for MSIR. *)

VAR globalConstsCGVar : CG.Var  := NIL;
VAR literals : LiteralTable := NIL;
VAR methodListOffset : INTEGER := -1;
    (* ^Offset w/in global constant area of a list of addresses of
       TextLiteral.T's overrides of Text.T methods. *)

PROCEDURE Reset () =
  BEGIN
    nextUID := 0;
    globalConstsCGVar := NIL;
    methodListOffset := -1;
    (* literals := NIL; *)
    IF literals # NIL THEN
       FOR i := FIRST(literals^) TO LAST(literals^) DO literals[i] := NIL; END;
    END;
  END Reset;

PROCEDURE New8 (value: M3String.T): Expr.T =
  VAR p := NEW (P);
  BEGIN
    ExprRep.Init (p);
    p.value8  := value;
    p.value32 := NIL;
    p.type    := Textt.T;
    p.repType := Textt.T;
    p.checked := TRUE;
    RETURN p;
  END New8;

PROCEDURE New32 (value: M3WString.T): Expr.T =
  VAR p := NEW (P);
  BEGIN
    ExprRep.Init (p);
    p.value8  := NIL;
    p.value32 := value;
    p.type    := Textt.T;
    p.repType := Textt.T;
    p.checked := TRUE;
    RETURN p;
  END New32;

PROCEDURE EqCheck (a: P;  e: Expr.T;  <*UNUSED*> x: M3.EqAssumption): BOOLEAN =
  BEGIN
    TYPECASE e OF
    | NULL => RETURN FALSE;
    | P(b) => RETURN (a.value8 = b.value8) AND (a.value32 = b.value32);
    ELSE      RETURN FALSE;
    END;
  END EqCheck;

PROCEDURE SetUID (p: P): INTEGER =
  VAR
    Header_offset := 0;
    Method_offset := Header_offset + Target.Address.pack;
    Length_offset := Method_offset + Target.Address.pack;
    Chars_offset  := Length_offset + Target.Integer.pack;
  VAR
    uid     : INTEGER;
    len     : INTEGER;
    x       : INTEGER;
    width   : INTEGER;
    cnt     : INTEGER;
  BEGIN
    (* assign this value a unique ID *)
    IF p.value8 # NIL THEN
      width := Target.Char.size;
      len   := M3String.Length (p.value8);
      cnt   := len;
      uid   := M3String.GetUID (p.value8);
      IF uid = M3String.NO_UID THEN
        uid := nextUID;  INC (nextUID);
        M3String.SetUID (p.value8, uid);
      END;
    ELSE
      width := Target.WideCharSize();
      len   := M3WString.Length (p.value32);
      cnt   := - len;
      uid   := M3WString.GetUID (p.value32);
      IF uid = M3WString.NO_UID THEN
        uid := nextUID;  INC (nextUID);
        M3WString.SetUID (p.value32, uid);
      END;
    END;

    (* make sure there's room in the table *)
    IF (literals = NIL) OR (LAST (literals^) < uid) THEN ExpandLiterals () END;

    (* already allocated? — literals[uid] is the representative P with cgOffset set *)
    IF literals[uid] # NIL THEN RETURN uid END;
    literals[uid] := p;   (* register this P as representative *)

    IF (globalConstsCGVar = NIL) THEN
      globalConstsCGVar := Module.GlobalData (is_const := TRUE);
      methodListOffset := BuildMethodList ();
    END;

    (* allocate the variable with room for the trailing null character *)
    x := Module.Allocate (Chars_offset + (len+1) * width,
                           Target.Address.align, TRUE, "*TEXT literal*");
    p.cgOffset := x;

    (* initialize the variable *)
    (*
     * CG.Init_intt (x+Header_offset + M3RT.RH_typecode_offset,
     *               M3RT.RH_typecode_size, M3RT.TEXT_typecode, is_const := TRUE);
     * Write an entire word instead for endian neutrality
     *)
    CG.Init_intt (x+Header_offset,
                  Target.Integer.pack,
                  Word.Shift (M3RT.TEXT_typecode, M3RT.RH_typecode_offset),
                  is_const := TRUE);
    CG.Init_var
      (x+Method_offset, globalConstsCGVar, methodListOffset, is_const := TRUE);
    CG.Init_intt (x+Length_offset, Target.Integer.size, cnt, is_const := TRUE);
    IF (p.value8 # NIL)
      THEN M3String.Init_chars (x+Chars_offset, p.value8, TRUE);
      ELSE M3WString.Init_chars (x+Chars_offset, p.value32, TRUE);
    END;

    RETURN uid;
  END SetUID;

PROCEDURE BuildMethodList (): INTEGER =
  TYPE
    Methods = [RunTyme.Hook.TextLitInfo .. RunTyme.Hook.TextLitGetWideChars];
  VAR offs: INTEGER;
  BEGIN
    IF methodListOffset >= 0 THEN RETURN methodListOffset; END;

    methodListOffset := Module.Allocate (NUMBER (Methods) * Target.Address.size,
                     Target.Address.align, TRUE, "TEXT literal methods");
    offs := methodListOffset;
    FOR i := FIRST (Methods) TO LAST (Methods) DO
      CG.Init_proc (offs, Procedure.CGName (RunTyme.LookUpProc (i)), TRUE);
      INC (offs, Target.Address.size);
    END;

    RETURN methodListOffset;
  END BuildMethodList;

PROCEDURE ExpandLiterals () =
  VAR new: LiteralTable;
  BEGIN
    IF (literals = NIL) THEN
      new := NEW (LiteralTable, 200);
    ELSE
      new := NEW (LiteralTable, 2 * NUMBER (literals^));
      SUBARRAY (new^, 0, NUMBER (literals^)) := literals^;
    END;
    literals := new;
  END ExpandLiterals;

PROCEDURE Compile (p: P; StaticOnly: BOOLEAN) =
  VAR uid := SetUID (p);
  BEGIN
    <* ASSERT NOT StaticOnly *>
(* NOTE^ If ever duplicate text constants are removed as for arrays, this
    will need to change. *)
    CG.Load_addr_of (globalConstsCGVar,
                     literals[uid].cgOffset + Target.Address.pack,
                     Target.Address.align);
  END Compile;

PROCEDURE Split8 (e: Expr.T;  VAR value: M3String.T): BOOLEAN =
  BEGIN
    TYPECASE e OF
    | NULL => RETURN FALSE;
    | P(p) => value := p.value8;  RETURN (p.value8 # NIL);
    ELSE      RETURN FALSE;
    END;
  END Split8;

PROCEDURE Split32 (e: Expr.T;  VAR value: M3WString.T): BOOLEAN =
  BEGIN
    TYPECASE e OF
    | NULL => RETURN FALSE;
    | P(p) => value := p.value32;  RETURN (p.value32 # NIL);
    ELSE      RETURN FALSE;
    END;
  END Split32;

PROCEDURE IsTextExpr (e: Expr.T): BOOLEAN =
  BEGIN
    TYPECASE e OF
    | NULL => RETURN FALSE;
    | P    => RETURN TRUE;
    ELSE      RETURN FALSE;
    END;
  END IsTextExpr;

PROCEDURE Cat (a, b: Expr.T;  VAR c: Expr.T): BOOLEAN =
  VAR sa, sb: M3String.T;  wa, wb: M3WString.T;
  BEGIN
    TYPECASE a OF
    | NULL => RETURN FALSE;
    | P(p) => sa := p.value8;  wa := p.value32;
    ELSE      RETURN FALSE;
    END;
    TYPECASE b OF
    | NULL => RETURN FALSE;
    | P(p) => sb := p.value8;  wb := p.value32;
    ELSE      RETURN FALSE;
    END;
    IF (sa # NIL) AND (sb # NIL) THEN
      c := New8 (M3String.Concat (sa, sb));
    ELSIF (wa # NIL) AND (wb # NIL) THEN
      c := New32 (M3WString.Concat (wa, wb));
    ELSIF (sa # NIL) THEN  (* wb # NIL *)
      c := New32 (M3WString.Concat (M3WString.Add (M3String.ToText (sa)), wb));
    ELSE (*wa # NIL  AND  sb # NIL*)
      c := New32 (M3WString.Concat (wa, M3WString.Add (M3String.ToText (sb))));
    END;
    RETURN TRUE;
  END Cat;

PROCEDURE IsZeroes (<*UNUSED*>p: P;  <*UNUSED*> lhs: BOOLEAN): BOOLEAN = 
  BEGIN
    RETURN FALSE;
  END IsZeroes;

PROCEDURE GenFPLiteral (p: P;  buf: M3Buf.T) =
  BEGIN
    IF (p.value8 # NIL) THEN
      M3Buf.PutText (buf, "TEXT8<");
      M3Buf.PutInt  (buf, M3String.Length (p.value8));
      M3Buf.PutChar (buf, ',');
      M3String.Put  (buf, p.value8);
      M3Buf.PutChar (buf, '>');
    ELSE
      M3Buf.PutText (buf, "TEXT16<");
      (* ^Even though the characters of a literal now could occupy 32 bits, 
         let's keep this string "TEXT16<", to avoid altering fingerprints and 
         undermining pickles written earlier. *) 
      M3Buf.PutInt  (buf, M3WString.Length (p.value32));
      M3Buf.PutChar (buf, ',');
      M3WString.PutLiteral (buf, p.value32);
      M3Buf.PutChar (buf, '>');
    END;
  END GenFPLiteral;

PROCEDURE PrepLiteral (p: P;  <*UNUSED*> type: Type.T;
                              <*UNUSED*> is_const: BOOLEAN) =
  BEGIN
    EVAL SetUID (p);
  END PrepLiteral;

PROCEDURE GenLiteral
  (p: P;  offset: INTEGER;  <*UNUSED*>type: Type.T; is_const: BOOLEAN) =
(* Put a pointer, at 'offset' w/in the 'is_const' static area, to the
   literal 'p', (whose value is located in the global constant area) *)
  VAR uid := SetUID (p);
  BEGIN
    CG.Init_var (offset, globalConstsCGVar,
                 literals[uid].cgOffset + Target.Address.pack,
                 is_const);
  END GenLiteral;

PROCEDURE GenLiteralMSIR (p: P;  <*UNUSED*> ft: MSIR.T): MSIR.Value =
(* Static/global-initializer constant.  This expr's CompileMSIR returns a pure
   ConstTextLit (no IR); use it, else NIL. *)
  VAR cv := CompileMSIR (p);
  BEGIN
    IF cv # NIL AND MSIR.GetValueKind (cv) = MSIR.ValueKind.ConstTextLit THEN RETURN cv END;
    RETURN NIL;
  END GenLiteralMSIR;

PROCEDURE CompileMSIR (p: P): MSIR.Value =
  VAR uid: INTEGER;  chars: TEXT;  cnt: INTEGER;
  BEGIN
    IF NOT MSIREmit.IsEnabled () THEN
      RETURN MSIR.ConstNil (MSIR.TGcRef (MSIR.TVoid ()));
    END;
    (* A text literal is a pure compile-time constant (SetUID + ConstTextLit emit
       no instructions), so build it even outside a proc — const record
       initializers of module globals are captured at global-declaration time
       (Variable.DeclareGlobalMSIR), where InProc is FALSE. *)
    (* SetUID registers the literal in the CG per-module table (deduplication)
       and returns the uid used to name the LLVM global @textlit_<uid>. *)
    uid := SetUID (p);
    IF p.value8 # NIL THEN
      chars := M3String.ToText (p.value8);
      cnt   := M3String.Length (p.value8);
    ELSE
      chars := M3WString.ToLiteral (p.value32);
      cnt   := - M3WString.Length (p.value32);
    END;
    RETURN MSIR.ConstTextLit (uid, chars, cnt);
  END CompileMSIR;

PROCEDURE CompileLValueMSIR (p: P): MSIR.Value =
  (* A text literal has no addressable storage, but READONLY formal passing
     requires a pointer.  Spill the literal reference into a local alloca
     and return that address. *)
  VAR
    textVal : MSIR.Value;
    textT   : MSIR.T;
    slot    : MSIR.Value;
    b       : MSIR.Block;
  BEGIN
    IF NOT MSIREmit.IsEnabled () OR NOT MSIRBuilder.InProc () THEN
      RETURN NIL;
    END;
    textVal := CompileMSIR (p);
    IF textVal = NIL THEN RETURN NIL END;
    textT := MSIR.TGcRef (MSIR.TVoid ());
    b     := MSIRBuilder.CurrentBlock ();
    slot  := MSIR.BuildAlloca (b, "", textT);
    MSIR.BuildStore (b, textVal, slot);
    RETURN slot;
  END CompileLValueMSIR;

PROCEDURE LiteralCount (): INTEGER =
  BEGIN RETURN nextUID END LiteralCount;

PROCEDURE LiteralExpr (uid: INTEGER): Expr.T =
  BEGIN
    IF literals = NIL OR uid > LAST(literals^) THEN RETURN NIL END;
    RETURN literals[uid];
  END LiteralExpr;

BEGIN
END TextExpr.
