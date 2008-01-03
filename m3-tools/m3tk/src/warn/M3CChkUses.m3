(***************************************************************************)
(*                      Copyright (C) Olivetti 1989                        *)
(*                          All Rights reserved                            *)
(*                                                                         *)
(* Use and copy of this software and preparation of derivative works based *)
(* upon this software are permitted to any person, provided this same      *)
(* copyright notice and the following Olivetti warranty disclaimer are     *) 
(* included in any copy of the software or any modification thereof or     *)
(* derivative work therefrom made by any person.                           *)
(*                                                                         *)
(* This software is made available AS IS and Olivetti disclaims all        *)
(* warranties with respect to this software, whether expressed or implied  *)
(* under any law, including all implied warranties of merchantibility and  *)
(* fitness for any purpose. In no event shall Olivetti be liable for any   *)
(* damages whatsoever resulting from loss of use, data or profits or       *)
(* otherwise arising out of or in connection with the use or performance   *)
(* of this software.                                                       *)
(***************************************************************************)
(**)
(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE M3CChkUses;

IMPORT IntRefTbl, RefList, Word, Text;
IMPORT AST, M3AST_AS, M3AST_SM;
IMPORT ASTWalk, M3CPragma;

IMPORT M3AST_LX_F, M3AST_AS_F, M3AST_SM_F, M3AST_TM_F;

IMPORT SeqM3AST_AS_DECL_REVL;
IMPORT M3Error;

CONST
  DefIdTableSize = 256;

TYPE
  DefId = OBJECT
    used := FALSE; saysUnused := FALSE;
    defId: M3AST_AS.DEF_ID;
  END;

  Import = DefId OBJECT
    usedId: M3AST_AS.USED_ID;
  END;

  DefIdList = REF RefList.T; (* OF DefId *)

REVEAL
  Handle = ASTWalk.Closure BRANDED OBJECT
    interface: BOOLEAN;
    pragmas: M3CPragma.Store;
    ignoreHeaderOrFrom, ignoreFormals, inImported := FALSE;
    ignoreFORvars := FALSE; saysUnused := FALSE;
    table: IntRefTbl.T;
  OVERRIDES
    callback := Node;
  END;


PROCEDURE AddDefId(
    h: Handle;
    defId: M3AST_SM.DEF_ID_UNSET;
    usedId: M3AST_AS.USED_ID := NIL): DefId
    RAISES {}=
  VAR
    d: DefId;
    ra: REFANY;
    l: DefIdList;
  BEGIN
    IF usedId = NIL THEN
      d := NEW(DefId, defId := defId);
    ELSE
      d := NEW(Import, defId := defId, usedId := usedId);
    END;
    (* Assert: not in the table already *)
    IF h.table.get(defId.lx_srcpos, ra) THEN
      (* lx_srcpos clash *)
      l := NARROW(ra, DefIdList);
    ELSE
      l := NEW(DefIdList);
      EVAL h.table.put(defId.lx_srcpos, l);
    END;
    l^ := RefList.Cons(d, l^);
    RETURN d;
  END AddDefId;

PROCEDURE Lookup(
    h: Handle;
    defId: M3AST_AS.DEF_ID;
    VAR (*out*) d: DefId)
    : BOOLEAN
    RAISES {}=
  VAR
    ra: REFANY;
    l: DefIdList;
    dl: RefList.T;
  BEGIN
    (* cant hash references with a copying collector, so hash on
    source position and then use straight comparisions *)
    IF h.table.get(defId.lx_srcpos, ra) THEN
      l := NARROW(ra, DefIdList);
      dl := l^;
      WHILE dl # NIL DO
        d := dl.head;
        IF d.defId = defId THEN RETURN TRUE;
	ELSE
      	  dl := dl.tail;
        END;
      END; (* while *)
    END;
    RETURN FALSE
  END Lookup;

PROCEDURE Node(h: Handle; n: AST.NODE; vm: ASTWalk.VisitMode) RAISES {}=
  VAR
    usedId: M3AST_AS.USED_ID;
    d: DefId;
  BEGIN
    IF vm = ASTWalk.VisitMode.Entry THEN
      IF n.IsA_USED_ID(usedId) THEN
        IF h.ignoreHeaderOrFrom THEN
          (* We are ignoring a unit header (contains Interface_id or Module_id
           and possibly an exports list) or the interface in an unqualified
           import (i.e. FROM interface IMPORT). If we are ignoring a FROM
           interface we set 'ignore' to FALSE so we don't ignore the imported
           ids which follow *)
          IF h.inImported THEN h.ignoreHeaderOrFrom := FALSE END;
          RETURN;
        END;
        VAR
          defId := usedId.sm_def;
        BEGIN
          TYPECASE defId OF
          | NULL, M3AST_AS.Module_id, M3AST_AS.Enum_id,
            M3AST_AS.Field_id, M3AST_AS.METHOD_OVERRIDE_ID =>
              RETURN;
          ELSE
          END;
          IF Lookup(h, defId, d) THEN
            (* This name has already been entered *)
          ELSE
            (* First encounter; put it in the table. If we are dealing with
             an import we must pass the 'usedId' to 'AddDefId' *)
            IF NOT h.inImported THEN usedId := NIL END;
            d := AddDefId(h, defId, usedId);
          END;
          IF NOT h.inImported THEN
            d.used := TRUE;
          END;
        END;
        RETURN;
      END;

      TYPECASE n OF
      | M3AST_AS.UNIT =>
          h.ignoreHeaderOrFrom := TRUE;
      | M3AST_AS.IMPORTED(imported) =>
          h.ignoreHeaderOrFrom := ISTYPE(imported, M3AST_AS.From_import);
          h.inImported := TRUE;
      | M3AST_AS.Import_item(ii) =>
          h.ignoreHeaderOrFrom := ii.as_id # NIL;
      | M3AST_AS.Block =>
          h.ignoreHeaderOrFrom := FALSE;
          h.inImported := FALSE;
      | M3AST_AS.Procedure_type(procType) =>
          TYPECASE procType.sm_def_id OF
          | NULL =>
              h.ignoreFormals := TRUE;
          | M3AST_AS.Proc_id =>
              h.ignoreFormals := FALSE;
          ELSE
              h.ignoreFormals := TRUE;
          END;
      | M3AST_AS.Var_decl_s,
        M3AST_AS.Formal_param =>
          IF UnusedPragma(h, n) THEN h.saysUnused := TRUE END;

      | M3AST_AS.Module_id, M3AST_AS.Enum_id,
        M3AST_AS.Field_id, M3AST_AS.METHOD_OVERRIDE_ID =>
          (* Don't want to take the 'DEF_ID' arm; we don't track these ids *)
      | M3AST_AS.DEF_ID(defId) =>
          TYPECASE n OF
          | M3AST_AS.For_id =>
              IF h.ignoreFORvars THEN RETURN END;
	      IF UnusedPragma(h, n) THEN h.saysUnused := TRUE END;
          ELSE (* drop through *)
          END;
          IF h.ignoreHeaderOrFrom OR h.interface OR
              h.ignoreFormals AND ISTYPE(defId, M3AST_AS.FORMAL_ID) THEN
            RETURN
          END;
          TYPECASE defId OF
          | M3AST_AS.Proc_id(procId) =>
              (* Exported proc ids are not considered to be unused *)
              IF procId.vREDEF_ID.sm_int_def # NIL THEN RETURN END;
          ELSE
          END;
          IF NOT Lookup(h, defId, d) THEN 
	    d := AddDefId(h, defId);
          END;
	  d.saysUnused := h.saysUnused;
     ELSE
     END; (* typecase *)
   ELSE (* vm = ASTWalk.VisitMode.Exit *)
     TYPECASE n OF
     | M3AST_AS.Var_decl_s, M3AST_AS.Formal_param => h.saysUnused := FALSE
     ELSE
     END;
   END
  END Node;

PROCEDURE UnusedPragma(h: Handle; n: AST.NODE):BOOLEAN RAISES {}=
  VAR
    iter := M3CPragma.BeforeNode(h.pragmas, n);
    pragma: M3CPragma.T;
    args: Text.T;
  BEGIN
    WHILE M3CPragma.Next(iter, pragma) AND
        M3CPragma.FollowingNode(pragma) = n DO
      IF M3CPragma.Match(pragma, "UNUSED", args) THEN
        RETURN TRUE;
      END;
    END;
    RETURN FALSE;
  END UnusedPragma;

PROCEDURE NewHandle(cu: M3AST_AS.Compilation_Unit;
  ignoreFORvars := FALSE): Handle RAISES {}=
  BEGIN
    RETURN NEW(Handle, interface := ISTYPE(cu.as_root, M3AST_AS.Interface),
        pragmas := cu.lx_pragmas,
        table := NEW(IntRefTbl.Default).init(DefIdTableSize),
	ignoreFORvars := ignoreFORvars);
  END NewHandle;

PROCEDURE Unused(d: DefId; defId: M3AST_AS.DEF_ID) RAISES {}=
  VAR
    errNode: M3Error.ERROR_NODE;
  BEGIN
    TYPECASE d OF
    | Import(import) =>
        TYPECASE defId OF
        | M3AST_AS.Interface_id(intId) =>
            VAR
              iter := SeqM3AST_AS_DECL_REVL.NewIter(
                  NARROW(intId.sm_spec, M3AST_AS.UNIT_WITH_BODY).as_block.as_decl_s);
              d: M3AST_AS.DECL_REVL;
            BEGIN
              WHILE SeqM3AST_AS_DECL_REVL.Next(iter, d) DO
                IF ISTYPE(d, M3AST_AS.Revelation_s) THEN RETURN END;
              END;
            END;
        ELSE
        END;
        errNode := import.usedId;
    ELSE
      errNode := defId;
    END;
    M3Error.WarnWithId(errNode, "\'%s\' is not used", defId.lx_symrep);
  END Unused;


PROCEDURE CloseHandle(h: Handle) RAISES {}=
  VAR
    i := h.table.iterate();
    key: Word.T;  value: REFANY;
    d: DefId;
    dl: RefList.T;
  BEGIN
    WHILE i.next(key, value) DO
      dl := NARROW(value, DefIdList)^;
      WHILE dl # NIL DO
        d := dl.head;
	IF d.used THEN
	  IF d.saysUnused THEN 
            M3Error.WarnWithId(d.defId, "\'%s\' is NOT unused!",
	        d.defId.lx_symrep);
          END;
        ELSE IF NOT d.saysUnused THEN Unused(d, d.defId) END;
        END;
	dl := dl.tail;
      END;
    END;
  END CloseHandle;


BEGIN

END M3CChkUses.
