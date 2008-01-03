(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: ProcBody.m3                                           *)
(* Last modified on Tue Dec 20 14:28:10 PST 1994 by kalsow     *)

MODULE ProcBody;

IMPORT Text;
IMPORT CG, Host, Target, M3RT, Module;

REVEAL
  T = T_ BRANDED "ProcBody.T" OBJECT
    sibling  : T := NIL;
    children : T := NIL;
  END;

VAR
  cur   : T := NIL;
  head  : T := NIL;
  done  : T := NIL;
  depth : INTEGER := -1;

PROCEDURE Push (t: T) =
  BEGIN
    <* ASSERT (t.parent = NIL) AND (t.sibling = NIL) AND (t.children = NIL) *>
    INC (depth);
    t.level  := depth;
    t.parent := cur;
    IF (cur = NIL) THEN
      (* depth = 0 *)
      t.sibling := head;
      head := t;
    ELSE
      t.sibling := cur.children;
      cur.children := t;
    END;
    cur := t;
  END Push;

PROCEDURE Pop () =
  BEGIN
    cur := cur.parent;
    DEC (depth);
  END Pop;

PROCEDURE Schedule (t: T) =
  BEGIN
    t.sibling := head;
    head := t;
  END Schedule;

PROCEDURE EmitAll (VAR proc_info: INTEGER) =
  VAR
    t           : T;
    base        : INTEGER := 0;
    n_base      : INTEGER;
    n, total    : INTEGER;
    consts      : CG.Var := Module.GlobalData (is_const := TRUE);
  BEGIN
    proc_info := -1;

    (* generate the declarations and bodies *)
    WHILE (head # NIL) DO
      t := head;  head := NIL;  (* grab the guys that are waiting *)
      t := SourceOrder (t);     (* put'em in souce order *)
      EmitDecl (t);             (* generate their declarations *)
      EmitBody (t);             (* generate their bodies & build "done" list *)
    END;

    (* count the linker registrations *)
    t := done;  n := 0;
    WHILE (t # NIL) DO
      IF (t.cg_proc # NIL) AND (t.name # NIL) THEN
        INC (n);
      END;
      t := t.sibling;
    END;

    IF (n > 0) THEN
      (* compute the total lengths of the procedure names *)
      t := done;  total := 0;
      WHILE (t # NIL) DO
        IF (t.cg_proc # NIL) AND (t.name # NIL) THEN
          INC (total, Text.Length (t.name) + 1);
        END;
        t := t.sibling;
      END;

      (* allocate the space we need for names *)
      total := total * Target.Char.size;
      n_base := Module.Allocate (total, Target.Address.align, TRUE, "*proc names*");
      CG.Comment (n_base, TRUE, "procedure names");

      (* allocate the space we need for proc info headers *)
      n := n * M3RT.PI_SIZE + Target.Address.size;
      base := Module.Allocate (n, Target.Address.align, TRUE, "*proc info*");
      CG.Comment (base, TRUE, "procedure table");
      proc_info := base;

      (* generate the procedure names *)
      t := done;  total := 0;
      WHILE (t # NIL) DO
        IF (t.cg_proc # NIL) AND (t.name # NIL) THEN
          CG.Init_chars (n_base + total, t.name, is_const := TRUE);
          INC (total, Target.Char.size * (Text.Length (t.name) + 1));
        END;
        t := t.sibling;
      END;

      (* generate the linker registrations *)
      t := done;  total := 0;  n := proc_info;
      WHILE (t # NIL) DO
        IF (t.cg_proc # NIL) THEN
          IF (t.name # NIL) THEN
            CG.Init_proc (n + M3RT.PI_proc, t.cg_proc, is_const := TRUE);
            CG.Init_var (n + M3RT.PI_name, consts, n_base+total, is_const := TRUE);
            INC (total, Target.Char.size * (Text.Length (t.name) + 1));
          END;
          INC (n, M3RT.PI_SIZE);
        END;
        t := t.sibling;
      END;

    END;
  END EmitAll;

PROCEDURE SourceOrder (t: T): T =
  VAR a, b, c: T;
  BEGIN
    (* reverse the list *)
    a := t;  b := NIL;
    WHILE (a # NIL) DO
      c := a.sibling;
      a.sibling := b;
      b := a;
      a := c;
    END;
    t := b;

    (* recursively reorder the children *)
    WHILE (t # NIL) DO
      t.children := SourceOrder (t.children);
      t := t.sibling;
    END;
    
    RETURN b;
  END SourceOrder;

PROCEDURE EmitDecl (t: T) =
  BEGIN
    WHILE (t # NIL) DO
      t.gen_decl ();
      EmitDecl (t.children);
      t := t.sibling;
    END;
  END EmitDecl;

PROCEDURE EmitBody (t: T) =
  VAR a: T;
  BEGIN
    WHILE (t # NIL) DO
      IF (Host.nested_procs_first) THEN
        EmitBody (t.children);
        IF (t.name # NIL) THEN CG.Comment (-1, FALSE, t.name) END;
        t.gen_body ();
      ELSE
        IF (t.name # NIL) THEN CG.Comment (-1, FALSE, t.name) END;
        t.gen_body ();
        EmitBody (t.children);
      END;

      (* move to the next sibling, but leave this guy on the "done" list *)
      a := t.sibling;
      t.sibling := done;  done := t;
      t := a;
    END;
  END EmitBody;

PROCEDURE Reset () =
  BEGIN
    cur    := NIL;
    head   := NIL;
    done   := NIL;
    depth  := -1;
  END Reset;

BEGIN
END ProcBody.
