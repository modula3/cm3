(* Copyright 1996-2003 John D. Polstra.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgment:
 *      This product includes software developed by John D. Polstra.
 * 4. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * $Id: GlobTree.m3,v 1.1.1.1 2009-04-09 17:01:54 jkrell Exp $ *)

MODULE GlobTree;

IMPORT Glob, RegEx;

TYPE
  (* The base node type.  For simplicity, all nodes have left and right
     child pointers even if they are not used. *)
  Node = T OBJECT
    left, right: T := NIL;
  END;

  (* Internal nodes (operators). *)
  TreeNode = Node OBJECT OVERRIDES
    test := TreeTest;
  END;
  (* The following node types are branded because they are structurally
     identical but we need to be able to distinguish between them with
     TYPECASE. *)
  NotNode = TreeNode BRANDED OBJECT END;
  AndNode = TreeNode BRANDED OBJECT END;
  OrNode = TreeNode BRANDED OBJECT END;

  (* Leaf nodes. *)
  MatchNode = Node OBJECT
    pattern: TEXT;
    options: Glob.MatchOptions;
  OVERRIDES
    test := MatchTest;
  END;

  RegExNode = Node OBJECT
    pattern: RegEx.Pattern;
  OVERRIDES
    test := RegExTest;
  END;

  TrueNode = Node OBJECT OVERRIDES
    test := TrueTest;
  END;

  FalseNode = Node OBJECT OVERRIDES
    test := FalseTest;
  END;

(* Constructors. *)

PROCEDURE Match(pattern: TEXT; options := Glob.MatchOptions{}): T =
  BEGIN
    RETURN NEW(MatchNode, pattern := pattern, options := options);
  END Match;

PROCEDURE RegExMatch(pattern: TEXT): T RAISES {RegEx.Error} =
  BEGIN
    RETURN NEW(RegExNode, pattern := RegEx.Compile(pattern));
  END RegExMatch;

PROCEDURE And(left, right: T): T =
  BEGIN
    IF left = False OR right = False THEN
      RETURN False;
    ELSIF left = True THEN
      RETURN right;
    ELSIF right = True THEN
      RETURN left;
    ELSE
      RETURN NEW(AndNode, left := left, right := right);
    END;
  END And;

PROCEDURE Or(left, right: T): T =
  BEGIN
    IF left = True OR right = True THEN
      RETURN True;
    ELSIF left = False THEN
      RETURN right;
    ELSIF right = False THEN
      RETURN left;
    ELSE
      RETURN NEW(OrNode, left := left, right := right);
    END;
  END Or;

PROCEDURE Not(child: T): T =
  BEGIN
    IF child = True THEN
      RETURN False;
    ELSIF child = False THEN
      RETURN True;
    ELSE
      RETURN NEW(NotNode, left := child);
    END;
  END Not;

(* Leaf evaluators. *)

PROCEDURE MatchTest(self: MatchNode; filename: TEXT): BOOLEAN =
  BEGIN
    RETURN Glob.Match(self.pattern, filename, self.options);
  END MatchTest;

PROCEDURE RegExTest(self: RegExNode; filename: TEXT): BOOLEAN =
  BEGIN
    RETURN RegEx.Execute(self.pattern, filename) >= 0;
  END RegExTest;

PROCEDURE TrueTest(<*UNUSED*> self: T; <*UNUSED*> filename: TEXT): BOOLEAN =
  BEGIN
    RETURN TRUE;
  END TrueTest;

PROCEDURE FalseTest(<*UNUSED*> self: T; <*UNUSED*> filename: TEXT): BOOLEAN =
  BEGIN
    RETURN FALSE;
  END FalseTest;

(* Evaluation of more complex trees. *)

TYPE
  State = { DoingLeft, DoingRight };

  StackElem = REF RECORD
    next: StackElem;
    node: Node;
    state: State;
  END;

PROCEDURE TreeTest(self: TreeNode; filename: TEXT): BOOLEAN =
  VAR
    stack: StackElem := NIL;
    cur: Node;
    state: State;
    val: BOOLEAN;
  PROCEDURE Push(node: Node; state: State) =
    BEGIN
      stack := NEW(StackElem, node := node, state := state, next := stack);
    END Push;
  PROCEDURE Pop(VAR node: Node; VAR state: State) =
    BEGIN
      node := stack.node;
      state := stack.state;
      stack := stack.next;
    END Pop;
  BEGIN
    cur := self;
    LOOP
      (* Descend to the left until we hit bottom. *)
      WHILE cur.left # NIL DO
	Push(cur, State.DoingLeft);
	cur := cur.left;
      END;
      (* Now cur is a leaf node.  Evaluate it. *)
      val := cur.test(filename);
      (* Ascend, propagating the value through operator nodes. *)
      LOOP
	IF stack = NIL THEN
	  RETURN val;
	END;
	Pop(cur, state);
	TYPECASE cur OF
	| NotNode =>
	    val := NOT val;
	| AndNode =>
	    (* If we haven't yet evaluated the right subtree and the partial
	       result is TRUE, descend to the right.  Otherwise the result
	       is already determined to be val. *)
	    IF state = State.DoingLeft AND val THEN
	      Push(cur, State.DoingRight);
	      cur := cur.right;
	      EXIT;
	    END;
	| OrNode =>
	    (* If we haven't yet evaluated the right subtree and the partial
	       result is FALSE, descend to the right.  Otherwise the result
	       is already determined to be val. *)
	    IF state = State.DoingLeft AND NOT val THEN
	      Push(cur, State.DoingRight);
	      cur := cur.right;
	      EXIT;
	    END;
	ELSE
	  (* We only push nodes that have children -- i.e., operator nodes. *)
	  <*ASSERT FALSE *>
	END;
      END;
    END;
  END TreeTest;

BEGIN
  True := NEW(TrueNode);
  False := NEW(FalseNode);
END GlobTree.
