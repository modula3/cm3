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
(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE M3CPragma;

IMPORT Text;
IMPORT M3AST_AS;
IMPORT M3CSrcPos;

(* The following are used by the parser to build up a pragma store. 'NewStore'
creates a new pragma store and each pragma encountered is added with the
'AddToStore' procedure. Pragmas must be added in source position order. Pragmas
must start with "<*" and end with "*>" *)

TYPE
  Store <: REFANY;
  T <: REFANY;

PROCEDURE NewStore(): Store RAISES {};
(* Create new pragma store *)

PROCEDURE AddToStore(
    body: Text.T;
    pos: M3CSrcPos.T;
    precedingNode: M3AST_AS.SRC_NODE;
    VAR store: Store)
    : T
    RAISES {};
(* Add a new pragma to a store, giving its body, position and the preceding
source node *)

PROCEDURE AddFollowingNode(
    followingNode: M3AST_AS.SRC_NODE;
    store: Store)
    RAISES {};
(* Called after a pragma (or pragmas) has been added by 'AddToStore' and
another source node is encountered. Marks all the pragmas after the last
source node as having the given 'followingNode' *)

PROCEDURE AddPrecedingStmOrDecl(
    followingStmOrDecl: M3AST_AS.SRC_NODE;
    store: Store)
    RAISES {};
(*  Called after a pragma (or pragmas) has been added by 'AddToStore' and
the end of a statement or declaration is encountered. Marks all the pragmas
after the last source node as being after the given 'stmOrDecl' *)


(* The pragmas in a pragma store can be iterated using 'NewIter' and 'Next'.
The iterator works even while the store is being built up using 'AddToStore' *)

TYPE
  Iter <: REFANY;

PROCEDURE NewIter(ps: Store; after := M3CSrcPos.Null): Iter RAISES {};
(* Return iterator for pragmas. They will be iterated in ascending positional
order. If 'after' is not null only the pragmas whose position is greater than
'after' will be iterated *)

PROCEDURE Next(VAR iter: Iter; VAR t: T): BOOLEAN RAISES {};
(* 'Next' returns FALSE if 'iter' is NIL *)


(* The following enquiry functions can be used on all pragmas *)

PROCEDURE Position(t: T): M3CSrcPos.T RAISES {};
(* Source position of pragma *)

PROCEDURE Body(t: T): Text.T RAISES {};
(* Text of pragma. Includes opening and closing brackets *)

PROCEDURE Match(t: T; keyword: Text.T; VAR args: Text.T): BOOLEAN RAISES {};
(* If the first word in 't' is 'keyword' returns TRUE and sets 'args' to the
remaining words in 't'. 'args' will have no leading or trailing whitespace and
can be NIL if 'keyword' is the only word in 't'.
  If the first word in 't' is not 'keyword' returns FALSE and leaves args
untouched *)


(* A pragma has a hook field from which data can be hung. It is intended for
use by the code which processes the pragma. A pragma with a NIL hook field
is assumed to be unprocessed so any code which uses a pragma e.g. binds it to
a node and processes its contents, should set the hook to a non NIL value to
let everyone know the pragma has been dealt with.
  Pragmas with NIL hook fields at the end of compilation are assumed to be
errors and a warning message will result. *)

PROCEDURE Hook(t: T): REFANY RAISES {};
PROCEDURE SetHook(t: T; hook: REFANY) RAISES {};


PROCEDURE BeforeNode(
    store: Store;
    node: M3AST_AS.SRC_NODE)
    : Iter
    RAISES {};
(* Returns an iterator which will iterate the pragmas in 'store' starting at
the first which is immediately before the given node. Returns NIL if there are
no pragmas immediately before 'node' *)

PROCEDURE AfterNode(
    store: Store;
    node: M3AST_AS.SRC_NODE)
    : Iter
    RAISES {};
(* Returns an iterator which will iterate the pragmas in 'store' starting at
the first which is immediately after the given node. Returns NIL if there are
no pragmas immediately after 'node' *)

PROCEDURE AfterStmOrDecl(
    store: Store;
    stmOrDecl: M3AST_AS.SRC_NODE)
    : Iter
    RAISES {};
(* Returns an iterator which will iterate the pragmas in 'store' starting at
the first which is immediately after the given statement, declaration or
revelation. Returns NIL if there are no pragmas immediately after 'stmOrDecl'
*)

PROCEDURE PrecedingNode(t: T): M3AST_AS.SRC_NODE RAISES {};
(* Return the node which immediately precedes the given pragma, or NIL if there
is no such node *)

PROCEDURE FollowingNode(t: T): M3AST_AS.SRC_NODE RAISES {};
(* Return the node which immediately follows the given pragma, or NIL if there
is no such node *)
 
PROCEDURE PrecedingStmOrDecl(t: T): M3AST_AS.SRC_NODE RAISES {};
(* Return the statement, declaration or revelation which precedes the given
pragma, or NIL if there is no such node *)


(* Example of use of 'BeforeNode':

VAR
  iter := M3CPragma.BeforeNode(store, node);
  pragma: M3CPragma.T;
BEGIN
  WHILE M3CPragma.Next(iter, pragma) AND
      M3CPragma.FollowingNode(pragma) = node DO
    (* here we know that 'pragma' is immediately before 'node' *)
  END;
END;
*)

END M3CPragma.
