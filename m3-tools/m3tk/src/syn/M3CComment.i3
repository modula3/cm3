(************************************************************************
!		                                                        *
!*                                                                      *
!*         Copyright 1994 Sun Microsystems, Inc. All Rights Reserved.   *
!*                                                                      *
!*      Permission to use, copy, modify, and distribute this software   *
!*      and its documentation for any purpose and without fee is hereby *
!*      granted, provided that the above copyright notice appear in all *
!*      copies and that both that copyright notice and this permission  *
!*      notice appear in supporting documentation, and that the name of *
!*      Sun Microsystems, Inc. (SMI) not be used in advertising or      *
!*      publicity pertaining to distribution of the software without    *
!*      specific, written prior permission.                             *
!*                                                                      *
!*                                                                      *
!*      SMI DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,      *
!*      INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY,	        *
!*      FITNESS FOR A PARTICULAR PURPOSE OR NON-INFRINGEMENT.           *
!*      IN NO EVENT SHALL SMI BE LIABLE FOR ANY SPECIAL, INCIDENTAL,    *
!*	INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER     *
!*      RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN      *
!*      ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,        *
!*      ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE     *
!*      OF THIS SOFTWARE.                                               *
!*                                                                      *
!***********************************************************************)

INTERFACE M3CComment;

IMPORT Text;
IMPORT M3AST_AS;
IMPORT M3CSrcPos;


(* The following are used by the parser to build up a Comment store. 'NewStore'
creates a new Comment store and each Comment encountered is added with the
'AddToStore' procedure. Comments must be added in source position order. Comments
must start with "(*" and end with "*)" *)

TYPE
  Store <: REFANY;
  T <: REFANY;

PROCEDURE NewStore(): Store RAISES {};
(* Create new Comment store *)

PROCEDURE AddToStore(
    body: Text.T;
    pos: M3CSrcPos.T;
    precedingNode: M3AST_AS.SRC_NODE;
    VAR store: Store)
    : T
    RAISES {};
(* Add a new Comment to a store, giving its body, position and the preceding
source node *)

PROCEDURE AddFollowingNode(
    followingNode: M3AST_AS.SRC_NODE;
    store: Store)
    RAISES {};
(* Called after a Comment (or Comments) has been added by 'AddToStore' and
another source node is encountered. Marks all the Comments after the last
source node as having the given 'followingNode' *)

(* The Comments in a Comment store can be iterated using 'NewIter' and 'Next'.
The iterator works even while the store is being built up using 'AddToStore' *)

TYPE
  Iter <: REFANY;

PROCEDURE NewIter(ps: Store): Iter RAISES {};
(* Return iterator for Comments. They will be iterated in ascending positional
order. If 'after' is not null only the Comments whose position is greater than
'after' will be iterated *)

PROCEDURE Next(VAR iter: Iter; VAR t: T): BOOLEAN RAISES {};
(* 'Next' returns FALSE if 'iter' is NIL *)


(* The following enquiry functions can be used on all Comments *)

PROCEDURE Position(t: T): M3CSrcPos.T RAISES {};
(* Source position of Comment *)

PROCEDURE Body(t: T): Text.T RAISES {};
(* Text of Comment. Includes opening and closing brackets *)

PROCEDURE PrecedingNode(t: T): M3AST_AS.SRC_NODE RAISES {};
(* Return the node which immediately precedes the given Comment, or NIL if there
is no such node *)

PROCEDURE FollowingNode(t: T): M3AST_AS.SRC_NODE RAISES {};
(* Return the node which immediately follows the given Comment, or NIL if there
is no such node *)
 
END M3CComment.

