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

MODULE M3CComment;

IMPORT Text;
IMPORT M3AST_AS;
IMPORT M3CSrcPos;
IMPORT M3CComment;

(**IMPORT M3AST_AS_F;**)

REVEAL
  M3CComment.T = BRANDED OBJECT
    prev, next:  M3CComment.Iter := NIL;
    pos: M3CSrcPos.T;
    body: TEXT;
    precedingNode, followingNode: M3AST_AS.SRC_NODE := NIL;
  END;

  M3CComment.Iter = M3CComment.T BRANDED OBJECT END;

  M3CComment.Store = BRANDED REF RECORD
    first, last: M3CComment.Iter := NIL;
  END;

PROCEDURE NewStore(): Store RAISES {}=
  BEGIN
    RETURN NEW(Store);
  END NewStore;

 
PROCEDURE AddToStore(
    body: Text.T;
    pos: M3CSrcPos.T;
    precedingNode: M3AST_AS.SRC_NODE;
    VAR store: Store)
    : T
    RAISES {}=
  VAR
    new := NEW(Iter, pos := pos, body := body,
        precedingNode := precedingNode);
  BEGIN
    WITH last = store.last DO
      IF last = NIL THEN
        store.first := new;
      ELSE
        last.next := new;
        new.prev := last;
      END;
      last := new;
    END;
    RETURN new;
  END AddToStore;


PROCEDURE AddFollowingNode(
    followingNode: M3AST_AS.SRC_NODE;
    store: Store)
    RAISES {}=
  VAR
    t := store.last;
    precedingNode := t.precedingNode;
  BEGIN
    REPEAT
      t.followingNode := followingNode;
      t := t.prev;
    UNTIL t = NIL OR t.precedingNode # precedingNode;
  END AddFollowingNode;


PROCEDURE NewIter(ps: Store): Iter RAISES {}=
  BEGIN
    IF ps = NIL THEN RETURN NIL ELSE RETURN ps.first END;
  END NewIter;

 
PROCEDURE Next(VAR iter: Iter; VAR t: T): BOOLEAN RAISES {}=
  BEGIN
    IF iter = NIL THEN RETURN FALSE END;
    t := iter;
    iter := iter.next;
    RETURN TRUE;
  END Next;


PROCEDURE Position(t: T): M3CSrcPos.T RAISES {}=
  BEGIN
    RETURN t.pos;
  END Position;

 
PROCEDURE Body(t: T): Text.T RAISES {}=
  BEGIN
    RETURN t.body;
  END Body;

 
PROCEDURE PrecedingNode(t: T): M3AST_AS.SRC_NODE RAISES {}=
  BEGIN
    RETURN t.precedingNode;
  END PrecedingNode;

 
PROCEDURE FollowingNode(t: T): M3AST_AS.SRC_NODE RAISES {}=
  BEGIN
    RETURN t.followingNode;
  END FollowingNode;

 
BEGIN

END M3CComment.
