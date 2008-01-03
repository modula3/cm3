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

MODULE M3CWhitespace;

IMPORT M3CHash;

PROCEDURE NewWhitespace(<*UNUSED*> c: M3CHash.IdCreator;
                        <*UNUSED*> text: TEXT): M3CHash.Id RAISES {}=
  BEGIN
    RETURN NEW(T);
  END NewWhitespace;


TYPE
  WhitespaceCreator = M3CHash.IdCreator OBJECT OVERRIDES new := NewWhitespace END;

VAR
  table_g := NEW(M3CHash.Table).init(256, NEW(WhitespaceCreator));


<*INLINE*> PROCEDURE Table(): M3CHash.Table RAISES {}=
  BEGIN
    RETURN table_g;
  END Table;


<*INLINE*> PROCEDURE ToText(id: T): TEXT RAISES {}=
  BEGIN
    RETURN id.toText();
  END ToText;


<*INLINE*> PROCEDURE Enter(text: TEXT): T RAISES {}=
  BEGIN
    RETURN table_g.enter(text);
  END Enter;

BEGIN
END M3CWhitespace.
       
