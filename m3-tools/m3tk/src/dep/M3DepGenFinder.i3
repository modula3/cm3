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

INTERFACE M3DepGenFinder;

IMPORT M3DepFindFile;

PROCEDURE New(overrides := FALSE): M3DepFindFile.T;
(* fire off m3build to create a finder for a complete source map,
   using -O if "override = TRUE". *)

END M3DepGenFinder.
