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
 * $Id: RCSDeltaClass.i3,v 1.1.1.1 2009-04-09 17:01:57 jkrell Exp $ *)

(* The "RCSDeltaClass" interface reveals the private implementation details
   of RCS deltas. *)

INTERFACE RCSDeltaClass;

IMPORT
  RCSDelta, RCSDeltaList, RCSError, RCSFile, RCSPhrases, RCSString;

REVEAL
  RCSDelta.T = RCSDelta.Public BRANDED OBJECT
    rcsFile: RCSFile.T := NIL;
    log: RCSString.T := NIL;
    text: RCSString.T := NIL;
    next: RCSDelta.T := NIL;
    prev: RCSDelta.T := NIL;
    diffBase: RCSDelta.T := NIL;  (* = prev, next, or NIL *)
    branches: RCSDeltaList.T := NIL;
    treePhrases: RCSPhrases.T := NIL;
    textPhrases: RCSPhrases.T := NIL;
    accessor: Accessor := NIL;
    isParsed: BOOLEAN := FALSE;  (* TRUE if text has been parsed. *)
    isPlaceHolder: BOOLEAN := FALSE;
  END;

TYPE
  Accessor <: AccessorPublic;

  AccessorPublic = OBJECT METHODS
    getLine(i: CARDINAL): RCSString.T;
    numLines(): CARDINAL;
  END;

PROCEDURE AddBranch(delta: RCSDelta.T;
                    branch: RCSDelta.T;
		    diffBase: RCSDelta.T)
  RAISES {RCSError.E};
(* Add a new branch to the given delta. *)

(* The list of branches is maintained in sorted order.  It is an error
   to add a branch that is already present. *)

PROCEDURE ChangeBranch(delta, oldBranch, newBranch: RCSDelta.T)
  RAISES {RCSError.E};
(* Changes a branch of the given delta to point to a different delta. *)

PROCEDURE DeleteBranch(delta: RCSDelta.T; branch: RCSDelta.T)
  RAISES {RCSError.E};
(* Delete a branch from a delta.  It is an error to attempt to delete a
   non-existent branch. *)

END RCSDeltaClass.
