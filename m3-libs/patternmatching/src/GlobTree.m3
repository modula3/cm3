(* Copyright 1996-1998 John D. Polstra.
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
 * $Id: GlobTree.m3,v 1.1 2001-12-01 16:07:31 wagner Exp $ *)

MODULE GlobTree;

IMPORT Glob, RegEx;

TYPE
  MatchNode = T OBJECT
    pattern: TEXT;
    options: Glob.MatchOptions;
  OVERRIDES
    test := MatchTest;
  END;

  UnaryNode = T OBJECT
    child: T;
  END;

  BinaryNode = T OBJECT
    left, right: T;
  END;

(* Constructors. *)

PROCEDURE Match(pattern: TEXT; options := Glob.MatchOptions{}): T =
  BEGIN
    RETURN NEW(MatchNode, pattern := pattern, options := options);
  END Match;

PROCEDURE And(left, right: T): T =
  BEGIN
    IF left = True THEN
      RETURN right;
    ELSIF left = False THEN
      RETURN False;
    ELSIF right = True THEN
      RETURN left;
    ELSIF right = False THEN
      RETURN False;
    ELSE
      RETURN NEW(BinaryNode, test := AndTest, left := left, right := right);
    END;
  END And;

PROCEDURE Or(left, right: T): T =
  BEGIN
    IF left = True THEN
      RETURN True;
    ELSIF left = False THEN
      RETURN right;
    ELSIF right = True THEN
      RETURN True;
    ELSIF right = False THEN
      RETURN left;
    ELSE
      RETURN NEW(BinaryNode, test := OrTest, left := left, right := right);
    END;
  END Or;

PROCEDURE Not(child: T): T =
  BEGIN
    IF child = True THEN
      RETURN False;
    ELSIF child = False THEN
      RETURN True;
    ELSE
      RETURN NEW(UnaryNode, test := NotTest, child := child);
    END;
  END Not;

(* Evaluators. *)

PROCEDURE MatchTest(self: MatchNode; filename: TEXT): BOOLEAN 
  RAISES {RegEx.Error} =
  BEGIN
    RETURN Glob.Match(self.pattern, filename, self.options);
  END MatchTest;

PROCEDURE AndTest(self: BinaryNode; filename: TEXT): BOOLEAN
  RAISES {RegEx.Error} =
  BEGIN
    RETURN self.left.test(filename) AND self.right.test(filename);
  END AndTest;

PROCEDURE OrTest(self: BinaryNode; filename: TEXT): BOOLEAN
  RAISES {RegEx.Error} =
  BEGIN
    RETURN self.left.test(filename) OR self.right.test(filename);
  END OrTest;

PROCEDURE NotTest(self: UnaryNode; filename: TEXT): BOOLEAN
  RAISES {RegEx.Error} =
  BEGIN
    RETURN NOT self.child.test(filename);
  END NotTest;

PROCEDURE TrueTest(<*UNUSED*> self: T; <*UNUSED*> filename: TEXT): BOOLEAN =
  BEGIN
    RETURN TRUE;
  END TrueTest;

PROCEDURE FalseTest(<*UNUSED*> self: T; <*UNUSED*> filename: TEXT): BOOLEAN =
  BEGIN
    RETURN FALSE;
  END FalseTest;

BEGIN
  True := NEW(T, test := TrueTest);
  False := NEW(T, test := FalseTest);
END GlobTree.
