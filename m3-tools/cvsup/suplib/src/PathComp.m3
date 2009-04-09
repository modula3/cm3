(* Copyright 1997-2003 John D. Polstra.
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
 * $Id: PathComp.m3,v 1.1.1.1 2009-04-09 17:01:56 jkrell Exp $ *)

MODULE PathComp;

IMPORT Pathname, SupMisc, Text;

REVEAL
  Compressor = CPublic BRANDED OBJECT
    root: Pathname.T;
    prev: Pathname.T;
    target: Pathname.T;
    file: TEXT;
    rootLen: CARDINAL;
    targLen: CARDINAL;
    curLen: CARDINAL;
    goal: CARDINAL;  (* Pathname length we are currently moving toward. *)
    rootIsAbsolute: BOOLEAN;
  OVERRIDES
    init := CInit;
    put := CPut;
    finish := CFinish;
    get := CGet;
  END;

PROCEDURE CInit(self: Compressor; root: Pathname.T := ""): Compressor =
  BEGIN
    self.root := root;
    self.prev := root;
    self.target := NIL;
    self.file := NIL;
    self.rootLen := Text.Length(self.root);
    self.curLen := self.rootLen;
    self.rootIsAbsolute := Pathname.Absolute(root);
    RETURN self;
  END CInit;

PROCEDURE CPut(self: Compressor; type: Type; path: Pathname.T)
  RAISES {Error} =
  VAR
    slashPos: INTEGER;
  BEGIN
    <* ASSERT self.target = NIL *>
    IF Pathname.Absolute(path) # self.rootIsAbsolute THEN
      RAISE Error("Absoluteness of path does not match root");
    END;
    CASE type OF
    | Type.DirDown =>
	self.target := path;
	self.file := NIL;
    | Type.File, Type.DirUp =>
	slashPos := Text.FindCharR(path, SupMisc.SlashChar);
	IF type = Type.File THEN
	  self.file := Text.Sub(path, slashPos+1);
	ELSE
	  self.file := NIL;
	END;
	IF slashPos <= 0 THEN  (* Special case for "" or "/". *)
	  INC(slashPos);
	END;
	self.target := Text.Sub(path, 0, slashPos);
    END;
    self.targLen := Text.Length(self.target);
    self.goal := SupMisc.CommonPathLength(self.prev, self.target);
    IF self.goal < self.rootLen THEN
      RAISE Error("Attempt to ascend above the root");
    END;
    IF self.curLen = self.goal THEN  (* No need to go up. *)
      self.goal := self.targLen;
    END;
  END CPut;

PROCEDURE CFinish(self: Compressor) =
  BEGIN
    self.target := self.root;
    self.targLen := self.rootLen;
    self.goal := self.rootLen;
    self.file := NIL;
  END CFinish;

PROCEDURE CGet(self: Compressor;
               VAR type: Type;
	       VAR name: TEXT): BOOLEAN =
  VAR
    slashPos, start, limit: INTEGER;
  BEGIN
    IF self.curLen > self.goal THEN  (* Going up. *)
      type := Type.DirUp;
      slashPos := Text.FindCharR(self.prev, SupMisc.SlashChar, self.curLen-1);
      name := Text.Sub(self.prev, slashPos+1, self.curLen - (slashPos+1));
      IF slashPos <= 0 THEN  (* Special case for "" or "/". *)
	self.curLen := slashPos + 1;
      ELSE
	self.curLen := slashPos;
      END;
      IF self.curLen <= self.goal THEN  (* Done going up. *)
	<* ASSERT self.curLen = self.goal *>
	self.goal := self.targLen;
      END;
      RETURN TRUE;
    ELSIF self.curLen < self.goal THEN  (* Going down. *)
      type := Type.DirDown;
      IF self.curLen = 0
      OR self.curLen = 1 AND self.rootIsAbsolute THEN
	(* Special case for "" or "/". *)
	start := self.curLen;
      ELSE
	start := self.curLen + 1;
      END;
      limit := Text.FindChar(self.target, SupMisc.SlashChar, start);
      IF limit = -1 THEN limit := self.goal END;
      name := Text.Sub(self.target, start, limit - start);
      self.curLen := limit;
      RETURN TRUE;
    ELSIF self.file # NIL THEN  (* In the right directory, emit filename. *)
      type := Type.File;
      name := self.file;
      self.file := NIL;
      RETURN TRUE;
    ELSE  (* Done. *)
      IF self.target # NIL THEN
	self.prev := self.target;
	self.target := NIL;
      END;
      RETURN FALSE;
    END;
  END CGet;

(*****************************************************************************)

REVEAL
  Decompressor = DPublic BRANDED OBJECT
    current: Pathname.T;
  OVERRIDES
    init := DInit;
    put := DPut;
    getDir := DGetDir;
  END;

PROCEDURE DInit(self: Decompressor; root: Pathname.T := ""): Decompressor =
  BEGIN
    self.current := root;
    RETURN self;
  END DInit;

PROCEDURE DPut(self: Decompressor; type: Type; name: TEXT): Pathname.T =
  VAR
    path: Pathname.T;
  BEGIN
    CASE type OF
    | Type.DirDown =>
	self.current := SupMisc.CatPath(self.current, name);
	RETURN self.current;
    | Type.File =>
	RETURN SupMisc.CatPath(self.current, name);
    | Type.DirUp =>
	path := self.current;
	self.current := SupMisc.PathPrefix(self.current);
	RETURN path;
    END;
  END DPut;

PROCEDURE DGetDir(self: Decompressor): Pathname.T =
  BEGIN
    RETURN self.current;
  END DGetDir;

BEGIN
END PathComp.
