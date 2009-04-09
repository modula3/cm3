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
 * $Id: SupFileRec.m3,v 1.1.1.1 2009-04-09 17:02:00 jkrell Exp $ *)

MODULE SupFileRec;

IMPORT
  ExecRecSeq, FileAttr, Fmt, GlobTree, RCSKeyword, Text, TextSeq,
  TokScan, Word;

REVEAL
  T = Public BRANDED OBJECT
  OVERRIDES
    init := Init;
    overrideFrom := OverrideFrom;
  END;

PROCEDURE Init(self: T; default: T := NIL): T =
  BEGIN
    IF default # NIL THEN
      self.release       := default.release;
      self.serverHost    := default.serverHost;
      self.clientBase    := default.clientBase;
      self.clientCollDir := default.clientCollDir;
      self.clientPrefix  := default.clientPrefix;
      self.checkoutTag   := default.checkoutTag;
      self.checkoutDate  := default.checkoutDate;
      self.listSuffix    := default.listSuffix;
      self.options       := default.options;
      self.umask         := default.umask;
    ELSE
      self.release       := NIL;
      self.serverHost    := NIL;
      self.clientBase    := NIL;
      self.clientCollDir := NIL;
      self.clientPrefix  := NIL;
      self.checkoutTag   := ".";
      self.checkoutDate  := ".";
      self.listSuffix    := NIL;
      self.options       := Options{};
      self.umask         := -1;
    END;
    self.collection      := NIL;
    self.serverBase      := NIL;
    self.serverCollDirs  := NIL;
    self.serverPrefix    := NIL;
    self.keywordPrefix   := NIL;
    self.serverListFile  := NIL;
    self.serverScanFile  := NIL;
    self.superCollection := NIL;
    self.scanTime        := 0.0d0;
    self.accepts         := NEW(TextSeq.T).init();
    self.refusals        := NEW(TextSeq.T).init();
    self.executes        := NEW(ExecRecSeq.T).init();
    self.fileFilter      := GlobTree.True;
    self.dirFilter       := GlobTree.True;
    self.symlink         := GlobTree.False;
    self.noRsync         := GlobTree.False;
    self.attrIgnore      :=
      FileAttr.AttrTypes{ FileAttr.AttrType.Dev, FileAttr.AttrType.Inode };
    self.expander        := NEW(RCSKeyword.T).init();
    RETURN self;
  END Init;

PROCEDURE OverrideFrom(self: T;
                       v: T;
		       mask := Options{}) =
  CONST
    AllOptions = Options{FIRST(Option)..LAST(Option)};
  VAR
    newOptions := v.options * mask;
    oldOptions := self.options * (AllOptions - mask);
  BEGIN
    IF v.release # NIL THEN self.release := v.release END;
    IF v.serverHost # NIL THEN self.serverHost := v.serverHost END;
    IF v.clientBase # NIL THEN self.clientBase := v.clientBase END;
    IF v.clientCollDir # NIL THEN self.clientCollDir := v.clientCollDir END;
    IF v.clientPrefix # NIL THEN self.clientPrefix := v.clientPrefix END;
    IF v.umask # -1 THEN self.umask := v.umask END;
    IF Option.CheckoutMode IN newOptions THEN
      self.checkoutTag := v.checkoutTag;
      self.checkoutDate := v.checkoutDate;
    END;
    IF v.listSuffix # NIL THEN self.listSuffix := v.listSuffix END;

    FOR i := 0 TO v.accepts.size()-1 DO
      self.accepts.addhi(v.accepts.get(i));
    END;
    FOR i := 0 TO v.refusals.size()-1 DO
      self.refusals.addhi(v.refusals.get(i));
    END;

    self.options := oldOptions + newOptions;
  END OverrideFrom;

PROCEDURE Compare(a, b: T): [-1..1] =
  VAR
    c: [-1..1];
  BEGIN
    c := Text.Compare(a.collection, b.collection);
    IF c = 0 THEN
      c := Text.Compare(a.release, b.release);
    END;
    RETURN c;
  END Compare;

PROCEDURE Check(sfr: T; coll, rel: TEXT)
  RAISES {TokScan.Error} =
  BEGIN
    IF NOT Text.Equal(coll, sfr.collection) THEN
      RAISE TokScan.Error("Collection mismatch: expected \"" &
	sfr.collection & "\", got \"" & coll & "\"");
    END;
    IF NOT Text.Equal(rel, sfr.release) THEN
      RAISE TokScan.Error("Release mismatch: expected \"" &
	sfr.release & "\", got \"" & rel & "\"");
    END;
  END Check;

PROCEDURE DecodeOptions(text: TEXT): Options
  RAISES {TokScan.Error} =
  VAR
    options := Options{};
  BEGIN
    TRY
      WITH flags = TokScan.AtoI(text) DO
	FOR o := FIRST(Option) TO LAST(Option) DO
	  IF Word.And(flags, Word.LeftShift(1, ORD(o))) # 0 THEN
	    options := options + Options{o};
	  END;
	END;
      END;
      RETURN options;
    EXCEPT TokScan.Error =>
      RAISE TokScan.Error("Invalid SupFileRec option encoding");
    END;
  END DecodeOptions;

PROCEDURE Equal(a, b: T): BOOLEAN =
  BEGIN
    RETURN Text.Equal(a.collection, b.collection) AND
      Text.Equal(a.release, b.release);
  END Equal;

PROCEDURE EncodeOptions(options: Options): TEXT =
  VAR
    flags: Word.T := 0;
  BEGIN
    FOR o := FIRST(Option) TO LAST(Option) DO
      IF o IN options THEN
	flags := Word.Or(flags, Word.LeftShift(1, ORD(o)));
      END;
    END;
    RETURN Fmt.Unsigned(flags, 10);
  END EncodeOptions;

BEGIN
END SupFileRec.
