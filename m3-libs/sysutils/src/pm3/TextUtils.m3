(* Copyright 1999-2002 elego Software Solutions GmbH, Berlin, Germany.
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
 *)

(*---------------------------------------------------------------------------*)
MODULE TextUtils EXPORTS TextUtils;

IMPORT ASCII, CIText, FastLex, Msg, Text, TextF, TextSeq, TextTextTbl,
       TextRd, TextWr, UnsafeRd, Rd, Wr, ProcessEnv;
IMPORT TextExtras AS TextEx;
(* IMPORT Msg; *)

(*---------------------------------------------------------------------------*)
TYPE CharacterArray = REF ARRAY OF CHAR;

(*--------------------------------------------------------------------------*)
CONST
  ExprApplChar     = '$';
  ExprBeginChar    = '{';
  ExprEndChar      = '}'; <*NOWARN*>

  Num             = SET OF CHAR {'0'..'9'};
  Alpha           = SET OF CHAR {'a'..'z', 'A'..'Z'};
  AllChars        = SET OF CHAR {FIRST(CHAR) .. LAST(CHAR)};
  KeywordChars    = Alpha + SET OF CHAR {'-', '_'} + Num;

  NonExprApplChars = AllChars - SET OF CHAR {ExprApplChar};

VAR
  exprApplChar := Text.FromChar(ExprApplChar);

(*---------------------------------------------------------------------------*)
PROCEDURE SkipLeft(t : TEXT; s : ASCII.Set := ASCII.Spaces) : TEXT =
  VAR l := Text.Length(t);
      i : CARDINAL := 0;
  BEGIN
    WHILE i < l AND t^[i] IN s DO
      INC(i);
    END;
    IF i = 0 THEN RETURN t END;
    IF i = l THEN RETURN "" END;
    RETURN Text.Sub(t, i, l - i);
  END SkipLeft;

(*---------------------------------------------------------------------------*)
PROCEDURE SkipRight(t : TEXT; s : ASCII.Set := ASCII.Spaces) : TEXT =
  VAR l := Text.Length(t);
      j : CARDINAL := l;
  BEGIN
    WHILE j > 0 AND t^[j - 1] IN s DO
      DEC(j);
    END;
    IF j = 0 THEN RETURN "" END;
    IF j = l THEN RETURN t END;
    RETURN Text.Sub(t, 0, j);
  END SkipRight;

(*---------------------------------------------------------------------------*)
PROCEDURE Compress(t : TEXT; s : ASCII.Set := ASCII.Spaces) : TEXT =
  VAR l := Text.Length(t);
      i : CARDINAL := 0;
      j : CARDINAL := l;
  BEGIN
    WHILE i < l AND t^[i] IN s DO
      INC(i);
    END;
    WHILE j > 0 AND t^[j - 1] IN s DO
      DEC(j);
    END;
    IF i = 0 AND j = l THEN RETURN t END;
    IF i > j THEN RETURN "" END;
    RETURN Text.Sub(t, i, j - i);
  END Compress;

(*---------------------------------------------------------------------------*)
PROCEDURE SubstChar(t : TEXT; a, b : CHAR) : TEXT =
  VAR
    len := Text.Length(t);
    res :  CharacterArray;
  BEGIN
    res := NEW(CharacterArray, len);
    FOR i := 0 TO len - 1 DO
      IF t^[i] = a THEN
        res^[i] := b;
      ELSE
        res^[i] := t^[i];
      END;
    END;
    RETURN Text.FromChars(res^);
  END SubstChar;

(*---------------------------------------------------------------------------*)
PROCEDURE SubstChars(t : TEXT; READONLY a, b : ARRAY OF CHAR) : TEXT =
  VAR
    len := Text.Length(t);
    res :  CharacterArray;
    found : BOOLEAN;
    k     : CARDINAL;
  BEGIN
    res := NEW(CharacterArray, len);
    FOR i := 0 TO len - 1 DO
      found := FALSE;
      FOR j := FIRST(a) TO LAST(a) DO
        IF t^[i] = a[j] THEN
          k := j;
          found := TRUE;
          EXIT;
        END;
      END;
      IF found THEN
        res^[i] := b[k];
      ELSE
        res^[i] := t^[i];
      END;
    END;
    RETURN Text.FromChars(res^);
  END SubstChars;

(*---------------------------------------------------------------------------*)
PROCEDURE Substitute(READONLY t, a, b : TEXT; times := 0) : TEXT =
  VAR
    i : CARDINAL := 0;
    k : CARDINAL := 0;
    n : CARDINAL := 0;
    r : TEXT := NIL;
  BEGIN
    WHILE ((times = 0) OR (n < times)) AND
      TextEx.FindSub(t, a, i) DO
      INC(n);
      IF r = NIL THEN
        r := Text.Sub(t, k, i - k) & b;
      ELSE
        r := r & Text.Sub(t, k, i - k) & b;
      END;
      INC(i, Text.Length(a));
      k := i;
    END;
    IF r = NIL THEN
      RETURN t;
    ELSE
      RETURN r & Text.Sub(t, k, Text.Length(t) - k);
    END;
  END Substitute;

(*---------------------------------------------------------------------------*)
PROCEDURE RemoveChars(t : TEXT; s : ASCII.Set := ASCII.Spaces) : TEXT =
  VAR
    len := Text.Length(t);
    res :  CharacterArray;
    cc  := 0;
    a   : CHAR;
  BEGIN
    res := NEW(CharacterArray, len);
    FOR i := 0 TO len - 1 DO
      a := t^[i];
      IF a IN s THEN
        INC(cc);
      ELSE
        res^[i-cc] := a;
      END;
    END;
    RETURN Text.FromChars(SUBARRAY(res^, 0, len - cc));
  END RemoveChars;

(*---------------------------------------------------------------------------*)
PROCEDURE Squeeze(READONLY t : TEXT) : TEXT =
  VAR
    in  := TextRd.New(t);
    res := TextWr.New();
    nlc := 0;
    c   : CHAR;
  BEGIN
    WHILE NOT Rd.EOF(in) DO <* NOWARN *>
      c := Rd.GetChar(in); <* NOWARN *>
      IF c = '\n' THEN
        INC(nlc);
      ELSE
        nlc := 0;
      END;
      IF nlc < 3 THEN
        Wr.PutChar(res, c); <* NOWARN *>
      END;
    END;
    RETURN TextWr.ToText(res);
  END Squeeze;

(*---------------------------------------------------------------------------*)
PROCEDURE MemberOfTextSeq(tl : TextSeq.T; elem : TEXT) : BOOLEAN =
  BEGIN
    FOR i := 0 TO tl.size() - 1 DO
      WITH act = tl.get(i) DO
        IF Text.Equal(act, elem) THEN
          RETURN TRUE;
        END;
      END;
    END;
    RETURN FALSE;
  END MemberOfTextSeq;

(*--------------------------------------------------------------------------*)
PROCEDURE TextSeqToText(seq : TextSeq.T; sep := " "; maxCol := 0;
                        contToken := "\\\n") : TEXT =
  VAR
    t    := "";
    col  := 0;
    e    :  TEXT;
    len  :  CARDINAL;
    slen := Text.Length(sep);
    scon := Text.Length(contToken);
    size := 0;
    j    := 0;
    res  :  CharacterArray;
  BEGIN
    IF seq.size() = 1 THEN
      RETURN seq.get(0);
    ELSIF seq.size() > 1 THEN
      (* FIXME: This should be done with a character array, since it
         will result in heavy garbage collections for long lists and
         texts. *)

      t := seq.get(0);
      IF t = NIL THEN
        size := 5;
      ELSE
        len := Text.Length(t);
        size := len;
      END;
      IF maxCol > 0 THEN
        col := size;
      END;
      FOR i := 1 TO seq.size() - 1 DO
	e := seq.get(i);
	IF e = NIL THEN
	  e := "`NIL'";
	END;
        len := Text.Length(e);
        IF maxCol > 0 THEN
          IF col + len + slen > maxCol THEN
            col := len + slen;
            INC(size, 2 * slen + scon + len);
          ELSE
            INC(col, len + slen);
            INC(size, slen + len);
          END;
        ELSE
          INC(size, slen + len);
        END;
      END;

      res := NEW(CharacterArray, size);
      col := 0; j := 0;
      e := seq.get(0);
      IF e = NIL THEN
        e := "`NIL'";
      END;
      len := Text.Length(t);
      SUBARRAY(res^, j, len) := SUBARRAY(e^, 0, len); INC(j, len);
      IF maxCol > 0 THEN
        col := len;
      END;
      FOR i := 1 TO seq.size() - 1 DO
	e := seq.get(i);
	IF e = NIL THEN
	  e := "`NIL'";
	END;
        len := Text.Length(e);
        IF maxCol > 0 THEN
          IF col + len + slen > maxCol THEN
            col := len + slen;
            SUBARRAY(res^, j, slen) := SUBARRAY(sep^, 0, slen); INC(j, slen);
            SUBARRAY(res^, j, scon) := SUBARRAY(contToken^, 0, scon);
            INC(j, scon);
          ELSE
            SUBARRAY(res^, j, slen) := SUBARRAY(sep^, 0, slen); INC(j, slen);
            SUBARRAY(res^, j, len) := SUBARRAY(e^, 0, len); INC(j, len);
            INC(col, len + slen);
          END;
        ELSE
          SUBARRAY(res^, j, slen) := SUBARRAY(sep^, 0, slen); INC(j, slen);
          SUBARRAY(res^, j, len) := SUBARRAY(e^, 0, len); INC(j, len);
        END;
      END;
      RETURN Text.FromChars(res^);
    ELSE
      RETURN "";
    END;
      (*
      t := seq.get(0);
      IF t = NIL THEN
        t := "`NIL'";
      END;
      IF maxCol > 0 THEN
        col := Text.Length(t);
      END;
      FOR i := 1 TO seq.size() - 1 DO
	e := seq.get(i);
	IF e = NIL THEN
	  e := "`NIL'";
	END;
        IF maxCol > 0 THEN
          len := Text.Length(e);
          IF col + len + slen > maxCol THEN
            col := len + slen;
            t := t & sep & contToken & sep & e;
          ELSE
            t := t & sep & e;
            INC(col, len + slen);
          END;
        ELSE
          t := t & sep & e;
        END;
      END;
    END;
    RETURN t; *)
  END TextSeqToText;

(*--------------------------------------------------------------------------*)
(* old interface
PROCEDURE SubstEnvVars(READONLY t : TEXT;
                       READONLY vars : ARRAY OF TEXT;
                       env : TextTextTbl.T := NIL) : TEXT =
  VAR
    name, val, res : TEXT;
  BEGIN
    res := t;
    IF env = NIL THEN
      env := ProcessEnv.Current();
    END;
    FOR i := FIRST(vars) TO LAST(vars) DO
      WITH var = vars[i] DO
        IF NOT Text.Empty(var) THEN
	  name := "${" & var & "}";
	  val := ProcessEnv.Value(env, var);
	  (* Msg.Debug("SubstEnvVars: s/" & name & "/" & val & "/g"); *)
	  IF val = NIL THEN val := "" END;
	  res := Substitute(res, name, val);
	  name := "$" & var;
	  res := Substitute(res, name, val);
        END;
      END;
    END;
    RETURN res;
  END SubstEnvVars;
*)

(*--------------------------------------------------------------------------*)
PROCEDURE SubstEnvVars(READONLY t : TEXT;
                       env : TextTextTbl.T := NIL) : TEXT =
  VAR
    name, val, res : TEXT;
    rd := TextRd.New(t);
    c : CHAR;

  BEGIN
    IF env = NIL THEN
      env := ProcessEnv.Current();
    END;

    TRY
      LOCK rd DO
        res := FastLex.Scan(rd, NonExprApplChars);
        WHILE NOT UnsafeRd.FastEOF(rd) DO
          (* next char is $ *)
          EVAL UnsafeRd.FastGetChar(rd);
          c := UnsafeRd.FastGetChar(rd);
          (* This may be the { or an alpha character or another $ *)
          IF c = ExprBeginChar THEN
            name := FastLex.Scan(rd, KeywordChars);
            EVAL UnsafeRd.FastGetChar(rd); (* assume trailing } :-) *)
            val := ProcessEnv.Value(env, name);
            IF val # NIL THEN res := res & val; END;
          ELSIF c IN Alpha THEN
            UnsafeRd.FastUnGetChar(rd);
            name := FastLex.Scan(rd, KeywordChars);
            val := ProcessEnv.Value(env, name);
            IF val # NIL THEN res := res & val; END;
          ELSIF c = ExprApplChar THEN
            res := res & exprApplChar;
          ELSE
            (* This is an error, but we just ignore it. *)
            res := res & exprApplChar;
            UnsafeRd.FastUnGetChar(rd);
          END;
          res := res & FastLex.Scan(rd, NonExprApplChars);
        END;
      END;
    EXCEPT ELSE
      (* We cannot really do something meaningful here *)
      Msg.Error("caught exception in SubstEnvVars");
    END;
    RETURN res;
  END SubstEnvVars;

(*--------------------------------------------------------------------------*)
PROCEDURE AddPrefix(seq : TextSeq.T; prefix : TEXT) : TextSeq.T =
  VAR res := NEW(TextSeq.T).init(seq.size());
  BEGIN
    FOR i := 0 TO seq.size() - 1 DO
      WITH elem = seq.get(i) DO
        res.addhi(prefix & elem);
      END;
    END;
    RETURN res;
  END AddPrefix;

(*--------------------------------------------------------------------------*)
PROCEDURE AddSuffix(seq : TextSeq.T; suffix : TEXT) : TextSeq.T =
  VAR res := NEW(TextSeq.T).init(seq.size());
  BEGIN
    FOR i := 0 TO seq.size() - 1 DO
      WITH elem = seq.get(i) DO
        res.addhi(elem & suffix);
      END;
    END;
    RETURN res;
  END AddSuffix;

(*--------------------------------------------------------------------------*)
PROCEDURE Split(text : TEXT; sep : TEXT) : TextSeq.T =
  VAR
    b : CARDINAL := 0;
    i : CARDINAL := 0;
    seplen := Text.Length(sep);
    res := NEW(TextSeq.T).init();
  BEGIN
    WHILE TextEx.FindSub(text, sep, i) DO
      WITH elem = Text.Sub(text, b, i - b) DO
        res.addhi(elem);
        (* next begin is after the current separator *)
        b := i + seplen;
        (* this is where we start searching now, too *)
        i := b;
      END;
    END;
    (* Add the rest of the text as last element. (This is the only one
       in case no separator has been found.) *)
    res.addhi(Text.Sub(text, b, Text.Length(text) -b));
    RETURN res;
  END Split;

(*--------------------------------------------------------------------------*)
PROCEDURE Tokenize(text : TEXT; sepchars := ASCII.Spaces;
                   squeeze := TRUE) : TextSeq.T =
  VAR
    i   := 0;
    b   := 0;
    l   := 0;
    len := Text.Length(text);
    res := NEW(TextSeq.T).init();
  BEGIN
    WHILE i < len DO
      IF text^[i] IN sepchars THEN
        l := i - b;
        IF squeeze AND l > 0 OR NOT squeeze AND l >= 0 THEN
          res.addhi(Text.Sub(text, b, l));
          (* Msg.D(Text.Sub(text, b, l)); *)
        END;
        b := i + 1;
      END;
      INC(i);
    END;
    l := i - b;
    IF squeeze AND l > 0 OR NOT squeeze AND l >= 0 THEN
      res.addhi(Text.Sub(text, b, l));
      (* Msg.D(Text.Sub(text, b, l)); *)
    END;
    RETURN res;
  END Tokenize;

(*--------------------------------------------------------------------------*)
PROCEDURE IsUpper(t : TEXT; len : INTEGER) : BOOLEAN =
  (* return if all of a text is already all uppercase *)
  BEGIN
    FOR i := 0 TO len - 1 DO
      WITH ch = t^[i] DO
        IF ASCII.Upper[ch] # ch THEN
          RETURN FALSE;
        END;
      END;
    END;
    RETURN TRUE;
  END IsUpper;

(*--------------------------------------------------------------------------*)
PROCEDURE IsLower(t : TEXT; len : INTEGER) : BOOLEAN =
  (* return if all of a text is already all lowercase *)
  BEGIN
    FOR i := 0 TO len - 1 DO
      WITH ch = t^[i] DO
        IF ASCII.Lower[ch] # ch THEN
          RETURN FALSE;
        END;
      END;
    END;
    RETURN TRUE;
  END IsLower;

(*--------------------------------------------------------------------------*)
PROCEDURE Lower(t : TEXT) : TEXT =
  (* return a text where all alphas are in lower case *)
  VAR
    len :  INTEGER;
    res :  CharacterArray;
  BEGIN
    IF t = NIL THEN RETURN t; END;
    len := Text.Length(t);
    IF IsLower(t, len) THEN RETURN t; END;
    res := NEW(CharacterArray, len);
    FOR i := 0 TO len - 1 DO
      res^[i] := ASCII.Lower[t^[i]]
    END;
    RETURN Text.FromChars(res^);
  END Lower;

(*--------------------------------------------------------------------------*)
PROCEDURE Upper(t : TEXT) : TEXT =
  (* return a text where all alphas are in upper case *)
  VAR
    len :  INTEGER;
    res :  CharacterArray;
  BEGIN
    IF t = NIL THEN RETURN t; END;
    len := Text.Length(t);
    IF IsUpper(t, len) THEN RETURN t; END;
    res := NEW(CharacterArray, len);
    FOR i := 0 TO len - 1 DO
      res^[i] := ASCII.Upper[t^[i]]
    END;
    RETURN Text.FromChars(res^);
  END Upper;

(*---------------------------------------------------------------------------*)
PROCEDURE CountChar(s : TEXT; ch: CHAR; caseSensitive := TRUE) : INTEGER =
  VAR
    len := Text.Length(s);
    count := 0;
  BEGIN
    IF NOT caseSensitive THEN
      s := Lower(s);
      ch := ASCII.Lower[ch];
    END;
    FOR i := 0 TO len - 1 DO
        IF s^[i] = ch THEN
            INC(count);
        END
    END;
    RETURN count;
  END CountChar;

(*---------------------------------------------------------------------------*)
PROCEDURE SubstituteVariables(t : TEXT; parameters : TextTextTbl.T) : TEXT
  RAISES {Error} =
  VAR
    i : CARDINAL := 0;
    j, k, l, m : CARDINAL;
    pre, suf, name, val : TEXT;
    vchars := ASCII.Set{':', '?', '!'};
    c : CHAR;
    defVar, defConst : BOOLEAN;
    defaultValue, defaultVarName, expr : TEXT;
  BEGIN
    (* Msg.V("SubstituteVariables(" & t & ")"); *)
    WHILE TextEx.FindChar(t, '{', i) DO
      j := i;
      IF TextEx.FindCharSet(t, vchars, j) AND (j = i + 1) THEN
        (* found {: or {! or {? *)
        c := Text.GetChar(t, j);
        INC(j);
        k := j;
        IF TextEx.FindChar(t, '}', k) THEN
          pre  := Text.Sub(t, 0, i);
          name := Text.Sub(t, j, k - j);
          (* check for default values, either
             {:varname?varname},
             {:varname:const}, or
             {:varname?varname:const}
          *)
          defaultValue := NIL; l:= 0 ; m := 0;
          defVar := TextEx.FindChar(name, '?', l);
          defConst := TextEx.FindChar(name, ':', m);
          IF defVar AND defConst THEN
            IF l < m THEN
              expr := name;
              name := Text.Sub(expr, 0, l);
              defaultVarName := Text.Sub(expr, l + 1, m - l -1);
              IF parameters # NIL AND
                parameters.get(defaultVarName, defaultValue) THEN
                (* Msg.V("  defaultValue(1) for " & name & " from " &
                   defaultVarName & ": " & defaultValue); *)
                defaultValue := SubstituteVariables(defaultValue, parameters);
              ELSE
                defaultValue := Text.Sub(expr, m + 1);
              END;
              (* Msg.V("  defaultValue(2) for " & name & ": " &
                 defaultValue); *)
            ELSE
              RAISE Error("invalid default value syntax: " & expr);
            END;
          ELSIF defVar THEN
            expr := name;
            name := Text.Sub(expr, 0, l);
            defaultVarName := Text.Sub(expr, l + 1);
            IF parameters # NIL AND
              parameters.get(defaultVarName, defaultValue) THEN
              (* Msg.V("  defaultValue(3) for " & name & " from " &
                 defaultVarName & ": " & defaultValue); *)
              defaultValue := SubstituteVariables(defaultValue, parameters);
              (* Msg.V("  defaultValue(4) for " & name & ": " &
                 defaultValue); *)
            ELSE
              defaultValue := NIL;
            END;
          ELSIF defConst THEN
            expr := name;
            name := Text.Sub(expr, 0, m);
            defaultValue := Text.Sub(expr, m + 1);
            (* Msg.V("  defaultValue(5) for " & name & ": " & defaultValue); *)
          END;
          (* If there is a default value, it is now contained in defaultValue,
             and name is adapted appropriately. *)
          suf  := Text.Sub(t, k + 1, LAST(CARDINAL));
          IF parameters # NIL THEN
            IF parameters.get(name, val) THEN
              IF val # NIL THEN
                val := SubstituteVariables(val, parameters);
              END;
              IF c = '!' THEN
                IF val = NIL OR Text.Empty(val) THEN
                  val := defaultValue;
                  IF val = NIL OR Text.Empty(val) THEN
                    RAISE Error("mandatory variable " & name & " is empty");
                  END;
                END;
              END;
              pre := pre & val;
            ELSE (* no value found for `name' *)
              IF defaultValue = NIL THEN
                IF c # '?' THEN
                  RAISE Error("mandatory variable " & name & " undefined");
                END;
              ELSE
                pre := pre & defaultValue;
              END;
            END;
          ELSE (* no values at all *)
            IF c # '?' THEN
              RAISE Error("mandatory variable " & name & " undefined");
            END;
          END;
          t := pre & suf;
          i := Text.Length(pre);
        ELSE
          (* no matching '}' found *)
          RAISE Error("syntax error: no matching } in `" & t & "'");
        END;
      ELSE
        (* no valid begin found *)
        INC(i);
      END;
    END;
    (* Msg.V("SubstituteVariables(" & t & ") --> " & t); *)
    RETURN t;
  END SubstituteVariables;

(*---------------------------------------------------------------------------*)
PROCEDURE Pos(READONLY s, t : TEXT; caseSensitive := TRUE) : INTEGER =
  VAR
    ss := s;
    tt := t;
    i  : CARDINAL := 0;
  BEGIN
    IF NOT caseSensitive THEN
      ss := Lower(s);
      tt := Lower(t);
    END;
    IF TextEx.FindSub(ss, tt, i) THEN
      RETURN i;
    ELSE
      RETURN -1;
    END;
  END Pos;

(*---------------------------------------------------------------------------*)
PROCEDURE Contains(READONLY s, t : TEXT; caseSensitive := TRUE) : BOOLEAN =
  BEGIN
    RETURN Pos(s, t, caseSensitive) > -1;
  END Contains;

(*---------------------------------------------------------------------------*)
PROCEDURE StartsWith(s, t : TEXT; caseSensitive := TRUE) : BOOLEAN =
  VAR
    tlen := Text.Length(t);
    slen := Text.Length(s);
    sub: TEXT;
  BEGIN
    IF tlen > slen THEN
      RETURN FALSE;
    END;
    sub := Text.Sub(s, 0, tlen);
    IF caseSensitive THEN
      RETURN Text.Equal(sub, t);
    END;
    RETURN TextEx.CIEqual(sub, t);
  END StartsWith;

(*---------------------------------------------------------------------------*)

PROCEDURE EndsWith(s, t : TEXT; caseSensitive := TRUE) : BOOLEAN =
  VAR
    tlen := Text.Length(t);
    slen := Text.Length(s);
    sub: TEXT;
  BEGIN
    IF tlen > slen THEN
      RETURN FALSE;
    END;
    sub := Text.Sub(s, slen - tlen, tlen);
    IF caseSensitive THEN
      RETURN Text.Equal(sub, t);
    END;
    RETURN TextEx.CIEqual(sub, t);
  END EndsWith;

(*---------------------------------------------------------------------------*)
PROCEDURE BoolVal(READONLY s : TEXT; default := FALSE) : BOOLEAN =
  BEGIN
    WITH t = Compress(s) DO
      IF TextEx.CIEqual(t, "1") THEN
        RETURN TRUE;
      ELSIF TextEx.CIEqual(t, "0") THEN
        RETURN FALSE;
      ELSIF TextEx.CIEqual(t, "on") THEN
        RETURN TRUE;
      ELSIF TextEx.CIEqual(t, "no") THEN
        RETURN FALSE;
      ELSIF TextEx.CIEqual(t, "n") THEN
        RETURN FALSE;
      ELSIF TextEx.CIEqual(t, "yes") THEN
        RETURN TRUE;
      ELSIF TextEx.CIEqual(t, "y") THEN
        RETURN TRUE;
      ELSIF TextEx.CIEqual(t, "off") THEN
        RETURN FALSE;
      ELSIF TextEx.CIEqual(t, "true") THEN
        RETURN TRUE;
      ELSIF TextEx.CIEqual(t, "false") THEN
        RETURN FALSE;
      ELSE
        RETURN default;
      END;
    END;
  END BoolVal;

(*--------------------------------------------------------------------------*)
BEGIN
END TextUtils.

