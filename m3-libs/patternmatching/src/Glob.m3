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
 * $Id: Glob.m3,v 1.1 2001-12-01 16:07:31 wagner Exp $ *)

UNSAFE MODULE Glob;

IMPORT Ctypes, M3toC, Uglob, Word;
IMPORT RegEx;

PROCEDURE Match(pattern, string: TEXT;
                options := MatchOptions{}): BOOLEAN RAISES {RegEx.Error} =
  VAR
    flags: Ctypes.int := 0;
    patternStr: Ctypes.char_star; 
    stringStr: Ctypes.char_star;
    res: BOOLEAN;
  BEGIN
    IF MatchOption.UseSimpleRE IN options THEN
      VAR
        rePattern : RegEx.Pattern;
      BEGIN
        rePattern := RegEx.Compile(pattern);
        RETURN RegEx.Execute(rePattern, string) > -1;
      END;
    END;
    patternStr := M3toC.SharedTtoS(pattern); 
    stringStr := M3toC.SharedTtoS(string);
    IF MatchOption.NoEscape IN options THEN
      flags := Word.Or(flags, Uglob.FNM_NOESCAPE);
    END;
    IF MatchOption.Pathname IN options THEN
      flags := Word.Or(flags, Uglob.FNM_PATHNAME);
    END;
    IF MatchOption.Period IN options THEN
      flags := Word.Or(flags, Uglob.FNM_PERIOD);
    END;
    IF MatchOption.LeadingDir IN options THEN
      flags := Word.Or(flags, Uglob.FNM_LEADING_DIR);
    END;
    IF MatchOption.CaseFold IN options THEN
      flags := Word.Or(flags, Uglob.FNM_CASEFOLD);
    END;
    IF MatchOption.PrefixDirs IN options THEN
      flags := Word.Or(flags, Uglob.FNM_PREFIX_DIRS);
    END;

    res := Uglob.fnmatch(patternStr, stringStr, flags) = 0;
    M3toC.FreeSharedS(pattern, patternStr);
    M3toC.FreeSharedS(string, stringStr);
    RETURN res;
  END Match;

BEGIN
END Glob.
