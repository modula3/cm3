(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Mon Jul 25 13:57:20 PDT 1994 by najork                   *)
(*       Created on Mon Jul 25 13:25:14 PDT 1994 by najork                   *)


MODULE ObKeySym;

IMPORT Latin1Key, ObValue, Obliq, Text, VBT;

PROCEDURE M3ToObliq (k : VBT.KeySym) : ObValue.Val =
  VAR
    t : TEXT;
  BEGIN
    CASE k OF
    | Latin1Key.space        => t := " ";
    | Latin1Key.exclam       => t := "!";
    | Latin1Key.quotedbl     => t := "\"";
    | Latin1Key.numbersign   => t := "#";
    | Latin1Key.dollar       => t := "$";
    | Latin1Key.percent      => t := "%";
    | Latin1Key.ampersand    => t := "&";
    | Latin1Key.apostrophe   => t := "'";
    | Latin1Key.parenleft    => t := "(";
    | Latin1Key.parenright   => t := ")";
    | Latin1Key.asterisk     => t := "*";
    | Latin1Key.plus         => t := "+";
    | Latin1Key.comma        => t := ",";
    | Latin1Key.minus        => t := "-";
    | Latin1Key.period       => t := ".";
    | Latin1Key.slash        => t := "/";
    | Latin1Key.zero         => t := "0";
    | Latin1Key.one          => t := "1";
    | Latin1Key.two          => t := "2";
    | Latin1Key.three        => t := "3";
    | Latin1Key.four         => t := "4";
    | Latin1Key.five         => t := "5";
    | Latin1Key.six          => t := "6";
    | Latin1Key.seven        => t := "7";
    | Latin1Key.eight        => t := "8";
    | Latin1Key.nine         => t := "9";
    | Latin1Key.colon        => t := ":";
    | Latin1Key.semicolon    => t := ";";
    | Latin1Key.less         => t := "<";
    | Latin1Key.equal        => t := "=";
    | Latin1Key.greater      => t := ">";
    | Latin1Key.question     => t := "?";
    | Latin1Key.at           => t := "@";
    | Latin1Key.A            => t := "A";
    | Latin1Key.B            => t := "B";
    | Latin1Key.C            => t := "C";
    | Latin1Key.D            => t := "D";
    | Latin1Key.E            => t := "E";
    | Latin1Key.F            => t := "F";
    | Latin1Key.G            => t := "G";
    | Latin1Key.H            => t := "H";
    | Latin1Key.I            => t := "I";
    | Latin1Key.J            => t := "J";
    | Latin1Key.K            => t := "K";
    | Latin1Key.L            => t := "L";
    | Latin1Key.M            => t := "M";
    | Latin1Key.N            => t := "N";
    | Latin1Key.O            => t := "O";
    | Latin1Key.P            => t := "P";
    | Latin1Key.Q            => t := "Q";
    | Latin1Key.R            => t := "R";
    | Latin1Key.S            => t := "S";
    | Latin1Key.T            => t := "T";
    | Latin1Key.U            => t := "U";
    | Latin1Key.V            => t := "V";
    | Latin1Key.W            => t := "W";
    | Latin1Key.X            => t := "X";
    | Latin1Key.Y            => t := "Y";
    | Latin1Key.Z            => t := "Z";
    | Latin1Key.bracketleft  => t := "[";
    | Latin1Key.backslash    => t := "\\";
    | Latin1Key.bracketright => t := "]";
    | Latin1Key.asciicircum  => t := "";
    | Latin1Key.underscore   => t := "_";
    | Latin1Key.grave        => t := "`";
    | Latin1Key.a            => t := "a";
    | Latin1Key.b            => t := "b";
    | Latin1Key.c            => t := "c";
    | Latin1Key.d            => t := "d";
    | Latin1Key.e            => t := "e";
    | Latin1Key.f            => t := "f";
    | Latin1Key.g            => t := "g";
    | Latin1Key.h            => t := "h";
    | Latin1Key.i            => t := "i";
    | Latin1Key.j            => t := "j";
    | Latin1Key.k            => t := "k";
    | Latin1Key.l            => t := "l";
    | Latin1Key.m            => t := "m";
    | Latin1Key.n            => t := "n";
    | Latin1Key.o            => t := "o";
    | Latin1Key.p            => t := "p";
    | Latin1Key.q            => t := "q";
    | Latin1Key.r            => t := "r";
    | Latin1Key.s            => t := "s";
    | Latin1Key.t            => t := "t";
    | Latin1Key.u            => t := "u";
    | Latin1Key.v            => t := "v";
    | Latin1Key.w            => t := "w";
    | Latin1Key.x            => t := "x";
    | Latin1Key.y            => t := "y";
    | Latin1Key.z            => t := "z";
    | Latin1Key.braceleft    => t := "{";
    | Latin1Key.bar          => t := "|";
    | Latin1Key.braceright   => t := "}";
    | Latin1Key.asciitilde   => t := "~";
    ELSE
      t := "Other";
    END;
    RETURN Obliq.NewText (t);
  END M3ToObliq;


PROCEDURE ObliqToM3 (val : ObValue.Val) : VBT.KeySym RAISES {ObValue.Error} =
  BEGIN
    WITH t = Obliq.ToText (val) DO 
      IF Text.Equal (t, " ") THEN
        RETURN Latin1Key.space;
      ELSIF Text.Equal (t, "!") THEN
        RETURN Latin1Key.exclam;
      ELSIF Text.Equal (t, "\"") THEN
        RETURN Latin1Key.quotedbl;
      ELSIF Text.Equal (t, "#") THEN
        RETURN Latin1Key.numbersign;
      ELSIF Text.Equal (t, "$") THEN
        RETURN Latin1Key.dollar;
      ELSIF Text.Equal (t, "%") THEN
        RETURN Latin1Key.percent;
      ELSIF Text.Equal (t, "&") THEN
        RETURN Latin1Key.ampersand;
      ELSIF Text.Equal (t, "'") THEN
        RETURN Latin1Key.apostrophe;
      ELSIF Text.Equal (t, "(") THEN
        RETURN Latin1Key.parenleft;
      ELSIF Text.Equal (t, ")") THEN
        RETURN Latin1Key.parenright;
      ELSIF Text.Equal (t, "*") THEN
        RETURN Latin1Key.asterisk;
      ELSIF Text.Equal (t, "+") THEN
        RETURN Latin1Key.plus;
      ELSIF Text.Equal (t, ",") THEN
        RETURN Latin1Key.comma;
      ELSIF Text.Equal (t, "-") THEN
        RETURN Latin1Key.minus;
      ELSIF Text.Equal (t, ".") THEN
        RETURN Latin1Key.period;
      ELSIF Text.Equal (t, "/") THEN
        RETURN Latin1Key.slash;
      ELSIF Text.Equal (t, "0") THEN
        RETURN Latin1Key.zero;
      ELSIF Text.Equal (t, "1") THEN
        RETURN Latin1Key.one;
      ELSIF Text.Equal (t, "2") THEN
        RETURN Latin1Key.two;
      ELSIF Text.Equal (t, "3") THEN
        RETURN Latin1Key.three;
      ELSIF Text.Equal (t, "4") THEN
        RETURN Latin1Key.four;
      ELSIF Text.Equal (t, "5") THEN
        RETURN Latin1Key.five;
      ELSIF Text.Equal (t, "6") THEN
        RETURN Latin1Key.six;
      ELSIF Text.Equal (t, "7") THEN
        RETURN Latin1Key.seven;
      ELSIF Text.Equal (t, "8") THEN
        RETURN Latin1Key.eight;
      ELSIF Text.Equal (t, "9") THEN
        RETURN Latin1Key.nine;
      ELSIF Text.Equal (t, ":") THEN
        RETURN Latin1Key.colon;
      ELSIF Text.Equal (t, ";") THEN
        RETURN Latin1Key.semicolon;
      ELSIF Text.Equal (t, "<") THEN
        RETURN Latin1Key.less;
      ELSIF Text.Equal (t, "=") THEN
        RETURN Latin1Key.equal;
      ELSIF Text.Equal (t, ">") THEN
        RETURN Latin1Key.greater;
      ELSIF Text.Equal (t, "?") THEN
        RETURN Latin1Key.question;
      ELSIF Text.Equal (t, "@") THEN
        RETURN Latin1Key.at;
      ELSIF Text.Equal (t, "A") THEN
        RETURN Latin1Key.A;
      ELSIF Text.Equal (t, "B") THEN
        RETURN Latin1Key.B;
      ELSIF Text.Equal (t, "C") THEN
        RETURN Latin1Key.C;
      ELSIF Text.Equal (t, "D") THEN
        RETURN Latin1Key.D;
      ELSIF Text.Equal (t, "E") THEN
        RETURN Latin1Key.E;
      ELSIF Text.Equal (t, "F") THEN
        RETURN Latin1Key.F;
      ELSIF Text.Equal (t, "G") THEN
        RETURN Latin1Key.G;
      ELSIF Text.Equal (t, "H") THEN
        RETURN Latin1Key.H;
      ELSIF Text.Equal (t, "I") THEN
        RETURN Latin1Key.I;
      ELSIF Text.Equal (t, "J") THEN
        RETURN Latin1Key.J;
      ELSIF Text.Equal (t, "K") THEN
        RETURN Latin1Key.K;
      ELSIF Text.Equal (t, "L") THEN
        RETURN Latin1Key.L;
      ELSIF Text.Equal (t, "M") THEN
        RETURN Latin1Key.M;
      ELSIF Text.Equal (t, "N") THEN
        RETURN Latin1Key.N;
      ELSIF Text.Equal (t, "O") THEN
        RETURN Latin1Key.O;
      ELSIF Text.Equal (t, "P") THEN
        RETURN Latin1Key.P;
      ELSIF Text.Equal (t, "Q") THEN
        RETURN Latin1Key.Q;
      ELSIF Text.Equal (t, "R") THEN
        RETURN Latin1Key.R;
      ELSIF Text.Equal (t, "S") THEN
        RETURN Latin1Key.S;
      ELSIF Text.Equal (t, "T") THEN
        RETURN Latin1Key.T;
      ELSIF Text.Equal (t, "U") THEN
        RETURN Latin1Key.U;
      ELSIF Text.Equal (t, "V") THEN
        RETURN Latin1Key.V;
      ELSIF Text.Equal (t, "W") THEN
        RETURN Latin1Key.W;
      ELSIF Text.Equal (t, "X") THEN
        RETURN Latin1Key.X;
      ELSIF Text.Equal (t, "Y") THEN
        RETURN Latin1Key.Y;
      ELSIF Text.Equal (t, "Z") THEN
        RETURN Latin1Key.Z;
      ELSIF Text.Equal (t, "[") THEN
        RETURN Latin1Key.bracketleft;
      ELSIF Text.Equal (t, "\\") THEN
        RETURN Latin1Key.backslash;
      ELSIF Text.Equal (t, "]") THEN
        RETURN Latin1Key.bracketright;
      ELSIF Text.Equal (t, "") THEN
        RETURN Latin1Key.asciicircum;
      ELSIF Text.Equal (t, "_") THEN
        RETURN Latin1Key.underscore;
      ELSIF Text.Equal (t, "`") THEN
        RETURN Latin1Key.grave;
      ELSIF Text.Equal (t, "a") THEN
        RETURN Latin1Key.a;
      ELSIF Text.Equal (t, "b") THEN
        RETURN Latin1Key.b;
      ELSIF Text.Equal (t, "c") THEN
        RETURN Latin1Key.c;
      ELSIF Text.Equal (t, "d") THEN
        RETURN Latin1Key.d;
      ELSIF Text.Equal (t, "e") THEN
        RETURN Latin1Key.e;
      ELSIF Text.Equal (t, "f") THEN
        RETURN Latin1Key.f;
      ELSIF Text.Equal (t, "g") THEN
        RETURN Latin1Key.g;
      ELSIF Text.Equal (t, "h") THEN
        RETURN Latin1Key.h;
      ELSIF Text.Equal (t, "i") THEN
        RETURN Latin1Key.i;
      ELSIF Text.Equal (t, "j") THEN
        RETURN Latin1Key.j;
      ELSIF Text.Equal (t, "k") THEN
        RETURN Latin1Key.k;
      ELSIF Text.Equal (t, "l") THEN
        RETURN Latin1Key.l;
      ELSIF Text.Equal (t, "m") THEN
        RETURN Latin1Key.m;
      ELSIF Text.Equal (t, "n") THEN
        RETURN Latin1Key.n;
      ELSIF Text.Equal (t, "o") THEN
        RETURN Latin1Key.o;
      ELSIF Text.Equal (t, "p") THEN
        RETURN Latin1Key.p;
      ELSIF Text.Equal (t, "q") THEN
        RETURN Latin1Key.q;
      ELSIF Text.Equal (t, "r") THEN
        RETURN Latin1Key.r;
      ELSIF Text.Equal (t, "s") THEN
        RETURN Latin1Key.s;
      ELSIF Text.Equal (t, "t") THEN
        RETURN Latin1Key.t;
      ELSIF Text.Equal (t, "u") THEN
        RETURN Latin1Key.u;
      ELSIF Text.Equal (t, "v") THEN
        RETURN Latin1Key.v;
      ELSIF Text.Equal (t, "w") THEN
        RETURN Latin1Key.w;
      ELSIF Text.Equal (t, "x") THEN
        RETURN Latin1Key.x;
      ELSIF Text.Equal (t, "y") THEN
        RETURN Latin1Key.y;
      ELSIF Text.Equal (t, "z") THEN
        RETURN Latin1Key.z;
      ELSIF Text.Equal (t, "{") THEN
        RETURN Latin1Key.braceleft;
      ELSIF Text.Equal (t, "|") THEN
        RETURN Latin1Key.bar;
      ELSIF Text.Equal (t, "}") THEN
        RETURN Latin1Key.braceright;
      ELSIF Text.Equal (t, "~") THEN
        RETURN Latin1Key.asciitilde;
      ELSE 
        Obliq.RaiseError ("Unknown KeySym");
        RETURN VBT.NoKey;
      END;
    END;
  END ObliqToM3;


BEGIN
END ObKeySym.
