%source RegExp.t RegExp.l
%import RegExpTok RegExpLex

%interface{
IMPORT Interval;
IMPORT CharRange;
}

COUNT:		  {val: Interval.T}
IDENTIFIER:	  {val: TEXT}
CHAR_RANGE:	  {val: CharRange.T}
STRING:		  {val: TEXT}

COUNT             {$R COUNT{$$ := ParseInterval($)}}
IDENTIFIER        {$R IDENTIFIER{$$ := $}}
brac_CHAR_RANGE   {$R CHAR_RANGE{$$ := CharRange.FromText($)}}
dot_CHAR_RANGE    {$R CHAR_RANGE{$$ := CharRange.AllExceptNewline}}
STRING		  {$R STRING{$$ := CharCodes.ParseString($)}}

%module{
IMPORT Interval;
IMPORT NFA;
IMPORT Scan;
IMPORT Text;
IMPORT CharRange;
IMPORT CharCodes;
IMPORT FloatMode, Lex;
<* FATAL FloatMode.Trap, Lex.Error *>

PROCEDURE ParseInterval(t: TEXT): Interval.T =
  VAR
    inter: TEXT := CharCodes.StripDelims(t);
    pos := Text.FindChar(inter, ',');
    loStr,hiStr: TEXT;
  PROCEDURE GetInt(str: TEXT; default: INTEGER): INTEGER =
    BEGIN
      IF Text.Length(str) = 0 THEN
        RETURN default;
      ELSE
        RETURN Scan.Int(str);
      END;
    END GetInt;
  BEGIN
    IF pos = -1 THEN
      loStr := inter;
      hiStr := inter;
    ELSE
      loStr := Text.Sub(inter, 0, pos);
      hiStr := Text.Sub(inter, pos+1, LAST(INTEGER));
    END;
(*    Term.WrLn("lo = " & loStr & ", hi = " & hiStr); *)
    RETURN Interval.T{GetInt(loStr,0), GetInt(hiStr,NFA.OrMore)};
  END ParseInterval;
}