%source test.t test.l
%import testTok testLex

RULE	{$R RULE{$$ := Scan.Int(Text.Sub($, 0, Text.Length($)-2))}}
MACRO	{$R MACRO{$$ := Text.Sub($, 0, Text.Length($)-2))}}
COUNT	{$R COUNT{$$ := ParseInterval($))}}
dot_CHAR_RANGE	{$R CHAR_RANGE}