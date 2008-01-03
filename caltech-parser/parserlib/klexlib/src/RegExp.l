%macro {
D                       [0-9]
L                       [a-zA-Z_]
}

%expr {
COUNT                   \{{D}*","?{D}*\}
IDENTIFIER              \{"%"?({D}|{L})*\}
brac_CHAR_RANGE         \[(\\.|[^\\\]])*\]
dot_CHAR_RANGE          \.
STRING                  ({D}|{L}|(\\.))*|(\"(\\.|[^\\"])*\")

char			{%char}
skip			[\t\040]
ERROR			[^]
}