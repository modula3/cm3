%macro {
D                       [0-9]
L                       [a-zA-Z_]
}

%expr {
RULE			{D}*":="
MACRO			({D}|{L})*":="
COUNT			\{{D}*","?{D}*\}
IDENTIFIER		\{"%"?({D}|{L})*\}
brac_CHAR_RANGE 	\[(\\.|[^\\\]])*\]
dot_CHAR_RANGE		\.
quote_STRING		\"(\\.|[^\\"])*\"
bare_STRING		({D}|{L}|(\\.))*

ASSIGN			":="
PROD			"->"

char			{%char}
default			[^]
}
