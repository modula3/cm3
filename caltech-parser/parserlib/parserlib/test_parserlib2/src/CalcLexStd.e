%source Calc.t Calc.l
%import CalcTokStd CalcLex

%module{
IMPORT Text;
}

LETTER		{$R LETTER{$$ := Text.GetChar($,0)}}
DIGIT		{$R DIGIT{$$ := ORD(Text.GetChar($,0)) - ORD('0')}}