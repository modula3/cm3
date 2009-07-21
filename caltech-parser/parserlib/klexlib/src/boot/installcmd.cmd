set COPYCMD=/y
copy ..\src\boot\*3 .

echo MODULE RegExpLex; >RegExpLex.m3
echo IMPORT FileRdErr AS SeekRd; >>RegExpLex.m3
findstr /v /c:"SeekRd.DiscardPrevious(self.rd);" /c:"IMPORT SeekRd;" /c:"MODULE RegExpLex;" ..\src\boot\RegExpLex.m3 >>RegExpLex.m3
