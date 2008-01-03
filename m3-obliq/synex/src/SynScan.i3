(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* Last modified on Fri Nov  5 00:49:32 PST 1993 by luca                       *)

INTERFACE SynScan;
IMPORT SynWr, SynLocation, Rd;

(* Part of the synex package: a parser generator for extensible grammars. *)

(* This interface provides a (somewhat specialized) token scanner.
|
|   The ascii characters are divided into a fixed number of 
|   character classes which default to the following table:
|
|     Blank	HT LF FF CR SP
|     Reserved	" ' ~ DEL
|     Delimiter	( ) , . ; [ ] _ { } ? !
|     Special	# $ % & * + - / : = < > @ \ ^ |
|     Digit	0..9
|     Letter	A..Z ` a..z
|     Illegal	all the others
|
|   Moreover, there are some "improper" character classes:
|
|     StringChar  is either any single character that is not Illegal
|                 or one of `'` `"` `\`, or is one of the pairs of
|		  characters `\'` `\"` `\\`.
|     Comment     is, recursively, a sequence of non-Illegal chars and
|		  Comments enclosed between `(*` and `*)`.
|     Eof	  is the end-of-file token (optionally generated)
|
|  The stream of input characters is split into "lexemes" by always
|  extracting the longest prefix that is a legal lexeme.
|  The following fixed set of lexemes is recognized:
|
|     Space	  a sequence of Blanks and Comments
|     AlphaNum	  a sequence of Letters and Digits starting with a Letter
|     Symbol	  a sequence of Specials
|     Char	  a single StringChars enclosed between two `'`
|     String	  a sequence of StringChars enclosed betweenn two `"`
|     Nat	  a sequence of Digits
|     Int	  a Nat, possibly preceded by a single `~`
|     Real	  an Int followed by `.` and a Nat
|     Delimiter	  a single Delimiter character
|     Eof	  end-of-file (optionally generated)
|
|  Finally, the scanner produces "tokens" from the stream of lexemes:
|
|     - Space lexemes do not produce tokens.
|     - Char, String, Int, Real, Delimiter, and Eof lexemes are also tokens.
|     - AlphaNum and Symbol lexemes are Identifier tokens, except when
|         they have been explicitly declared to be keywoTextCrds, in which
|         case they are Keyword tokens.
|----------------------------------------------------------------------
|  Both Identifier and Keyword tokens are called Name tokens.
|  Name tokens are inserted in an internal symbol table; when they are
|  returned as texts they are unique.
|----------------------------------------------------------------------
|  The scanner scans characters out of a stack of input readers. When a reader
|  is exhausted the scanner switches to the next one in the stack; if the
|  stack is empty the scanner raises NoReader. Initially, the only reader in
|  the stack is stdin, with generateEOF=TRUE (see PushInput); use PopInput
|  right away if you want to remove this reader.
|----------------------------------------------------------------------
|  Here is a typical top-level loop for the scanner:
|
|    SynScan.SetPrompt("- ", "  ");
|    LOOP
|      TRY
|        SynScan.FirstPrompt();
| 	 IF SynScan.GetTokenEof() THEN RAISE SynScan.NoReader END;
|	 (* ... *)
|        (* parse, execute, and print *)
|	 (* ... *)
|    	 SynWr.Flush(swr);
|      EXCEPT
|      | SynScan.Fail => (* Continue. *)
|      | SynScan.NoReader => EXIT;
|      END;
|    END;
*)  

EXCEPTION Fail;

EXCEPTION NoReader;
(* Raised when there is no reader from which to scan characters. *)

TYPE
  T <: REFANY;
  (* A scanner. *)
  
  Keyword <: ROOT;
  (* An AlphanNum or Symbol that has been declared to be a Keyword instead
     of just a Name. *)

  KeywordSet <: ROOT;
  (* A set of keywords. There is a "current" KeywordSet used to tokenize
     the input reader. This can be changed to a different keyword
     set even on-the-fly during scanning. In the latter case one should be 
     aware that there may be a single buffered token recognized according
     to the previous KeywordSet. *)

PROCEDURE Setup();
(* To be called before any other use of this module.
   Initializes the input stack to read from stdin. *)

PROCEDURE New(swr: SynWr.T): T;
(* A brand new scanner sending error messages to a writer "swr".
   Use it single-threaded. You can use separate scanners with separate
   threads, concurrently. *)

PROCEDURE GetWriter(sc: T): SynWr.T;
(* Return the current writer. *)

PROCEDURE Clear(sc: T);
(* Clean up the scanner state, but preserve the input file stack. 
   Buffered characters and tokens are discarded. If scanning from
   stdin, the pending input chars (according to Rd.CharsReady)
   are discarded. *)

PROCEDURE Reset(sc: T);
(* Reinitialize the scanner state. It calls Clear and empties
   the file stack, leaving only the initial stdin input (if any).
   Reset is useful mostly to clear the scanner after
   a scanning or parsing error. *)

TYPE
  CharacterClass =
    {IllegalCharCase, LetterCharCase, DigitCharCase, SpecialCharCase,
     DelimCharCase, ReservedCharCase, BlankCharCase, EofCase };

PROCEDURE SetChar(sc: T; n: CHAR; class: CharacterClass);
(* Assign a character class to a character, for customization.
   Must not modify the Reserved character class. *) 

(* === Source === *)

PROCEDURE PushInput(sc: T; fileName: TEXT; rd: Rd.T; closeReader: BOOLEAN;
  generateEOF: BOOLEAN := TRUE);
(* Switch the scanner input to a new reader, and push the existing
   one on the input stack. Scanning from the old reader will resume from 
   the current position when the new one is exhausted. If closeReader
   is true, the reader is closed when rd is exhausted. If genereteEOF
   is true, an Eof token is generated when rd is exhausted.
   It is admissible to give stdin as "rd", provided that its given
   fileName is "", and that it is the first reader ever pushed. *)

PROCEDURE PopInput(sc: T) RAISES {NoReader};
(* Discards the current reader and switches back to the previous 
   reader in the input stack. Raisse NoReader if there is no
   previous reader. Closes the current reader, if it was so requested
   at PushInput time. An Eof token is not generated, even if it was
   so requested at PushInput time. *)

PROCEDURE ScanPoint(sc: T): INTEGER;
  (* The number of tokens read so far. Useful in LL(1) parsers to detect
     illegal backtracking. *)

PROCEDURE CurrentLocationInfo(sc: T; VAR(*out*) info: SynLocation.Info);
(* Get the fileName and position of the current input reader. *)

PROCEDURE SetCharNo(sc: T; charNo, lineNo, lineCharNo: INTEGER);
(* Set the charNo counter associtated with the current input reader to the
   given number, for error reporting purposes. By default, charNo is 
   initialized to 0 when a reader is opened, and incremented every time a 
   new character is scanned. Similaraly, lineNo is incremented at each line,
   and lineCharNo is incremented at each character and reset to 0 at each 
   line. *)

PROCEDURE TopLevel(sc: T): BOOLEAN;
(* Whether the scanner is reading from the top level (stdin) or a file. *)

PROCEDURE FlushInput(sc: T);
(* Discard all the pending characters on the current reader
   (as determined by Rd.CharsReady) *)

PROCEDURE SetPrompt(sc: T; firstPrompt, nextPrompt: TEXT);
(* Set the top-level interaction prompt strings. The firstPrompt will be 
   generated once, then the nextPrompt all the other times, until FirstPrompt 
   is explicitly invoked. *)

PROCEDURE FirstPrompt(sc: T);
(* Reset the first prompt (see SetPrompt). *)

(* === Keywords === *)

PROCEDURE NewKeywordSet(): KeywordSet;
(* Create a new keywordSet. *)

PROCEDURE CopyKeywordSet(keywordSet: KeywordSet): KeywordSet;
(* Create a copy of keywordSet. *)

PROCEDURE GetKeywordSet(sc: T): KeywordSet;
(* Get the current keyword set (initially an empty one). *)

PROCEDURE UseKeywordSet(sc: T; keywordSet: KeywordSet);
(* From now on, use this keywordSet for scanning. *)

PROCEDURE BeKeyword(ide: TEXT; keywordSet: KeywordSet): Keyword;
(* From now on, the name "ide" is a keyword member of keywordSet.
   The (new or existing) keyword is returned. *)

PROCEDURE GetKeyword(ide: TEXT; keywordSet: KeywordSet): Keyword;
(* Returns the keyword of name "ide" from keywordSet, or NIL
   if it does not exist. *)

PROCEDURE GetKeywordName(key : Keyword): TEXT;
(* Returns the name of a keyword. *)

(* === Token primitives === *)

(* All the Get and Have procedures below "eat" a token if succesful,
   and leave it (buffered) in the input stream if unsuccesful. *)

PROCEDURE IsDelimiter(sc: T; char: CHAR): BOOLEAN;
(* Whether char is a legal Delimiter token. *)

PROCEDURE IsIdentifier(sc: T; string: TEXT): BOOLEAN;
(* Whether string is a legal Identifier (Name or Keyword) token *)

PROCEDURE GetTokenChar(sc: T; VAR (*out*) char: CHAR): BOOLEAN RAISES {NoReader, Fail};
(* Returns the value of the next (char) token (or FALSE) *)

PROCEDURE GetTokenNat(sc: T; VAR (*out*) nat: CARDINAL): BOOLEAN RAISES {NoReader, Fail};
(* Returns the value of the next (natural) token (or FALSE) *)

PROCEDURE GetTokenInt(sc: T; VAR (*out*) int: INTEGER): BOOLEAN RAISES {NoReader, Fail};
(* Returns the value of the next (integer) token (or FALSE) *)

PROCEDURE GetTokenReal(sc: T; VAR (*ou*) real: LONGREAL): BOOLEAN RAISES {NoReader, Fail};
(* Returns the value of the next (real) token (or FALSE) *)

PROCEDURE GetTokenString(sc: T; VAR (*out*) string: TEXT): BOOLEAN RAISES {NoReader, Fail};
(* Returns the value of the next (string) token (or FALSE) *)

PROCEDURE GetTokenIde(sc: T; VAR (*ou*) ide: TEXT): BOOLEAN RAISES {NoReader, Fail};
(* Returns the value of the next non-keyword identifier (or FALSE).
   If two calls to GetTokenIde return two texts that are "Text.Equal",
   then they are also "=". *)

PROCEDURE GetTokenName(sc: T; VAR (*ou*) text: TEXT): BOOLEAN RAISES {NoReader, Fail};
(* Returns the value of the next keyword or non-keyword identifier 
  (or FALSE). If two calls to GetTokenName return two texts that are 
  "Text.Equal", then they are also "=". *)

PROCEDURE GetTokenEof(sc: T; ): BOOLEAN RAISES {NoReader, Fail};
(* Returns TRUE if we have reached the end of a file for which
   and end-of-file token was requested, FALSE otherwise *)

PROCEDURE HaveTokenIde(sc: T; ide: TEXT): BOOLEAN RAISES {NoReader, Fail};
(* Tests the presence of a given non-keyword identifier. 
   Returns FALSE if not found *)

PROCEDURE HaveTokenKey(sc: T; key: TEXT): BOOLEAN RAISES {NoReader, Fail};
(* Tests the presence of a given keyword. Returns FALSE if not found *)

PROCEDURE HaveTokenName(sc: T; text: TEXT): BOOLEAN RAISES {NoReader, Fail};
(* Tests the presence of a given keyword or non-keyword identifer. 
   Returns FALSE if not found *)

PROCEDURE HaveTokenDelim(sc: T; delim: CHAR): BOOLEAN RAISES {NoReader, Fail};
(* Tests the presence of a given delimiter. Returns FALSE if not found *)

(* === Error Messages === *)

TYPE ErrorReportStyle = {LinePlusChar, CharRange};
(* Whether Syntax should report error positions by line number plus char in 
   line, or by range of characters from the beginning of file. *)

PROCEDURE PrintContext(sc: T);
(* Prints the current input context (a short segment of the input stream 
   following the current scanner position). This operation corrupts the
   input stream: it should be used after a syntactic error, as part
   of the error message, before resetting the scanner*)

PROCEDURE ErrorMsg(sc: T; msg: TEXT := "");
(* Prints msg (followed by newline if non-empty), and calls Reset. *)

PROCEDURE SyntaxMsg(sc: T; cause: TEXT := ""; culprit: TEXT := "";
  errorReportStyle: ErrorReportStyle := ErrorReportStyle.LinePlusChar);
(* A more elaborate version of "Error". It uses PrintContext and
   SynLocation.PrintLocation to show the error context in a meaninglful
   way. "causes" should specify the general reason of the error, and 
   "culprit" the specific offender. Prints error positions according to 
   "errorReportStyle". *)

END SynScan.
