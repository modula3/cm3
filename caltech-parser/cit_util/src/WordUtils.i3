INTERFACE WordUtils;
IMPORT Word;

TYPE
  T = Word.T;
  Rails = ARRAY OF BOOLEAN;

PROCEDURE FromRails(READONLY rails: Rails): T;

END WordUtils.
