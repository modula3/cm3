(*--------------------------------------------------------------------------*)
MODULE Release;

IMPORT
  ASCII, Fingerprint, SMsg AS Msg, Rd, Scan, Text, TextUtils,
  TextReadingUtils, TextRd;

(*--------------------------------------------------------------------------*)
CONST
  Delim = ASCII.DEL;

VAR
  version := ARRAY [1..24] OF CHAR {
    Delim, Delim,
    'D', 'e', 'v', 'e', 'l', 'o', 'p', 'm', 'e', 'n', 't', ' ',
    'V', 'e', 'r', 's', 'i', 'o', 'n', ' ',
    Delim, Delim
  };
    
  licenseNo := ARRAY [1..14] OF CHAR {
    Delim, Delim,
    ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '4', '2',
    Delim, Delim
  };

(*   1    2    3    4    5    6    7    8    9    0    1    2 *)

  valid := ARRAY [1..124] OF CHAR {
    Delim, Delim,
    '0', '1', '4', '2', '9', '3', '2', '0', '9', '5', '4', ' ',
    '-', '1', '0', '9', '2', '1', '4', '4', '0', '2', '3', ' ',
    '0', '1', '4', '7', '9', '6', '5', '6', '7', '9', '6', ' ',
    '-', '0', '5', '5', '8', '7', '6', '2', '3', '9', '6', ' ',
    '-', '0', '4', '4', '8', '6', '1', '6', '0', '5', '2', ' ',
    ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',
    ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',
    ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',
    ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',
    ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',
    Delim, Delim
  };

  key: ARRAY[1..10] OF INTEGER;

  keys: INTEGER;

(*--------------------------------------------------------------------------*)
PROCEDURE ComPactName(): TEXT =
  CONST SP = " ";
  BEGIN
    RETURN E & l & e & g & o & SP & C & o & m & P & a & c & t & SP &
           F & r & e & e ;
  END ComPactName;

VAR (* we just order the needed characters to make it not too easy to
       change the text *) 
  a := "a"; <* NOWARN *>
  C := "C"; <* NOWARN *>
  c := "c"; <* NOWARN *>
  D := "D"; <* NOWARN *>
  E := "E"; <* NOWARN *>
  e := "e"; <* NOWARN *>
  F := "F"; <* NOWARN *>
  g := "g"; <* NOWARN *>
  h := "h"; <* NOWARN *>
  i := "i"; <* NOWARN *>
  L := "L"; <* NOWARN *>
  l := "l"; <* NOWARN *>
  m := "m"; <* NOWARN *>
  o := "o"; <* NOWARN *>
  P := "P"; <* NOWARN *>
  r := "r"; <* NOWARN *>
  s := "s"; <* NOWARN *>
  t := "t"; <* NOWARN *>
  u := "u"; <* NOWARN *>
  v := "v"; <* NOWARN *>

(*--------------------------------------------------------------------------*)
PROCEDURE VersionToString(): TEXT =
  VAR
    t := TextUtils.Compress(Text.FromChars(version), ASCII.Set {Delim});
  BEGIN
    RETURN TextUtils.Compress(t);
  END VersionToString;

(*--------------------------------------------------------------------------*)
PROCEDURE LicenseNumberToString(): TEXT =
  VAR
    t := TextUtils.Compress(Text.FromChars(licenseNo), ASCII.Set {Delim});
  BEGIN
    RETURN TextUtils.Compress(t);
  END LicenseNumberToString;

(*--------------------------------------------------------------------------*)
PROCEDURE ComPactVersionText(): TEXT =
  BEGIN
    RETURN ComPactName() & " " & VersionToString() & "   #" & 
           LicenseNumberToString();
  END ComPactVersionText;

(*--------------------------------------------------------------------------*)
PROCEDURE Show() =
  VAR
    memo := Msg.tFlag;
  BEGIN
    Msg.tFlag := TRUE;
    Msg.T(ComPactVersionText());
    Msg.tFlag := memo;
  END Show;

(*--------------------------------------------------------------------------*)
PROCEDURE KeyCheck(pass: TEXT): BOOLEAN =
  VAR
    fl := Fingerprint.FromText(LicenseNumberToString());
    fp := Fingerprint.FromText(pass);
  BEGIN
    FOR i := 1 TO keys DO
      IF key[i] = Fingerprint.Hash(Fingerprint.Combine(fl, fp)) THEN
        RETURN TRUE;
      END;
    END;
    RETURN FALSE;
  END KeyCheck;

(*--------------------------------------------------------------------------*)
BEGIN
  VAR
    trd := TextRd.New(
               TextUtils.Compress(Text.FromChars(valid), ASCII.Set {Delim}));
    tok: TEXT;
    n: INTEGER;
    done := FALSE;
  BEGIN
    keys := 0;
    REPEAT
      TRY
        tok := TextReadingUtils.GetToken(trd);
      EXCEPT
        Rd.Failure, Rd.EndOfFile => done := TRUE;
      ELSE
      END;
      TRY
        n := Scan.Int(tok);
      EXCEPT ELSE
      END;
      INC(keys);
      key[keys] := n;
    UNTIL done;
  END;
END Release.
