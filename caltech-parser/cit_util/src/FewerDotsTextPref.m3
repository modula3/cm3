MODULE FewerDotsTextPref;
IMPORT TextEquivalence;
IMPORT TextUtils;
IMPORT Integer;
IMPORT Text;

REVEAL
  T = TextEquivalence.Preference BRANDED "FewerDotsIsBetter" OBJECT
  OVERRIDES
    is := Is;
  END;

PROCEDURE CountDots(t: TEXT): CARDINAL =
  BEGIN
    RETURN TextUtils.CountCharOccurences(t, '.');
  END CountDots;

PROCEDURE Is(<*UNUSED*>self: T;
             thisBetter, thanThis: TEXT): BOOLEAN =
  BEGIN
    CASE Integer.Compare(CountDots(thisBetter), CountDots(thanThis)) OF
    | -1 => RETURN TRUE;
    | 1 => RETURN FALSE;
    | 0 => RETURN Text.Length(thisBetter) < Text.Length(thanThis);
    END;
  END Is;

BEGIN
  instance := NEW(T);
END FewerDotsTextPref.
