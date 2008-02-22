(*--------------------------------------------------------------------------*)
MODULE FingerprintFmt;

IMPORT Fingerprint, Text;
IMPORT TextUtils;

(*--------------------------------------------------------------------------*)
PROCEDURE Hex(fp : Fingerprint.T) : TEXT =

  CONST digit = ARRAY OF CHAR {'0', '1', '2', '3', '4', '5', '6', '7',
                               '8', '9', 'A', 'B', 'C', 'D', 'E', 'F'};
  VAR res : ARRAY [0..15] OF CHAR;
  BEGIN
    FOR b := 0 TO 7 DO
      WITH byte = fp.byte[b] DO
        WITH h = byte DIV 16, l = byte MOD 16 DO
          res[2*b + 0] := digit[h];
          res[2*b + 1] := digit[l];
        END;
      END;
    END;
    RETURN Text.FromChars(res);
  END Hex;

(*--------------------------------------------------------------------------*)
PROCEDURE Scan(t : TEXT; VAR fp : Fingerprint.T) : BOOLEAN =
  CONST 
    digits = "0123456789ABCDEF";
  VAR
    tfp := TextUtils.Compress(t);
    len := Text.Length(tfp);
  BEGIN
    IF len # 16 THEN RETURN FALSE END;
    FOR i := 0 TO 7 DO
      WITH h = Text.GetChar(tfp, 2*i), l = Text.GetChar(tfp, 2*i + 1) DO
        WITH hh = Text.FindChar(digits, h),
             ll = Text.FindChar(digits, l) DO
          IF hh = -1 OR ll = -1 THEN
            RETURN FALSE;
          END;
          fp.byte[i] := 16 * hh + ll;
        END;
      END;
    END;
    RETURN TRUE;
  END Scan;
     

BEGIN
END FingerprintFmt.
