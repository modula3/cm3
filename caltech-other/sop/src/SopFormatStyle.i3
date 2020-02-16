INTERFACE SopFormatStyle;

TYPE
  T = RECORD
    andSym, orSym, notSym: TEXT;
  END;

CONST
  C  = T{" & ", " | ", "~"};
  M3 = T{" AND ", " OR ", "NOT "};

END SopFormatStyle.
