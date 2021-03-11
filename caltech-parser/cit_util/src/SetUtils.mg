GENERIC MODULE SetUtils(Elem);
IMPORT TextList;
IMPORT TextUtils;

PROCEDURE Format(set: T; postDelim:=" "; skipLastDelim:=TRUE): TEXT =
  VAR
    iter := set.iterate();
    elem: Elem.T;
    res: TextList.T := NIL;
  BEGIN
    WHILE iter.next(elem) DO
      res := TextList.Cons(Elem.Format(elem), res);
    END;
    RETURN TextUtils.Assemble(res, postDelim, skipLastDelim);
  END Format;

BEGIN
END SetUtils.
