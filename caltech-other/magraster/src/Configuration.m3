(* $Id$ *)

MODULE Configuration;
IMPORT Rd;
IMPORT Text, TextList, Fmt, TextReader, Debug;
IMPORT Tint;
IMPORT MagLayer AS Layer, TextMagLayerTbl AS TextLayerTbl;
IMPORT Thread;
IMPORT PaintLayer, BaseLayer, BaseLayerList, PaintLayerClass;

<* FATAL Rd.Failure, Thread.Alerted *>

PROCEDURE LookupBaseLayers(tbl : TextLayerTbl.T; layerNameList : TextList.T) :
  BaseLayerList.T =
  VAR
    res : BaseLayerList.T := NIL;
    base : Layer.T;
  BEGIN
    WHILE layerNameList # NIL DO
      IF tbl.get(layerNameList.head, base) THEN
        res := BaseLayerList.Cons(base, res)
      ELSE
        Debug.Error("Base layer \"" & layerNameList.head & "\" not found.")
      END;
      layerNameList := layerNameList.tail
    END;
    RETURN res
  END LookupBaseLayers;

PROCEDURE ReadConf(rd : Rd.T) : TextLayerTbl.T =

  PROCEDURE AddAliases( l : Layer.T; aliasList : TextList.T) =
    BEGIN
      WHILE aliasList # NIL DO
        EVAL res.put(aliasList.head, l);
        aliasList := aliasList.tail
      END;
    END AddAliases;

  VAR
    lineno := 1;
    priority := 0;
    res := NEW(TextLayerTbl.Default).init();
  BEGIN
    TRY
      LOOP
        VAR
          line := Rd.GetLine(rd);
          textReader := NEW(TextReader.T).init(line);
          keyWord : TEXT;
        BEGIN
          IF Text.Length(line) > 0 AND Text.GetChar(line,0) # '#' THEN
            keyWord := textReader.nextE(" ");
            IF Text.Equal(keyWord, "") THEN

            ELSIF Text.Equal(keyWord, "BASIC") THEN
              VAR
                aliasList := textReader.shatter(" ,", ":", skipNulls := TRUE);
                tint := Tint.Parse(textReader.nextE(":"));
                layer := NEW(BaseLayer.T, 
                             tint := tint, 
                             priority := priority).init(aliasList.head);
              BEGIN
                Debug.Out("Reading BASIC: " & line,100);
                AddAliases(layer, aliasList);
                INC(priority)
              END
            ELSIF Text.Equal(keyWord, "COMPOSITE") THEN
              Debug.Out("Reading COMPOSITE: " & line, 100);
              VAR
                 aliasList := textReader.shatter(" ,", ":", skipNulls := TRUE);
                 constitList:=
                     LookupBaseLayers(res, textReader.shatter(" ,", ":", skipNulls := TRUE));
                 compositeLayer := 
                    NEW(PaintLayer.T, 
                        baseLayers := constitList).init(aliasList.head);
              BEGIN
                AddAliases(compositeLayer, aliasList)
              END
            END
          END
        END;
        INC(lineno)
      END
    EXCEPT
    | Rd.EndOfFile => (* ok *)
    | TextReader.NoMore, Tint.ParseError => 
      Debug.Error("Syntax error on line " & Fmt.Int(lineno) & " of conf file.")
    END;
    RETURN res
  END ReadConf;

BEGIN END Configuration.
