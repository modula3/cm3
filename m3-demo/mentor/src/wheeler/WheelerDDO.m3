(* Copyright (C) 1993, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Jan  9 12:20:34 PST 1995 by najork                   *)
(*      modified on Wed Oct 13 15:44:08 PDT 1993 by mann                     *)
(*      modified on Thu Jul 22 14:26:58 PDT 1993 by perl                     *)

MODULE WheelerDDO EXPORTS WheelerDDO;

IMPORT Char, CharArraySort, IO, RefList, Text, TextArraySort, TextF, VBT;
IMPORT Thread, FormsVBT;

(* Zeus stuff *)
IMPORT Algorithm, WheelerAlgClass, WheelerIE, ZeusPanel, ZeusCodeView;

<* FATAL IO.Error, FormsVBT.Error, FormsVBT.Unimplemented *>

TYPE T = WheelerAlgClass.T BRANDED OBJECT
  OVERRIDES
    run := Run
  END;

PROCEDURE New(): Algorithm.T =
  BEGIN
    RETURN
      NEW(T,
          data := ZeusPanel.NewForm("WheelerInput.fv")
          ).init()
  END New;


PROCEDURE Run(alg: T) RAISES {Thread.Alerted} =
  VAR codes: REF ARRAY OF INTEGER;
      pos: CARDINAL;
      alphabet, string: TEXT;
  BEGIN
    LOCK VBT.mu DO
      alphabet := FormsVBT.GetText(alg.data, "alphabet");
      string := FormsVBT.GetText(alg.data, "string");
    END;
    codes := Ws(alg, alphabet, string, pos);
    EVAL UnWs(alg, alphabet, codes, pos)
  END Run;

PROCEDURE Ws(alg: T; alpha, string: TEXT; VAR pos: CARDINAL
  ): REF ARRAY OF INTEGER
    RAISES {Thread.Alerted} =
(* Apply the Wheeler transformation to the input "string", with
   alphabet "alpha", returning the sequence of integers that are
   codes for the characters of "string" and "pos" which is the
   position of "string" in the sorted rotations. Using these
   return values and the alphabet as inputs, "UnWs" can reconstruct "string". 
*)

  PROCEDURE Rotate(s: TEXT; i: CARDINAL): TEXT =
    (* Return a new string that is "s" rotated "i" positions
       to the left (cyclically). *)
    VAR sn := NUMBER(s^)-1;
        res := NEW(TEXT, sn+1);
    BEGIN
      FOR j := 0 TO sn-1 DO
        res[j] := string[(i+j) MOD sn]
      END;
      res[sn] := '\000';
      RETURN res
    END Rotate;

  VAR n := NUMBER(string^)-1;
      rotations := NEW(REF ARRAY OF TEXT, n);
      lastchars := NEW(TEXT, n+1);
      xchars: TEXT;
  BEGIN
(*  WheelerIE.StartPermute(alg, string, alpha);*)

    (* generate an "n * n" array containing the "n" rotations of "string". *)
    FOR i := 0 TO n-1 DO
      rotations[i] := Rotate(string, i);
(*    WheelerIE.NextRotation(alg, i, rotations[i]);*)
    END;

    (* sort the rotations *)
    TextArraySort.Sort(rotations^, Text.Compare);

    (* find the index of the original string in the list of sorted rotations *)
    pos := 0;
    WHILE (NOT Text.Equal(string, rotations[pos])) DO INC(pos) END;

    WheelerIE.RotationsSorted(alg, rotations, pos);

    (* pick off the last character of each rotation *)
    FOR i := 0 TO n-1 DO
      lastchars[i] := rotations[i][n-1]
    END;
    lastchars[n] := '\000';
(*  WheelerIE.PermuteDone(alg, lastchars, pos);*)

    (* append list of last characters to the alphabet *)
    VAR alen := Text.Length(alpha); BEGIN
      xchars := NEW(TEXT, alen + n + 1);
      SUBARRAY(xchars^, 0, alen) := SUBARRAY(alpha^, 0, alen);
      SUBARRAY(xchars^, alen, n+1) := lastchars^
    END;
(*  WheelerIE.StartEncode(alg, alpha);*)

    (* for each character in "lastchars", find the number of distinct
       characters between it and the preceding instance of the same character
       in the string.  If the character does not occur previously in the
       string, we continue the search as though the alphabet had been prepended
       to the original string. *)
    VAR output := NEW(REF ARRAY OF INTEGER, n); BEGIN
      FOR i := 0 TO n-1 DO
        VAR c := lastchars[i];
            seen := NEW(REF ARRAY OF BOOLEAN, 256);
            count := 0;
            j := Text.Length(alpha) + i - 1;
        BEGIN
(*        WheelerIE.EncodeNextChar(alg, i, c);*)
          WHILE(xchars[j] # c) DO
            IF NOT seen[ORD(xchars[j])] THEN
              seen[ORD(xchars[j])] := TRUE;
              INC(count);
(*            WheelerIE.EncodeDistinctCount(alg, i, j, count, c);*)
            END;
            DEC(j)
          END;
          output[i] := count;
(*        WheelerIE.EncodeFoundCode(alg, i, j, count, c);*)
        END
      END;
      (* Return the position and the output array. *)
(*    WheelerIE.EncodeDone(alg, alpha, output, pos);*)
      RETURN output
    END 
  END Ws;

PROCEDURE UnWs(alg: T; alpha: TEXT; codes: REF ARRAY OF INTEGER; pos: CARDINAL
  ): TEXT
    RAISES {Thread.Alerted} =
(* Undo a Wheeler transformation. "codes" is the sequence of
   integer codes and "pos" is the row position produced by "Ws".
   "alpha" is the alphabet given to "Ws". Returns the original string.
*)
  PROCEDURE At (line: INTEGER) RAISES {Thread.Alerted} =
    BEGIN ZeusCodeView.At(alg, line) END At;
  VAR n := NUMBER(codes^);
      alen := NUMBER(alpha^) - 1;
      xchars := NEW(TEXT, alen + n + 1);
      lastchars := NEW(TEXT, n + 1);
      firstchars := NEW(TEXT, n + 1);
      charmap := NEW(REF ARRAY OF INTEGER, n);
  BEGIN
    ZeusCodeView.Enter(alg, procedureName := "Decompress");

    (* rederive the "lastchars" string using "codes", the alphabet,
       and the row position*)
(*At(2);*)
(*  WheelerIE.InitDecode(alg, alpha, codes, pos);*)
(*  WheelerIE.StartDecode(alg);*)
    SUBARRAY(xchars^, 0, alen+1) := alpha^;
    FOR i := 0 TO n-1 DO
      VAR count := 0;
          seen := NEW(REF ARRAY OF BOOLEAN, 256);
          j := alen + i;
      BEGIN
(*      WheelerIE.DecodeNextCode(alg, i);*)
        WHILE (count < codes[i]+1) DO
          DEC(j);
          IF NOT seen[ORD(xchars[j])] THEN
            seen[ORD(xchars[j])] := TRUE;
            INC(count);
            IF count < codes[i] + 1 THEN
(*            WheelerIE.DecodeDistinctCount(alg, i, j, count)*)
            END
          END
        END;
(*      WheelerIE.DecodeFoundChar(alg, i, j, xchars[j]);*)
        xchars[alen+i] := xchars[j];
        lastchars[i] := xchars[j]
      END;
      lastchars[n] := '\000'
    END;

    WheelerIE.DecodeDone(alg, lastchars, pos);
(*At(3);*)
    WheelerIE.StartReconstruct(alg, lastchars, pos);
    WheelerIE.Reveal1(alg);

    (* obtain the array of initial characters in the sorted rotations
       by sorting the "lastchars" array *)
    firstchars^ := lastchars^;
    CharArraySort.Sort(SUBARRAY(firstchars^, 0, n), Char.Compare); 

(*At(4);*)
    WheelerIE.FirstChars(alg, firstchars);
    WheelerIE.Reveal2(alg);

    (* set "charmap[i]" to contain the index in "lastchars" of the character
       corresponding to "firstchar[i]" *)
(*At(5);*)
    VAR j := 0; BEGIN
      FOR i := 0 TO n-1 DO
        IF i # 0 AND firstchars[i] # firstchars[i-1] THEN
          j := 0;
          WheelerIE.FinishCharRun(alg)
        END;
        WheelerIE.ConsiderChar(alg, i);
        WHILE firstchars[i] # lastchars[j] DO
          INC(j);
        END;
        WheelerIE.EqualChars(alg, i, j);
        charmap[i] := j;
        INC(j)
      END;
      WheelerIE.FinishCharRun(alg);
    END;

    WheelerIE.StartResult(alg);

    (* construct the original string by reading through "firstchars" and
       "lastchars" using "charmap" *)
(*At(6);*)
    VAR ans := NEW(TEXT, n+1); BEGIN
      FOR i := 0 TO n-1 DO
        WheelerIE.ResultNextChar(alg, pos, i);
        ans[i] := firstchars[pos];
        pos := charmap[pos]
      END;
      ans[n] := '\000';
      WheelerIE.EndResult(alg);
      RETURN ans
    END;
  END UnWs;


BEGIN
  ZeusPanel.RegisterAlg(New, "Decompress Details Only", "Wheeler")
END WheelerDDO.
