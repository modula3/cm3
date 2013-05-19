(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

UNSAFE MODULE TextCat EXPORTS RTHooks, TextCat;

IMPORT TextClass;
IMPORT Text8, Text8Short, Text16, Text16Short; 

IMPORT TextStats;
FROM TextStats IMPORT Op; 

REVEAL
  T = Public BRANDED "TextCat.T" OBJECT OVERRIDES
    get_info       := CatGetInfo;
    get_char       := CatGetChar;
    get_wide_char  := CatGetWideChar;
    get_chars      := CatGetChars;
    get_wide_chars := CatGetWideChars;
  END;
                                   
PROCEDURE New (t, u: TEXT): TEXT =
  BEGIN
    RETURN Concat (t, u);
  END New;

(* RTHooks.Concat -- called by the inline "&" operator *)
PROCEDURE Concat (t, u: TEXT): TEXT =
  VAR LInfo, RInfo: TextClass.Info;
  VAR Result: TEXT := NIL;
  BEGIN
    TextStats.NoteGround (Op.Cat); 
    IF TextClass.Old 
    THEN Result := OldConcat (t, u); 
    ELSE (* The nonrecursive part of the new Concat: *)  
      t.get_info (LInfo);  
      u.get_info (RInfo);  
      IF (LInfo.length <= 0) THEN Result := u 
      ELSIF (RInfo.length <= 0) THEN Result := t
      ELSE 
        Balance (t, LInfo, u, RInfo, ADR (Result), RebalanceFlat := FALSE);
      END (* IF *) 
    END (* IF *);
    TextStats.NoteFinished (Op.Cat); 
    RETURN Result 
  END Concat;

PROCEDURE OldConcat (t, u: TEXT): TEXT =
  VAR ti, ui: TextClass.Info;
  VAR Result: TEXT; 
  BEGIN
    t.get_info (ti);  IF (ti.length <= 0) THEN RETURN u; END;
    u.get_info (ui);  IF (ui.length <= 0) THEN RETURN t; END;
    Result := NEW (T, a := t, b := u,
                   a_len := ti.length, b_len := ui.length,
                   a_or_b_wide := ti.wide OR ui.wide);
    TextStats.NoteAllocTextCat(Result); 
    RETURN Result 
  END OldConcat;

PROCEDURE Balance 
     ( LText: TEXT; VAR LInfo : TextClass.Info; 
       RText: TEXT; VAR RInfo : TextClass.Info;
       (* LInfo and RInfo are not output parameters, but 
          1. we want to pass them by reference since they are records, and 
          2. they need to be changeable inside this procedure, to eliminate 
             tail recursion. *) 
       ResultRef : UNTRACED REF TEXT; 
       (* ^Done this necessarily unsafe way to eliminate tail recursion. *) 
       CatNode : T := NIL;
       (* ^If non-NIL, a node that is TextCat.T(LRext,RText), which Balance
          can return, rather than allocating a new one just like it, if it 
          decides this is its best-balanced result.
        *) 
       RebalanceFlat : BOOLEAN := TRUE 
     ) = 
(* Return the concatenation (LText/LInfo & RText/RInfo), with flattening and
   rebalancing done.  
   Try to combine short strings into flat nodes (Text8Short.T, Text8.T, 
   Text16Short.T, Text16.T).  Also, if not RebalanceFlat, keep a flat node 
   that is leftmost or rightmost at the top level, in case something 
   else is concatenated to it later than can be combined with it.  
   When the caller/previous iteration knows there is no chance of this, 
   it communicates so with RebalanceFlat=TRUE.  In this case, do all
   rebalancing. 
   It is not possible to both reuse CatNode and have recursion to balance 
   subtrees be (convertable to iteration) tail-recursion.  Prefer to 
   eliminate the recursion, which could be high-depth, and risk an unnecessary
   but fixed-time allocation. *) 
(* PRE: LText.length # 0 AND RText.length # 0. *) 
(* INVARIANT: (both recursive and loop) ResultRef^ is a location that
   the GC will trace. *) 

  VAR UnusedInfo: TextClass.Info;
  VAR Cat: T := NIL;
  VAR RDepth: CARDINAL; 
  VAR LLInfo, LRInfo, RLInfo, RRInfo: TextClass.Info; 
  VAR LLText, LRText, RLText, RRText: TEXT; 
  (* "[L|R]" in identifiers refer to left/right operand of Balance.
     When the operand is a TextCat.T, "[L|R][L|R]" refer to the left/right 
     operand's left/right subcomponent, first letter is the operand, second
     is the subcomponent.  This is the limit of depth we look at. 
  *) 
  (* To make sense of the names of these procedures, imagine a Polish prefix
     notation with Cat and Bal as binary operators.
  *) 

  PROCEDURE Cat_L_R ( ) = (* Never iterates, caller should always RETURN. *) 
    BEGIN 
      (* Cat(L,R) *)  
      IF CatNode = NIL 
      THEN (* Build a new Cat node. *) 
        Cat := NEW (T, a := LText, b := RText);  
        Cat.a_len := LInfo.length; 
        Cat.b_len := RInfo.length;
        Cat.a_or_b_wide := LInfo.wide OR RInfo.wide; 
        ResultRef ^ := Cat;
        TextStats.NoteAllocTextCat (Cat); 
      ELSE
        ResultRef ^ := CatNode; 
      END; 
    END Cat_L_R; 

  PROCEDURE Cat_Bal_LL_LR_R (LCat: T)
  : BOOLEAN (* FALSE -> Loop; TRUE -> RETURN. *) = 
    (* Cat(Bal(LL,LR),R) *) 
    VAR BalResult: TEXT; 
    BEGIN 
      IF CatNode = NIL 
      THEN 
        Cat := NEW (T, a := NIL, b := RText);  
        Cat.a_len := LInfo.length;
        Cat.b_len := RInfo.length;
        Cat.a_or_b_wide := LInfo.wide OR RInfo.wide; 
        ResultRef ^ := Cat;
        TextStats.NoteAllocTextCat (Cat); 
        (* Tail recursion elimination of: 
          Balance 
            (LLText, LLInfo, LRText, LRInfo, ADR(Cat.a), CatNode := LCat);
          RETURN 
        *)
        TextStats.NoteIter (Op.Cat); 
        LText := LLText; LInfo := LLInfo;
        RText := LRText; RInfo := LRInfo;
        ResultRef := ADR (Cat.a);
        CatNode := LCat; 
        RebalanceFlat := TRUE;
        RETURN FALSE (* Cause Balance to loop. *) 
      ELSE (* Avoid risk of creating a copy of CatNode, at the cost of 
              doing the Balance recursively. 
           *) 
        TextStats.NoteRecurse (Op.Cat); 
        Balance 
          (LLText, LLInfo, LRText, LRInfo, ADR(BalResult), CatNode := LCat);
        IF BalResult = LCat 
        THEN (* No rebalancing done.  We can reuse CatNode. *) 
          ResultRef ^ := CatNode; 
        ELSE
          Cat := NEW (T, a := BalResult, b := RText);  
          Cat.a_len := LInfo.length;
          Cat.b_len := RInfo.length;
          Cat.a_or_b_wide := LInfo.wide OR RInfo.wide; 
          ResultRef ^ := Cat;
          TextStats.NoteAllocTextCat (Cat); 
        END; 
        RETURN TRUE (* Cause Balance to return. *) 
      END; 
    END Cat_Bal_LL_LR_R; 

  PROCEDURE Cat_L_Bal_RL_RR (RCat: T)
  : BOOLEAN (* FALSE -> Loop; TRUE -> RETURN. *) = 
    (* Cat(L,Bal(RL,RR)) *)
    VAR BalResult: TEXT; 
    BEGIN 
      IF CatNode = NIL 
      THEN 
        Cat := NEW (T, a := LText, b := NIL);  
        Cat.a_len := LInfo.length;
        Cat.b_len := RInfo.length;
        Cat.a_or_b_wide := LInfo.wide OR RInfo.wide; 
        ResultRef ^ := Cat;
        TextStats.NoteAllocTextCat (Cat); 
        (* Tail recursion elimination of: 
          Balance 
            (RLText, RLInfo, RRText, RRInfo, ADR(Cat.b), CatNode := RCat);
          RETURN 
        *)
        TextStats.NoteIter (Op.Cat); 
        LText := RLText; LInfo := RLInfo;
        RText := RRText; RInfo := RRInfo;
        ResultRef := ADR (Cat.b);
        CatNode := RCat; 
        RebalanceFlat := TRUE;
        RETURN FALSE (* Cause Balance to loop. *) 
      ELSE (* Avoid risk of creating a copy of CatNode, at the cost of 
              doing the Balance recursively. 
           *) 
        TextStats.NoteRecurse (Op.Cat); 
        Balance 
          (RLText, RLInfo, RRText, RRInfo, ADR(BalResult), CatNode := RCat);
        IF BalResult = RCat 
        THEN (* No rebalancing done.  We can reuse CatNode. *) 
          ResultRef ^ := CatNode; 
        ELSE
          Cat := NEW (T, a := LText, b := BalResult);  
          Cat.a_len := LInfo.length;
          Cat.b_len := RInfo.length;
          Cat.a_or_b_wide := LInfo.wide OR RInfo.wide; 
          ResultRef ^ := Cat;
          TextStats.NoteAllocTextCat (Cat); 
        END; 
        RETURN TRUE (* Cause Balance to return. *) 
      END; 
    END Cat_L_Bal_RL_RR;

  PROCEDURE Cat_LL_Bal_LR_R (): BOOLEAN (* FALSE -> Loop; TRUE -> RETURN. *) = 
    BEGIN 
      (* Cat(LL,Bal(LR,R)) *) 
      Cat := NEW (T, a := LLText, b := NIL);  
      Cat.a_len := LLInfo.length;
      Cat.b_len := LRInfo.length + RInfo.length;
      Cat.a_or_b_wide := LInfo.wide OR RInfo.wide; 
      ResultRef ^ := Cat;
      TextStats.NoteAllocTextCat (Cat); 
      (* Tail recursion elimination of: 
        Balance (LRText, LRInfo, RText, RInfo, ADR(Cat.b));
        RETURN 
      *)
      TextStats.NoteIter (Op.Cat); 
      LText := LRText; LInfo := LRInfo;
      (* RText and RInfo are unchanged. *) 
      ResultRef := ADR (Cat.b);
      CatNode := NIL; 
      RebalanceFlat := TRUE;
      RETURN FALSE (* Cause Balance to loop. *) 
    END Cat_LL_Bal_LR_R; 

  PROCEDURE Cat_Bal_L_RL_RR (): BOOLEAN (* FALSE -> Loop; TRUE -> RETURN. *) = 
    BEGIN 
      (* Cat(Bal(L,RL),RR) *) 
      Cat := NEW (T, a := NIL, b := RRText);  
      Cat.a_len := LInfo.length + RLInfo.length;
      Cat.b_len := RRInfo.length;
      Cat.a_or_b_wide := LInfo.wide OR RInfo.wide; 
      ResultRef ^ := Cat;
      TextStats.NoteAllocTextCat (Cat); 
      (* Tail recursion elimination of: 
        Balance (LText, LInfo, RLText, RLInfo, ADR(Cat.a));
        RETURN 
      *)
      TextStats.NoteIter (Op.Cat); 
      (* LText and LInfo are unchanged. *) 
      RText := RLText; RInfo := RLInfo;
      ResultRef := ADR (Cat.a);
      CatNode := NIL; 
      RebalanceFlat := TRUE;
      RETURN FALSE (* Cause Balance to loop. *) 
    END Cat_Bal_L_RL_RR; 

  BEGIN 
    LOOP (* Used for tail recursion elimination. *) 
      IF NOT TextClass.Flatten THEN RebalanceFlat := TRUE END;
      TYPECASE LText 
      OF T (LCat)
      => TYPECASE RText 

        OF T (RCat)
        => (* LText and RText are both TextCat.Ts. *) 
          VAR LRCatRLInfo: TextClass.Info; 
          VAR LRCatRLText: TEXT; 
          VAR LRCatRLLength: CARDINAL;
          BEGIN
            LLText := LCat.a; 
            LRText := LCat.b; 
            RLText := RCat.a; 
            RRText := RCat.b;
            LLText.get_info(LLInfo);
            LRText.get_info(LRInfo);
            RLText.get_info(RLInfo);
            RRText.get_info(RRInfo);
            LRCatRLText 
              := Flat (LRText, LRInfo, RLText, RLInfo, (*VAR*)LRCatRLInfo);
            IF LRCatRLText # NIL 
            THEN (* Were able to combine and flatten LRText and RLText 
                    into a single node. *)  
              IF LLInfo.length < RRInfo.length
              THEN (* Cat(Bal(LL,Flat(LR,RL)),RR) *) 
                Cat := NEW (T, a := NIL, b := RRText);
                Cat.a_len := LLInfo.length + LRInfo.length + RLInfo.length;
                Cat.b_len := RRInfo.length;
                Cat.a_or_b_wide := LInfo.wide OR RInfo.wide; 
                ResultRef ^ := Cat;
                TextStats.NoteAllocTextCat (Cat); 
                (* Tail recursion elimination of: 
                  Balance 
                    (LLText, LLInfo, LRCatRLText, LRCatRLInfo, ADR(Cat.a));
                  RETURN
                *)
                TextStats.NoteIter (Op.Cat); 
                LText := LLText; LInfo := LLInfo;
                RText := LRCatRLText; RInfo := LRCatRLInfo;
                ResultRef := ADR(Cat.a);
                CatNode := NIL; 
                RebalanceFlat := TRUE
                (* And loop. *) 
              ELSE (* Cat(LL,Bal(Flat(LR,RL),RR)) *) 
                Cat := NEW (T, a := LLText, b := NIL);  
                Cat.a_len := LLInfo.length;
                Cat.b_len := LRInfo.length + RLInfo.length + RRInfo.length;
                Cat.a_or_b_wide := LInfo.wide OR RInfo.wide; 
                ResultRef ^ := Cat;
                TextStats.NoteAllocTextCat (Cat); 
                (* Tail recursion elimination of: 
                  Balance 
                    (LRCatRLText, LRCatRLInfo, RRText, RRInfo, ADR(Cat.b));
                  RETURN 
                *)
                TextStats.NoteIter (Op.Cat); 
                LText := LRCatRLText; LInfo := LRCatRLInfo;
                RText := RRText; RInfo := RRInfo;
                ResultRef := ADR(Cat.b);
                CatNode := NIL; 
                RebalanceFlat := TRUE
                (* And loop. *) 
              END (* IF *)
            ELSE (* Didn't combine LRText with RLText. *)  
              IF NOT ISTYPE (LRText, T)  
              THEN (* LRText is flat. *) 
                IF NOT ISTYPE (RLText, T)  
                THEN (* Both LRText and RLText are already flat, but could
                        not be combined.  Cat(Bal(LL,LR),Bal(RL,RR)) 
                     *)
                  Cat := NEW (T, a := NIL, b := NIL);  
                  Cat.a_len := LInfo.length;
                  Cat.b_len := RInfo.length;
                  Cat.a_or_b_wide := LInfo.wide OR RInfo.wide; 
                  ResultRef ^ := Cat;
                  TextStats.NoteAllocTextCat (Cat); 
                  TextStats.SaveRecurseDepth (Op.Cat, RDepth);
                  TextStats.NoteRecurse (Op.Cat); 
                  IF LInfo.length < RInfo.length
                  THEN (* Recurse on as-short left side, iterate on right. *) 
                    Balance 
                      (LLText, LLInfo, LRText, LRInfo, ADR (Cat.a), 
                       CatNode := LCat);
                    TextStats.RestoreRecurseDepth (Op.Cat, RDepth);
                    (* Tail recursion elimination of: 
                      Balance 
                        (RLText, RLInfo, RRText, RRInfo, ADR (Cat.b), 
                         CatNode := RCat);
                      RETURN 
                    *)
                    TextStats.NoteIter (Op.Cat); 
                    LText := RLText; LInfo := RLInfo;
                    RText := RRText; RInfo := RRInfo;
                    ResultRef := ADR(Cat.b);
                    CatNode := RCat; 
                    RebalanceFlat := TRUE
                    (* And loop. *) 
                  ELSE (* Recurse on shorter right side, iterate on left. *) 
                    Balance 
                      (RLText, RLInfo, RRText, RRInfo, ADR (Cat.b), 
                       CatNode := RCat);
                    TextStats.RestoreRecurseDepth (Op.Cat, RDepth);
                    (* Tail recursion elimination of: 
                      Balance 
                        (LLText, LLInfo, LRText, LRInfo, ADR (Cat.a), 
                         CatNode := LCat);
                      RETURN 
                    *)
                    TextStats.NoteIter (Op.Cat); 
                    LText := LLText; LInfo := LLInfo;
                    RText := LRText; RInfo := LRInfo;
                    ResultRef := ADR(Cat.a);
                    CatNode := LCat; 
                    RebalanceFlat := TRUE
                    (* And loop. *) 
                  END; 
                ELSE (* Only LRText is flat.  Cat(Bal(LL,LR),R) *)
                  IF Cat_Bal_LL_LR_R (LCat) THEN RETURN END; 
                  (* Else loop. *) 
                END (* IF *) 
              ELSE (* LRText is a TextCat.T *) 
                IF NOT ISTYPE (RLText, T)  
                THEN (* Only RLText is flat.  Cat(L,Bal(RL,RR)). *)
                  IF Cat_L_Bal_RL_RR (RCat) THEN RETURN END; 
                  (* Else loop. *) 
                ELSE (* Neither LRText nor RLText is flat. *) 
                  LRCatRLLength := LRInfo.length + RLInfo.length;
                  IF LLInfo.length > LRCatRLLength + RRInfo.length 
                  THEN (* Cat(LL,Bal(LR,R)) *) 
                    IF Cat_LL_Bal_LR_R () THEN RETURN END;                     
                    (* Else loop. *) 
                  ELSIF LLInfo.length + LRCatRLLength < RRInfo.length  
                  THEN (* Cat(Bal(L,RL),RR) *) 
                    IF Cat_Bal_L_RL_RR () THEN RETURN END; 
                    (* Else loop. *) 
                  ELSE (* Cat(L,R) *)  
                    Cat_L_R (); 
                    RETURN;  
                  END (* IF *) 
                END (* IF *) 
              END (* IF *) 
            END (* IF *)   
          END (* Block *)  

        ELSE (* LText is a TextCat.T; RText is not. *) 
          VAR LRCatRInfo: TextClass.Info; 
          VAR LRCatRText : TEXT; 
          BEGIN
            LLText := LCat.a;
            LRText := LCat.b;
            LLText.get_info(LLInfo);
            LRText.get_info(LRInfo);
            LRCatRText 
              := Flat (LRText, LRInfo, RText, RInfo, (*VAR*)LRCatRInfo); 
            IF LRCatRText # NIL 
            THEN (* Flattening worked.  Cat(LL,Flat(LR,R)) *) 
              Cat := NEW (T, a := LLText, b := LRCatRText);  
              Cat.a_len := LLInfo.length; 
              Cat.b_len := LRCatRInfo.length;
              Cat.a_or_b_wide := LLInfo.wide OR LRCatRInfo.wide; 
              ResultRef ^ := Cat;
              TextStats.NoteAllocTextCat (Cat); 
              RETURN 
            ELSE (* Can't flatten LRText and RText. *) 
              IF NOT RebalanceFlat 
                 (* ^Avoid rebalancing R to keep it at top right for possible
                    future flatting, *)  
                 OR LRInfo.length = LLInfo.length 
              THEN (* Cat(L,R) *) 
                Cat_L_R (); 
                RETURN;
              ELSIF LRInfo.length > LLInfo.length  
              THEN (* Cat(Bal(LL,LR),R) *) 
                IF Cat_Bal_LL_LR_R (LCat) THEN RETURN END; 
                (* Else loop. *) 
              ELSE (* Cat(LL,Bal(LR,R)) *) 
                IF Cat_LL_Bal_LR_R () THEN RETURN END;                     
                (* Else loop. *) 
              END (* IF *)  
            END (* IF *)  
          END (* Block *)  
        END (* TYPECASE RText *) 

      ELSE (* LText is not a TextCat.T. *) 
        TYPECASE RText 

        OF T (RCat)
        => (* LText is not a TextCat.T; RText is. *) 
          VAR LCatRLInfo: TextClass.Info; 
          VAR LCatRLText: TEXT; 
          BEGIN
            RLText := RCat.a;
            RRText := RCat.b;
            RLText.get_info(RLInfo);
            RRText.get_info(RRInfo);
            LCatRLText 
              := Flat (LText, LInfo, RLText, RLInfo, (*VAR*)LCatRLInfo); 
            IF LCatRLText # NIL 
            THEN (* Flattening worked. Cat(Flat(L,RL),RR) *) 
              Cat := NEW (T, a := LCatRLText, b := RRText);  
              Cat.a_len := LCatRLInfo.length; 
              Cat.b_len := RRInfo.length;
              Cat.a_or_b_wide := LCatRLInfo.wide OR RRInfo.wide; 
              ResultRef ^ := Cat;
              TextStats.NoteAllocTextCat (Cat); 
              RETURN 
            ELSE (* Can't flatten LText and RLText. *) 
              IF NOT RebalanceFlat
                 (* ^Avoid rebalancing L to keep it at top left for possible
                    future flattening. *)
                 OR RLInfo.length = RRInfo.length 
              THEN (* Cat(L,R) *) 
                Cat_L_R (); 
                RETURN;
              ELSIF RLInfo.length > RRInfo.length  
              THEN (* Cat(L,Bal(RL,RR)) *) 
                IF Cat_L_Bal_RL_RR (RCat) THEN RETURN END; 
                (* Else loop. *) 
              ELSE (* Cat(Bal(L,RL),RR) *) 
                IF Cat_Bal_L_RL_RR () THEN RETURN END; 
                (* Else loop. *) 
              END (* IF *)  
            END (* IF *)  
          END (* Block *)  

        ELSE (* Neither LText nor RText is a TextCat.T. *) 
          VAR LCatRText: TEXT 
            := Flat (LText, LInfo, RText, RInfo, (*VAR*)UnusedInfo); 
          BEGIN 
            IF LCatRText # NIL 
            THEN (* Combining into one flat node worked. Flat(L,R) *) 
              ResultRef ^ := LCatRText; 
              RETURN 
            ELSE (* Two flat nodes, can't combine, but they stay on the
                    left and right, as candidates for future combination. 
                    Cat(L,R) 
                 *) 
              Cat_L_R (); 
              RETURN; 
            END (* IF *)  
          END (* Block *)  
        END (* TYPECASE RText *) 
      END (* TYPECASE LText *) 
    END (* LOOP *) 
  END Balance;

PROCEDURE Flat  
   (LText: TEXT; READONLY LInfo: TextClass.Info; 
    RText: TEXT; READONLY RInfo: TextClass.Info; 
    VAR ResultInfo: TextClass.Info)
 : TEXT =
(* Maybe flatten left and right into a single heap object. 
   If did not flatten, return NIL, with possible changes made to 
   ResultInfo, but they are undefined and irrelevant to the caller.
*) 
  VAR ResultText8: Text8.T;
  VAR ResultText8Short: Text8Short.T;
  VAR ResultText16: Text16.T;
  VAR ResultText16Short: Text16Short.T;
  BEGIN
    IF NOT TextClass.Flatten THEN RETURN NIL END; 
    ResultInfo.length := LInfo.length + RInfo.length;
    ResultInfo.wide := LInfo.wide OR RInfo.wide; 
    ResultInfo.start := NIL;
    IF ResultInfo.wide 
    THEN 
      IF ResultInfo.length < Text16Short.MaxLength 
      THEN (* Flatten into a Text16Short.T. *) 
        ResultText16Short := NEW (Text16Short.T, len := ResultInfo.length);
        TextStats.NoteGround (Op.get_wide_chars); 
        LText.get_wide_chars 
          (SUBARRAY (ResultText16Short.contents, 0, LInfo.length), 0); 
        TextStats.NoteFinished (Op.get_wide_chars); 
        TextStats.NoteGround (Op.get_wide_chars); 
        RText.get_wide_chars 
          (SUBARRAY(ResultText16Short.contents, LInfo.length, RInfo.length), 0);
        TextStats.NoteFinished (Op.get_wide_chars); 
        ResultText16Short.contents[ResultInfo.length] := VAL(0, WIDECHAR);
        ResultInfo.start := ADR (ResultText16Short.contents[0]);
        TextStats.NoteAllocText16Short(ResultText16Short); 
        RETURN ResultText16Short
      ELSIF ResultInfo.length < TextClass.MaxFlatWide 
      THEN (* Flatten into a Text16.T. *) 
        ResultText16 := Text16.Create(ResultInfo.length);
                        (* ^Which sets the terminating null character. *) 
        TextStats.NoteGround (Op.get_wide_chars); 
        LText.get_wide_chars 
          (SUBARRAY (ResultText16.contents^, 0, LInfo.length), 0); 
        TextStats.NoteFinished (Op.get_wide_chars); 
        TextStats.NoteGround (Op.get_wide_chars); 
        RText.get_wide_chars 
          (SUBARRAY (ResultText16.contents^, LInfo.length, RInfo.length), 0); 
        TextStats.NoteFinished (Op.get_wide_chars); 
        ResultInfo.start := ADR (ResultText16.contents^[0]);
        RETURN ResultText16
      ELSE RETURN NIL (* Don't flatten. *) 
      END (* IF *) 
    ELSE (* No wide chars involved. *) 
      IF ResultInfo.length < Text8Short.MaxLength 
      THEN (* Flatten into a Text8Short.T. *) 
        ResultText8Short := NEW (Text8Short.T, len := ResultInfo.length);
        TextStats.NoteGround (Op.get_chars); 
        LText.get_chars 
          (SUBARRAY (ResultText8Short.contents, 0, LInfo.length), 0); 
        TextStats.NoteFinished (Op.get_chars); 
        TextStats.NoteGround (Op.get_chars); 
        RText.get_chars 
          (SUBARRAY(ResultText8Short.contents, LInfo.length, RInfo.length), 0);
        TextStats.NoteFinished (Op.get_chars); 
        ResultText8Short.contents[ResultInfo.length] := '\000';
        ResultInfo.start := ADR (ResultText8Short.contents[0]);
        TextStats.NoteAllocText8Short(ResultText8Short); 
        RETURN ResultText8Short
      ELSIF ResultInfo.length < TextClass.MaxFlat8 
      THEN (* Flatten into a Text8.T. *) 
        ResultText8 := Text8.Create(ResultInfo.length);
                       (* ^Which sets the terminating null character. *) 
        TextStats.NoteGround (Op.get_chars); 
        LText.get_chars (SUBARRAY (ResultText8.contents^, 0, LInfo.length), 0); 
        TextStats.NoteFinished (Op.get_chars); 
        TextStats.NoteGround (Op.get_chars); 
        RText.get_chars 
          (SUBARRAY (ResultText8.contents^, LInfo.length, RInfo.length), 0); 
        TextStats.NoteFinished (Op.get_chars); 
        ResultInfo.start := ADR (ResultText8.contents^[0]);
        RETURN ResultText8
      ELSE RETURN NIL (* Don't flatten. *) 
      END (* IF *) 
    END (* IF *) 
  END Flat;

PROCEDURE NewMulti (READONLY x: ARRAY OF TEXT): TEXT =
  BEGIN
    RETURN MultiCat (x);
  END NewMulti;

(* RTHooks.MultiCat *)
PROCEDURE MultiCat (READONLY x: ARRAY OF TEXT): TEXT =
  VAR result: TEXT;
  BEGIN
    TextStats.NoteGround (Op.MultiCat); 
    IF TextClass.Old THEN result := OldMultiCat (x)
    ELSE result := NewMultiCat(x)
    END; 
    TextStats.NoteFinished (Op.MultiCat); 
    RETURN result;
  END MultiCat;

PROCEDURE OldMultiCat (READONLY x: ARRAY OF TEXT): TEXT =
  VAR result: TEXT;
  VAR r_info, xi_info: TextClass.Info; 
  BEGIN
    IF NUMBER (x) <= 0 THEN RETURN "";   END;
    IF NUMBER (x) = 1  THEN RETURN x[0]; END;

    result := x[LAST(x)];
    result.get_info (r_info); 
    FOR i := LAST(x) - 1 TO 0 BY -1 DO
      WITH xi = x[i] DO
        xi.get_info(xi_info);
        r_info.wide := r_info.wide OR xi_info.wide;
        result := NEW (T, a := xi, a_len := xi_info.length, 
                          b := result, b_len := r_info.length,
                          a_or_b_wide := r_info.wide);
        INC(r_info.length, xi_info.length);
        TextStats.NoteAllocTextCat(result); 
        TextStats.NoteIter (Op.MultiCat); 
      END; 
    END;
    RETURN result;
  END OldMultiCat;

PROCEDURE NewMultiCat (READONLY x: ARRAY OF TEXT): TEXT =
  VAR result: TEXT;
  BEGIN
    IF NUMBER (x) <= 0 THEN RETURN "";   END;
    IF NUMBER (x) = 1  
    THEN 
      IF x[0] = NIL THEN RETURN ""
      ELSE RETURN x[0]
      END 
    END;

    result := x[LAST(x)];
    FOR i := LAST(x) - 1 TO 0 BY -1 DO
      result := Concat (x[i], result);
      TextStats.NoteIter (Op.MultiCat); 
    END;
    RETURN result;
  END NewMultiCat;

PROCEDURE CatGetInfo (t: T;  VAR info: TextClass.Info) =
  BEGIN
    info.start  := NIL;
    info.length := t.a_len + t.b_len;
    info.wide   := t.a_or_b_wide;
  END CatGetInfo;

PROCEDURE CatGetChar (t: T; index: CARDINAL): CHAR = 
  BEGIN
    IF TextClass.Old 
    THEN RETURN CatOldGetChar (t, index); 
    ELSE RETURN CatNewGetChar (t, index); 
    END (* IF *); 
  END CatGetChar;

PROCEDURE CatOldGetChar (t: T;  index: CARDINAL): CHAR =
  BEGIN
    IF (index < t.a_len) THEN 
      TextStats.NoteRecurse (Op.get_char); 
      RETURN t.a.get_char (index); 
    END;
    DEC (index, t.a_len);

    IF (index < t.b_len) THEN 
      TextStats.NoteRecurse (Op.get_char); 
      RETURN t.b.get_char (index); 
    END;
    DEC (index, t.b_len);

    index := -1;  (* force a range fault *) <*NOWARN*>
  END CatOldGetChar;

PROCEDURE CatNewGetChar (Text: T;  index: CARDINAL): CHAR =
  VAR Child: TEXT;  
  BEGIN
    LOOP 
      IF (index < Text.a_len) 
      THEN (* It's in the a side. *)
        Child := Text.a;
      ELSE  
        DEC (index, Text.a_len);
        IF index < Text.b_len 
        THEN (* Its in the b side. *) 
          Child := Text.b;
        ELSE
          index := -1;  (* force a range fault *) <*NOWARN*>
        END; 
      END; 
      IF ISTYPE (Child, T) 
      THEN (* Dispatch and Tail-recursion elimination of the code in the
              ELSE clause below. *) 
        TextStats.NoteIter (Op.get_char); 
        Text := Child;
        (* index is already set. *) 
        (* And loop. *)  
      ELSE 
        TextStats.NoteRecurse (Op.get_char); 
        RETURN Child.get_char (index); 
      END; 
    END; 
  END CatNewGetChar;

PROCEDURE CatGetWideChar (t: T; index: CARDINAL): WIDECHAR = 
  BEGIN
    IF TextClass.Old 
    THEN RETURN CatOldGetWideChar (t, index); 
    ELSE RETURN CatNewGetWideChar (t, index); 
    END (* IF *); 
  END CatGetWideChar;

PROCEDURE CatOldGetWideChar (t: T;  index: CARDINAL): WIDECHAR =
  BEGIN
    IF (index < t.a_len) THEN 
      TextStats.NoteRecurse (Op.get_wide_char); 
      RETURN t.a.get_wide_char (index); 
    END;
    DEC (index, t.a_len);

    IF (index < t.b_len) THEN 
      TextStats.NoteRecurse (Op.get_wide_char); 
      RETURN t.b.get_wide_char (index); 
    END;
    DEC (index, t.b_len);

    index := -1;  (* force a range fault *) <*NOWARN*>
  END CatOldGetWideChar;

PROCEDURE CatNewGetWideChar (Text: T;  index: CARDINAL): WIDECHAR =
  VAR Child: TEXT;  
  BEGIN
    LOOP 
      IF (index < Text.a_len) 
      THEN (* It's in the a side. *)
        Child := Text.a;
      ELSE  
        DEC (index, Text.a_len);
        IF index < Text.b_len 
        THEN (* Its in the b side. *) 
          Child := Text.b;
        ELSE
          index := -1;  (* force a range fault *) <*NOWARN*>
        END; 
      END; 
      IF ISTYPE (Child, T) 
      THEN (* Dispatch and Tail-recursion elimination of the code in the
              ELSE clause below. *) 
        TextStats.NoteIter (Op.get_wide_char); 
        Text := Child;
        (* index is already set. *) 
        (* And loop. *)  
      ELSE 
        TextStats.NoteRecurse (Op.get_wide_char); 
        RETURN Child.get_wide_char (index); 
      END; 
    END; 
  END CatNewGetWideChar;

PROCEDURE CatGetChars (t: T;  VAR a: ARRAY OF CHAR;  start: CARDINAL) =
  BEGIN
    IF TextClass.Old 
    THEN OldGetChars (t, a, start); 
    ELSE NewGetChars (t, a, start); 
    END (* IF *); 
  END CatGetChars;

PROCEDURE OldGetChars (t: T;  VAR a: ARRAY OF CHAR;  start: CARDINAL) =
  VAR u: TEXT;  a_offset, t_offset, u_offset: CARDINAL := 0;
  VAR RDepth: CARDINAL; 
  BEGIN
    u := t.a;
    IF (t_offset + t.a_len > start) THEN
      u_offset := MAX (start - t_offset, 0);
      TextStats.SaveRecurseDepth (Op.get_chars, RDepth); 
      TextStats.NoteRecurse (Op.get_chars); 
      u.get_chars (SUBARRAY (a, a_offset, NUMBER (a) - a_offset), u_offset);
      TextStats.RestoreRecurseDepth (Op.get_chars, RDepth); 
      INC (a_offset, t.a_len - u_offset);
      IF (a_offset >= NUMBER (a)) THEN RETURN; END;
    END;
    INC (t_offset, t.a_len);

    u := t.b;
    IF (t_offset + t.b_len > start) THEN
      u_offset := MAX (start - t_offset, 0);
      TextStats.NoteRecurse (Op.get_chars); 
      u.get_chars (SUBARRAY (a, a_offset, NUMBER (a) - a_offset), u_offset);
      INC (a_offset, t.b_len - u_offset);
      IF (a_offset >= NUMBER (a)) THEN RETURN; END;
    END;
    INC (t_offset, t.b_len);
  END OldGetChars;

PROCEDURE MiddleGetChars 
  (t: T;  VAR Chars: ARRAY OF CHAR;  start: CARDINAL) =
  VAR Chars_offset, b_offset: CARDINAL;
  VAR RDepth: CARDINAL; 
  BEGIN 
    IF start < t.a_len  THEN
      TextStats.SaveRecurseDepth (Op.get_chars, RDepth); 
      TextStats.NoteRecurse (Op.get_chars); 
      t.a.get_chars (SUBARRAY (Chars, 0, NUMBER (Chars)), start);
      TextStats.RestoreRecurseDepth (Op.get_chars, RDepth); 
      Chars_offset := t.a_len - start;
      IF Chars_offset >= NUMBER (Chars) THEN RETURN; END;
    ELSE Chars_offset := 0
    END;

    IF start < t.a_len + t.b_len THEN
      b_offset := MAX (start - t.a_len, 0);
      TextStats.NoteRecurse (Op.get_chars); 
      t.b.get_chars 
        ( SUBARRAY (Chars, Chars_offset, NUMBER (Chars) - Chars_offset), 
          b_offset
        );
    END;
  END MiddleGetChars;

PROCEDURE NewGetChars (t: T;  VAR Chars: ARRAY OF CHAR;  start: CARDINAL) =
  VAR a_charCt, b_charCt, a_start, b_start, Chars_start, Chars_charCt: CARDINAL;
  VAR RDepth: CARDINAL; 
  BEGIN 
    Chars_start := 0;
    Chars_charCt := NUMBER(Chars); 
    LOOP (* For tail-recursion elimination. *) 
      a_start := MIN(start, t.a_len);
      a_charCt := MIN (t.a_len - a_start, Chars_charCt);
      b_start := MIN (MAX (start - t.a_len, 0), t.b_len);  
      b_charCt := MIN (t.b_len - b_start, Chars_charCt - a_charCt); 
      IF b_charCt = 0
      THEN (* The b-side is empty, for us. *) 
        IF a_charCt > 0 
        THEN
          IF ISTYPE (t.a, T) 
          THEN 
            (* Dispatch and Tail-recursion elimination of the code in the
               ELSE clause below. *) 
            TextStats.NoteIter (Op.get_chars); 
            t := t.a;
            Chars_charCt := a_charCt;
            start := a_start
            (* And loop. *)  
          ELSE
            TextStats.NoteRecurse (Op.get_chars); 
            t.a.get_chars 
              (SUBARRAY (Chars, Chars_start, a_charCt), a_start); 
            EXIT  
          END (* IF *) 
        END (* IF *) 
      ELSE (* We have some chars from the b-side. *) 
        IF a_charCt > 0 
        THEN 
          TextStats.SaveRecurseDepth (Op.get_chars, RDepth); 
          TextStats.NoteRecurse (Op.get_chars); 
          t.a.get_chars 
            (SUBARRAY (Chars, Chars_start, a_charCt), a_start); 
          TextStats.RestoreRecurseDepth (Op.get_chars, RDepth); 
        END (* IF *) 
      ; IF ISTYPE (t.b, T)
        THEN
          (* Dispatch and Tail-recursion elimination of the code in the
             ELSE clause below. *) 
          TextStats.NoteIter (Op.get_chars); 
          t := t.b;
          INC(Chars_start, a_charCt);
          Chars_charCt := b_charCt;
          start := b_start
          (* And loop. *)  
        ELSE 
          TextStats.NoteRecurse (Op.get_chars); 
          t.b.get_chars 
            (SUBARRAY (Chars, Chars_start + a_charCt, b_charCt), b_start); 
          EXIT
        END (* IF*)
      END (* IF*) 
    END (* LOOP *) 
  END NewGetChars;

PROCEDURE CatGetWideChars (t: T;  VAR a: ARRAY OF WIDECHAR;  start: CARDINAL) =
  BEGIN
    IF TextClass.Old 
    THEN OldGetWideChars (t, a, start); 
    ELSE NewGetWideChars (t, a, start); 
    END (* IF *); 
  END CatGetWideChars;

PROCEDURE OldGetWideChars (t: T;  VAR a: ARRAY OF WIDECHAR;  start: CARDINAL) =
  VAR u: TEXT;  a_offset, t_offset, u_offset: CARDINAL := 0;
  VAR RDepth: CARDINAL; 
  BEGIN
    u := t.a;
    IF (t_offset + t.a_len > start) THEN
      u_offset := MAX (start - t_offset, 0);
      TextStats.SaveRecurseDepth (Op.get_wide_chars, RDepth); 
      TextStats.NoteRecurse (Op.get_wide_chars); 
      u.get_wide_chars 
        (SUBARRAY (a, a_offset, NUMBER (a) - a_offset), u_offset);
      TextStats.RestoreRecurseDepth (Op.get_wide_chars, RDepth); 
      INC (a_offset, t.a_len - u_offset);
      IF (a_offset >= NUMBER (a)) THEN RETURN; END;
    END;
    INC (t_offset, t.a_len);

    u := t.b;
    IF (t_offset + t.b_len > start) THEN
      u_offset := MAX (start - t_offset, 0);
      TextStats.NoteRecurse (Op.get_wide_chars); 
      u.get_wide_chars 
        (SUBARRAY (a, a_offset, NUMBER (a) - a_offset), u_offset);
      INC (a_offset, t.b_len - u_offset);
      IF (a_offset >= NUMBER (a)) THEN RETURN; END;
    END;
    INC (t_offset, t.b_len);
  END OldGetWideChars;

PROCEDURE NewGetWideChars 
  (Text: T;  VAR Chars: ARRAY OF WIDECHAR;  start: CARDINAL) =
  VAR a_charCt, b_charCt, a_start, b_start, Chars_start, Chars_charCt: CARDINAL;
  VAR RDepth: CARDINAL; 
  BEGIN 
    Chars_start := 0;
    Chars_charCt := NUMBER(Chars); 
    LOOP (* For tail-recursion elimination. *) 
      a_start := MIN(start, Text.a_len);
      a_charCt := MIN (Text.a_len - a_start, Chars_charCt);
      b_start := MIN (MAX (start - Text.a_len, 0), Text.b_len);  
      b_charCt := MIN (Text.b_len - b_start, Chars_charCt - a_charCt); 
      IF b_charCt = 0
      THEN (* The b-side is empty, for us. *) 
        IF a_charCt > 0 
        THEN
          IF ISTYPE (Text.a, T) 
          THEN 
            (* Dispatch and Tail-recursion elimination of the code in the
               ELSE clause below. *) 
            TextStats.NoteIter (Op.get_wide_chars); 
            Text := Text.a;
            Chars_charCt := a_charCt;
            start := a_start
            (* And loop. *)  
          ELSE
            TextStats.NoteRecurse (Op.get_wide_chars); 
            Text.a.get_wide_chars 
              (SUBARRAY (Chars, Chars_start, a_charCt), a_start); 
            EXIT  
          END (* IF *)
        ELSE EXIT (* Shouldn't happen, if a_len and b-len values are right. *)  
        END (* IF *) 
      ELSE (* We have some chars from the b-side. *)
        IF a_charCt > 0
        THEN
          TextStats.SaveRecurseDepth (Op.get_wide_chars, RDepth); 
          TextStats.NoteRecurse (Op.get_wide_chars); 
          Text.a.get_wide_chars 
            (SUBARRAY (Chars, Chars_start, a_charCt), a_start); 
          TextStats.RestoreRecurseDepth (Op.get_wide_chars, RDepth); 
        END (* IF *) 
      ; IF ISTYPE (Text.b, T)
        THEN
          (* Dispatch and Tail-recursion elimination of the code in the
             ELSE clause below. *) 
          TextStats.NoteIter (Op.get_wide_chars); 
          Text := Text.b;
          INC(Chars_start, a_charCt);
          Chars_charCt := b_charCt;
          start := b_start
          (* And loop. *)  
        ELSE 
          TextStats.NoteRecurse (Op.get_wide_chars); 
          Text.b.get_wide_chars 
            (SUBARRAY (Chars, Chars_start + a_charCt, b_charCt), b_start); 
          EXIT 
        END (* IF*)
      END (* IF*) 
    END (* LOOP *) 
END NewGetWideChars;

BEGIN
END TextCat.

