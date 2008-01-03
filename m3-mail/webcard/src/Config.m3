(* Copyright 1994 Digital Equipment Corporation. *)
(* Distributed only by permission. *)

(* Postcard - a user interface for mail and news *)
(* Configuration options *)

(* Last modified on Wed Feb  1 12:19:10 PST 1995 by kalsow    *)
(*      modified on Fri Apr 22 15:14:19 PDT 1994 by birrell   *)

MODULE Config EXPORTS Config;

IMPORT
  AnchorBtnVBT, ASCII,
  Env,
  FloatMode, Fmt, Font, FormsVBT,
  Lex, ListVBT,
  MailUtilities, MiscUtils,
  OSUtils,
  Rd,
  Scan, ScreenType, ScrnFont, Split,
  Text, TextPort, TextRd, TextWr, Thread, TrestleComm,
  UnixMail,
  VBT, VBTClass,
  Wr;

<* FATAL Rd.Failure, Thread.Alerted, Wr.Failure *>

(* *)
(* Types and Constants *)
(* *)

REVEAL T = Public BRANDED OBJECT
    OVERRIDES
      init := Init;
      check := Check;
      fromFile := FromFile;
      toFile := ToFile;
      toDlg := ToDlg;
      fromDlg := FromDlg;
      setFonts := SetFonts;
    END;

CONST
  DefaultPrintFilter = "enscript";
  DefaultEditorFilter = "";
  DefaultPSViewFilter = "dxpsview";
  DefaultPSPrintFilter = "lpr";
  ConfigFileName = (* $HOME *) "/.postcardFV_rc";
  DefaultFont = "Courier";        (* Used if user doesn't type a font family *)
  DefaultFontSize = "12";         (* Used if user doesn't type a font family *)


(* *)
(* Subroutines *)
(* *)

CONST WhiteExceptNL = MailUtilities.DefaultWS - SET OF CHAR{'\n'};

PROCEDURE NonWhiteChar(rd: Rd.T;
                       white: SET OF CHAR := MailUtilities.DefaultWS): CHAR
                       RAISES { Rd.EndOfFile } =
  (* Returns the first non-whitespace character, consuming it from the reader *)
  VAR c: CHAR;
  BEGIN
    REPEAT c := Rd.GetChar(rd) UNTIL NOT (c IN white);
    RETURN c
  END NonWhiteChar;

PROCEDURE SkipWhite(rd: Rd.T;
                    white: SET OF CHAR := MailUtilities.DefaultWS) =
  (* Skips whitespace on reader leaving reader at first non-white char or EOF *)
  BEGIN
    TRY
      EVAL NonWhiteChar(rd, white);
      Rd.UnGetChar(rd);
    EXCEPT Rd.EndOfFile =>
    END;
  END SkipWhite;
  
PROCEDURE TerminatedText(rd: Rd.T; term: SET OF CHAR): TEXT =
  (* Returns a text from the reader, up to but excluding the first char in term
     Leaves the terminating character available in the reader.  Terminates
     without complaint at end of file. *)
  VAR wr: Wr.T; c: CHAR;
  <*FATAL Rd.EndOfFile*>
  BEGIN
    wr := TextWr.New();
    LOOP 
      IF Rd.EOF(rd) THEN EXIT END;
      c := Rd.GetChar(rd);
      IF c IN term THEN Rd.UnGetChar(rd); EXIT END;
      Wr.PutChar(wr, c);
    END;
    RETURN TextWr.ToText(wr);
  END TerminatedText;

EXCEPTION RequiredMissing;

PROCEDURE RequireText(rd: Rd.T; t: TEXT) RAISES { RequiredMissing } =
   (* If "t" occurs next on reader, consume it; else do nothing
      and raise RequiredMissing. *)
    VAR rd2: Rd.T; initPos: CARDINAL;
    <*FATAL Rd.EndOfFile*>
  BEGIN
    initPos := Rd.Index(rd);
    rd2 := TextRd.New(t);
    FOR i := 0 TO Rd.Length(rd2)-1 DO 
      IF Rd.EOF(rd) OR (Rd.GetChar(rd) # Rd.GetChar(rd2)) THEN
        Rd.Seek(rd, initPos); RAISE RequiredMissing
      END;
    END;
  END RequireText;
  
PROCEDURE OKFontName(v: VBT.T; name: TEXT): BOOLEAN =
    (* LL < v *)
  VAR st: VBT.ScreenType;
  VAR o: ScrnFont.Oracle;
  VAR fnt: ScrnFont.T;
  BEGIN
    st := VBT.ScreenTypeOf(v);
    IF st = NIL THEN RETURN TRUE END;
    o := st.font;
    TRY
      fnt := o.lookup(name);
    EXCEPT
      | ScrnFont.Failure => RETURN FALSE;
      | TrestleComm.Failure => RETURN TRUE; (* can't check *)
    END;
    RETURN fnt # NIL
  END OKFontName;

PROCEDURE FontName(foundry: TEXT := "*";
                   family: TEXT;
                   weight: TEXT := "Medium";
                   slant: TEXT := "R";
                   pts: TEXT): TEXT =
    (* LL = arbitrary *)
  BEGIN
    IF MiscUtils.Equal(pts, NoFontSize, TRUE) THEN
      RETURN family
    ELSE
      IF Text.GetChar(pts, Text.Length(pts) - 1) IN ASCII.Digits THEN
        pts := pts & "0"; (* "decipoints"! *)
      END;
      RETURN "-" & foundry & "-" & family & "-" & weight & "-" & slant
                 & "-Normal-*-*-" & pts & "-*-*-*-*-ISO8859-1"
    END;
  END FontName;

TYPE EditModelTexts = ARRAY TextPort.SpecificModel OF TEXT;

CONST
  ModelNames = EditModelTexts{ "ivy", "emacs", "mac", "xterm" };
  ModelChoiceNames = EditModelTexts{
                       "ivyModel", "emacsModel", "macModel", "xtermModel" };

PROCEDURE ResolveModel(name : TEXT;
                      READONLY names: EditModelTexts): TextPort.SpecificModel =
  BEGIN
    FOR m := FIRST(TextPort.SpecificModel) TO LAST(TextPort.SpecificModel) DO
      IF Text.Equal(name, names[m]) THEN RETURN m END
    END;
    RETURN TextPort.DefaultModel
  END ResolveModel;
   

(* *)
(* Public methods *)
(* *)

PROCEDURE Init(t: T): T =
  BEGIN
    t.mailCheckInterval := 1;
    t.newsCheckInterval := 30;
    t.displayFont := DefaultFont;
    t.displayFontSize := DefaultFontSize;
    t.tFont := Font.BuiltIn;
    t.autoDisplayMessages := TRUE;
    t.externalCompose := FALSE;
    t.composeWindow := TRUE;
    t.autoCcToYourself := TRUE;
    t.autoFcc := FALSE;
    t.autoFccFolder := "";
    t.reallyDeleteMessages := TRUE;
    t.deleteMessagesToFolder := "";
    t.includeMessageInDraft := FALSE;
    t.includeReplyString := "";
    t.printFilter := DefaultPrintFilter;
    t.editorFilter := DefaultEditorFilter;
    t.psViewFilter := DefaultPSViewFilter;
    t.psPrintFilter := DefaultPSPrintFilter;
    t.purgeSaveMessages := 50;
    t.scale := 100; (* JRM *)
    t.model := TextPort.DefaultModel;
    RETURN t
  END Init;

PROCEDURE Check(t: T; v: VBT.T) RAISES { Error } =
    (* LL = actions *)
  VAR
    failure: TEXT := NIL;
  PROCEDURE CheckFilter(VAR new: TEXT; default: TEXT) =
    BEGIN
      IF Text.Empty(new) THEN new := default END;
    END CheckFilter;
  PROCEDURE CheckWritableFolder(VAR flag: BOOLEAN;
                                on: BOOLEAN;
                                prompt, name: TEXT) =
    BEGIN
      IF flag = on THEN
        flag := NOT on;
        IF Text.Empty(name) THEN
          failure := "you didn't specify the " & prompt;
        ELSIF NOT UnixMail.FolderExists(name) THEN
          failure := "the " & prompt & " \"" & name & "\" doesn\'t exist";
        ELSIF UnixMail.IsBBoard(name) THEN
          failure := "the " & prompt & " \"" & name & "\" is a bulletin board";
        ELSE
          flag := on;
        END;
      END;
    END CheckWritableFolder;
  VAR names: ARRAY [0..0] OF TEXT;
  BEGIN
    t.mailCheckInterval := MAX(1, t.mailCheckInterval);
    t.newsCheckInterval := MAX(5, t.newsCheckInterval);
    IF Text.Empty(t.displayFont) THEN
      t.displayFont := DefaultFont;
      t.displayFontSize := DefaultFontSize;
    END;
    IF MiscUtils.Equal(t.displayFont, BuiltInFontFamily, TRUE) THEN
      t.tFont := Font.BuiltIn
    ELSE
      names[0] := FontName(family := t.displayFont,
                           pts := t.displayFontSize);
      IF OKFontName(v, names[0]) THEN
        t.tFont := Font.FromName(names);
      ELSE
        t.displayFont := BuiltInFontFamily;
        t.displayFontSize := "";
        t.tFont := Font.BuiltIn;
        failure := "invalid font name \"" & names[0] & "\"";
      END;
    END;
    CheckWritableFolder(t.autoFcc, TRUE,
                               "auto Fcc folder",
                               t.autoFccFolder);
    CheckWritableFolder(t.reallyDeleteMessages, FALSE,
                               "deleted messages folder",
                               t.deleteMessagesToFolder);
    CheckFilter(t.printFilter, DefaultPrintFilter);
    CheckFilter(t.editorFilter, DefaultEditorFilter);
    CheckFilter(t.psViewFilter, DefaultPSViewFilter);
    CheckFilter(t.psPrintFilter, DefaultPSPrintFilter);
    t.purgeSaveMessages := MAX(0, t.purgeSaveMessages);
    t.scale := MAX(10, MIN(300, t.scale));
    IF failure # NIL THEN RAISE Error("Bad configuration: " & failure) END;
  END Check;

PROCEDURE FromFile(t: T) RAISES { Error } =
  (* LL = actions *)
  VAR
    configName := Env.Get("HOME") & ConfigFileName;
    rd: Rd.T;
  BEGIN
    EVAL t.init();
    TRY
      rd := OSUtils.OpenRead(configName);
      TRY
        SkipWhite(rd);
        RequireText(rd, "Postcard");
        SkipWhite(rd, WhiteExceptNL);
        EVAL TerminatedText(rd, SET OF CHAR {'\n'});
        SkipWhite(rd);
        RequireText(rd, "Mail check interval:");
        SkipWhite(rd, WhiteExceptNL);
        t.mailCheckInterval :=
          Scan.Int(TerminatedText(rd, SET OF CHAR {'\n'}));
        SkipWhite(rd);
        RequireText(rd, "News check interval:");
        SkipWhite(rd, WhiteExceptNL);
        t.newsCheckInterval :=
          Scan.Int(TerminatedText(rd, SET OF CHAR {'\n'}));
        TRY                      (* new format *)
          SkipWhite(rd);
          RequireText(rd, "Display font family:");
          SkipWhite(rd, WhiteExceptNL);
          t.displayFont := TerminatedText(rd, SET OF CHAR {'\n'});
          SkipWhite(rd);
          RequireText(rd, "Display font size:");
          SkipWhite(rd, WhiteExceptNL);
          t.displayFontSize := TerminatedText(rd, SET OF CHAR {'\n'});
        EXCEPT
          RequiredMissing =>     (* old format *)
            SkipWhite(rd);
            RequireText(rd, "Display font:");
            SkipWhite(rd, WhiteExceptNL);
            EVAL TerminatedText(rd, SET OF CHAR {'\n'});
        END;
        TRY                      (* old sound stuff *)
          SkipWhite(rd);
          RequireText(rd, "Play soundfile at new mail:");
          SkipWhite(rd, WhiteExceptNL);
          EVAL(Rd.GetChar(rd) = 'Y');
          EVAL TerminatedText(rd, SET OF CHAR {'\n'});
          SkipWhite(rd);
          RequireText(rd, "Sound file:");
          SkipWhite(rd, WhiteExceptNL);
          EVAL TerminatedText(rd, SET OF CHAR {'\n'});
        EXCEPT
          RequiredMissing =>     (* no sound stuff *)
        END;
        SkipWhite(rd);
        RequireText(rd, "Auto display messages:");
        SkipWhite(rd, WhiteExceptNL);
        t.autoDisplayMessages :=(Rd.GetChar(rd) = 'Y');
        EVAL TerminatedText(rd, SET OF CHAR {'\n'});
        TRY
          SkipWhite(rd);
          RequireText(rd, "Use external compose:");
          SkipWhite(rd, WhiteExceptNL);
          t.externalCompose := (Rd.GetChar(rd) = 'Y');
          EVAL TerminatedText(rd, SET OF CHAR {'\n'});
        EXCEPT
          RequiredMissing =>     (* old format *)
        END;
        TRY
          SkipWhite(rd);
          RequireText(rd, "Use compose window:");
          SkipWhite(rd, WhiteExceptNL);
          t.composeWindow := (Rd.GetChar(rd) = 'Y');
          EVAL TerminatedText(rd, SET OF CHAR {'\n'});
        EXCEPT
          RequiredMissing =>     (* old format *)
        END;
        SkipWhite(rd);
        RequireText(rd, "Auto cc to yourself:");
        SkipWhite(rd, WhiteExceptNL);
        t.autoCcToYourself := (Rd.GetChar(rd) = 'Y');
        EVAL TerminatedText(rd, SET OF CHAR {'\n'});
        SkipWhite(rd);
        RequireText(rd, "Auto fcc:");
        SkipWhite(rd, WhiteExceptNL);
        t.autoFcc := (Rd.GetChar(rd) = 'Y');
        EVAL TerminatedText(rd, SET OF CHAR {'\n'});
        SkipWhite(rd);
        RequireText(rd, "Auto fcc folder:");
        SkipWhite(rd, WhiteExceptNL);
        t.autoFccFolder := TerminatedText(rd, SET OF CHAR {'\n'});
        SkipWhite(rd);
        RequireText(rd, "Really delete messages:");
        SkipWhite(rd, WhiteExceptNL);
        t.reallyDeleteMessages := (Rd.GetChar(rd) = 'Y');
        EVAL TerminatedText(rd, SET OF CHAR {'\n'});
        SkipWhite(rd);
        RequireText(rd, "Deleted messages folder:");
        SkipWhite(rd, WhiteExceptNL);
        t.deleteMessagesToFolder := TerminatedText(rd, SET OF CHAR {'\n'});
        SkipWhite(rd);
        RequireText(rd, "Include message in draft:");
        SkipWhite(rd, WhiteExceptNL);
        t.includeMessageInDraft := (Rd.GetChar(rd) = 'Y');
        EVAL TerminatedText(rd, SET OF CHAR {'\n'});
        SkipWhite(rd);
        RequireText(rd, "Prefixed by:");
        IF Rd.GetChar(rd) = '\n' THEN
          t.includeReplyString := ""
        ELSE
          t.includeReplyString := TerminatedText(rd, SET OF CHAR {'\n'});
        END;
        SkipWhite(rd);
        RequireText(rd, "Print filter:");
        SkipWhite(rd, WhiteExceptNL);
        t.printFilter := TerminatedText(rd, SET OF CHAR {'\n'});
        TRY
          SkipWhite(rd);
          RequireText(rd, "Compose filter:");
          SkipWhite(rd, WhiteExceptNL);
          t.editorFilter := TerminatedText(rd, SET OF CHAR {'\n'});
          SkipWhite(rd);
          RequireText(rd, "PostScript view filter:");
          SkipWhite(rd, WhiteExceptNL);
          t.psViewFilter := TerminatedText(rd, SET OF CHAR {'\n'});
          SkipWhite(rd);
          RequireText(rd, "PostScript print filter:");
          SkipWhite(rd, WhiteExceptNL);
          t.psPrintFilter := TerminatedText(rd, SET OF CHAR {'\n'});
        EXCEPT
          RequiredMissing =>     (* old format *)
        END;
        SkipWhite(rd);
        RequireText(rd, "Purge saves messages:");
        SkipWhite(rd, WhiteExceptNL);
        t.purgeSaveMessages :=
          Scan.Int(TerminatedText(rd, SET OF CHAR {'\n'}));

        TRY                      (* new format - scale information *)
          SkipWhite(rd);
          RequireText(rd, "Scale factor:");
          SkipWhite(rd, WhiteExceptNL);
          t.scale :=
            Scan.Int(TerminatedText(rd, SET OF CHAR {'\n'})); (* JRM *)
        EXCEPT
          RequiredMissing =>
        END;
        TRY                      (* Editing Model.  JRM *)
          SkipWhite(rd);
          RequireText(rd, "Editing style:");
          SkipWhite(rd, WhiteExceptNL);
          t.model :=
            ResolveModel(TerminatedText(rd, SET OF CHAR {'\n'}), ModelNames)
        EXCEPT
          RequiredMissing =>
        END;
      FINALLY
        TRY Rd.Close(rd); EXCEPT Rd.Failure => END;
      END;
    EXCEPT
    | RequiredMissing, Lex.Error, FloatMode.Trap, Rd.EndOfFile =>
        RAISE Error("Damaged configuration file \"" & configName & "\".");
    | Rd.Failure =>
        RAISE Error("Error while reading configuration file \""
                         & configName & "\".");
    | OSUtils.FileNotFound =>
        RAISE Error("Configuration file \"" &
                         configName & "\" not found.");
    | OSUtils.FileError(t) =>
        RAISE Error("Can't open configuration file \"" &
                         configName & "\": " & t);
    END;
  END FromFile;

PROCEDURE ToFile(t: T; version: TEXT) RAISES { Error } =
  (* LL = actions *)
  VAR
    wr: Wr.T;
    configName := Env.Get("HOME") & ConfigFileName;
  CONST NY = ARRAY BOOLEAN OF TEXT {"N\n", "Y\n"};
  BEGIN
    TRY
      wr := OSUtils.OpenWrite(configName, FALSE);
      TRY
        Wr.PutText(wr, "Postcard " & version & "\n");
        Wr.PutText(wr, "Mail check interval: "
                          & Fmt.Int(t.mailCheckInterval) & "\n");
        Wr.PutText(wr, "News check interval: "
                          & Fmt.Int(t.newsCheckInterval) & "\n");
        Wr.PutText(wr, "Display font family: " & t.displayFont & "\n");
        Wr.PutText(wr, "Display font size: " & t.displayFontSize & "\n");
        Wr.PutText(
          wr, "Auto display messages: " & NY [t.autoDisplayMessages]);
        Wr.PutText(
          wr, "Use external compose: " & NY [t.externalCompose]);
        Wr.PutText(wr, "Use compose window: " & NY [t.composeWindow]);
        Wr.PutText(
          wr, "Auto cc to yourself: " & NY [t.autoCcToYourself]);
        Wr.PutText(wr, "Auto fcc: " & NY [t.autoFcc]);
        Wr.PutText(wr, "Auto fcc folder: " & t.autoFccFolder & "\n");
        Wr.PutText(
          wr, "Really delete messages: " & NY [t.reallyDeleteMessages]);
        Wr.PutText(wr, "Deleted messages folder: "
                          & t.deleteMessagesToFolder & "\n");
        Wr.PutText(wr, "Include message in draft: "
                          & NY [t.includeMessageInDraft]);
        Wr.PutText(wr, "Prefixed by: " & t.includeReplyString & "\n");
        Wr.PutText(wr, "Print filter: " & t.printFilter & "\n");
        Wr.PutText(wr, "Compose filter: " & t.editorFilter & "\n");
        Wr.PutText(
          wr, "PostScript view filter: " & t.psViewFilter & "\n");
        Wr.PutText(
          wr, "PostScript print filter: " & t.psPrintFilter & "\n");
        Wr.PutText(wr, "Purge saves messages: "
                          & Fmt.Int(t.purgeSaveMessages) & "\n");
        (* JRM: *)
        Wr.PutText(wr, "Scale factor: " & Fmt.Int(t.scale) & "\n");
        Wr.PutText(wr, "Editing style: " & ModelNames [t.model]
          & "\n");
      FINALLY
        TRY Wr.Close(wr) EXCEPT Wr.Failure => END;
      END;
    EXCEPT
      | OSUtils.FileError(t) =>
        RAISE Error("Can't write " & configName & ": " & t);
      | Wr.Failure =>
        RAISE Error("Failure while writing configuration file.");
    END;
  END ToFile;

PROCEDURE ToDlg(t: T; fv: FormsVBT.T) =
  (* LL = actions *)
  <* FATAL FormsVBT.Error, FormsVBT.Unimplemented *>
  BEGIN
    LOCK VBT.mu DO
      FormsVBT.PutInteger(fv, "mailCheckInterval", t.mailCheckInterval);
      FormsVBT.PutInteger(fv, "newsCheckInterval", t.newsCheckInterval);
      FormsVBT.PutText(fv, "displayFont", t.displayFont);
      FormsVBT.PutText(fv, "displayFontSize", t.displayFontSize);
      FormsVBT.PutBoolean(
          fv, "autoDisplayMessages", t.autoDisplayMessages);
      FormsVBT.PutBoolean(fv, "useExternalCompose", t.externalCompose);
      FormsVBT.PutBoolean(fv, "useComposeWindow", t.composeWindow);
      FormsVBT.PutBoolean(fv, "autoCcToYourself", t.autoCcToYourself);
      FormsVBT.PutBoolean(fv, "autoFcc", t.autoFcc);
      FormsVBT.PutText(fv, "autoFccFolder", t.autoFccFolder);
      FormsVBT.PutBoolean(
          fv, "reallyDeleteMessages", t.reallyDeleteMessages);
      FormsVBT.PutBoolean(fv, "moveMessages", NOT t.reallyDeleteMessages);
      FormsVBT.PutText(
          fv, "deleteMessagesToFolder", t.deleteMessagesToFolder);
      FormsVBT.PutBoolean(
          fv, "includeMessageInDraft", t.includeMessageInDraft);
      FormsVBT.PutText(fv, "includeReplyString", t.includeReplyString);
      FormsVBT.PutText(fv, "printFilter", t.printFilter);
      FormsVBT.PutText(fv, "editFilter", t.editorFilter);
      FormsVBT.PutText(fv, "psViewFilter", t.psViewFilter);
      FormsVBT.PutText(fv, "psPrintFilter", t.psPrintFilter);
      FormsVBT.PutInteger(fv, "purgeSaveMessages", t.purgeSaveMessages);
      FormsVBT.PutChoice(fv, "Model", ModelChoiceNames [t.model]);
    END;
  END ToDlg;

PROCEDURE FromDlg(t: T; fv: FormsVBT.T) =
  (* LL = actions *)
  <* FATAL FormsVBT.Error, FormsVBT.Unimplemented *>
  BEGIN
    LOCK VBT.mu DO
      t.mailCheckInterval :=
        FormsVBT.GetInteger(fv, "mailCheckInterval");
      t.newsCheckInterval :=
        FormsVBT.GetInteger(fv, "newsCheckInterval");
      t.displayFont := FormsVBT.GetText(fv, "displayFont");
      t.displayFontSize := FormsVBT.GetText(fv, "displayFontSize");
      t.autoDisplayMessages :=
        FormsVBT.GetBoolean(fv, "autoDisplayMessages");
      t.externalCompose :=
        FormsVBT.GetBoolean(fv, "useExternalCompose");
      t.composeWindow := FormsVBT.GetBoolean(fv, "useComposeWindow");
      t.autoCcToYourself := FormsVBT.GetBoolean(fv, "autoCcToYourself");
      t.autoFcc := FormsVBT.GetBoolean(fv, "autoFcc");
      t.autoFccFolder := FormsVBT.GetText(fv, "autoFccFolder");
      t.reallyDeleteMessages :=
        FormsVBT.GetBoolean(fv, "reallyDeleteMessages");
      t.deleteMessagesToFolder :=
        FormsVBT.GetText(fv, "deleteMessagesToFolder");
      t.includeMessageInDraft :=
        FormsVBT.GetBoolean(fv, "includeMessageInDraft");
      t.includeReplyString :=
        FormsVBT.GetText(fv, "includeReplyString");
      t.printFilter := FormsVBT.GetText(fv, "printFilter");
      t.editorFilter := FormsVBT.GetText(fv, "editFilter");
      t.psViewFilter := FormsVBT.GetText(fv, "psViewFilter");
      t.psPrintFilter := FormsVBT.GetText(fv, "psPrintFilter");
      t.purgeSaveMessages :=
        FormsVBT.GetInteger(fv, "purgeSaveMessages");
      (* t.scale := FormsVBT.GetInteger(fv, "scaleFactor"); *)
      t.model :=
        ResolveModel(FormsVBT.GetChoice(fv, "Model"), ModelChoiceNames);
    END;
  END FromDlg;

PROCEDURE SetFonts(t: T; v: VBT.T) =
    (* LL = VBT.mu *)
    VAR ch: VBT.T;
    <* FATAL Split.NotAChild *>
  BEGIN
    (* I don't use a TYPECASE because some of the types overlap, e.g. a
       ListVBT.T might well also be a Split.T *)
    IF ISTYPE(v, TextPort.T) THEN
      NARROW(v, TextPort.T).setFont(t.tFont)
    ELSIF ISTYPE(v, ListVBT.T) THEN
      NARROW(NARROW(v, ListVBT.T).painter,
             ListVBT.TextPainter).setFont(v, t.tFont);
    ELSIF ISTYPE(v, Split.T) THEN
      ch := NIL;
      LOOP
        ch := Split.Succ(NARROW(v, Split.T), ch);
        IF ch = NIL THEN EXIT END;
        t.setFonts(ch);
      END;
      (* and also ... *)
      IF ISTYPE(v, AnchorBtnVBT.T) THEN
        t.setFonts(NARROW(v, AnchorBtnVBT.T).menu);
      END;
    END;
  END SetFonts;

BEGIN
END Config.
