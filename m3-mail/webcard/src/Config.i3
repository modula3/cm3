(* Copyright 1994 Digital Equipment Corporation. *)
(* Distributed only by permission. *)

(* Postcard - a user interface for mail and news *)
(* Configuration options *)

(* Last modified on Fri Apr 22 14:23:52 PDT 1994 by birrell   *)

INTERFACE Config;

IMPORT Font, FormsVBT, TextPort, VBT;

EXCEPTION Error(TEXT);

CONST
  BuiltInFontFamily = "Built in"; (* Text for built-in font in config. dlg *)
  NoFontSize = "";                (* Absence of font size in config. dlg *)

TYPE
  T <: Public;

  Public = OBJECT
      mailCheckInterval: INTEGER;
      newsCheckInterval: INTEGER;
      displayFont, displayFontSize: TEXT;
      tFont: Font.T;
      autoDisplayMessages: BOOLEAN;
      externalCompose: BOOLEAN;
      composeWindow: BOOLEAN;
      autoCcToYourself: BOOLEAN;
      autoFcc: BOOLEAN;
      autoFccFolder: TEXT;
      reallyDeleteMessages: BOOLEAN;
      deleteMessagesToFolder: TEXT;
      includeMessageInDraft: BOOLEAN;
      includeReplyString: TEXT;
      printFilter, editorFilter, psViewFilter, psPrintFilter: TEXT;
      purgeSaveMessages: INTEGER;
      scale: INTEGER; (* JRM *)
      model: TextPort.SpecificModel; (* JRM *)
    METHODS
      init(): T;
        (* LL = any, subject to excluding other methods *)
        (* Initialize the configuration to reasonable default values *)
      check(v: VBT.T) RAISES { Error };
        (* LL = actions *)
        (* Sanity check the configuration, for use on "v"s screen, correcting
           any errors before reporting them. *)
      fromFile() RAISES { Error };
        (* LL = actions *)
        (* Read configuration from the user's configuration file,
           as much as possible, without checking it, but reporting any file
           errors. *)
      toFile(version: TEXT) RAISES { Error };
        (* LL = actions *)
        (* Write the (assumed already checked) configuration to the user's
           configuration file, reporting any file errors. *)
      toDlg(fv: FormsVBT.T);
        (* LL = actions *)
        (* Update the dialog to reflect the configuration *)
      fromDlg(fv: FormsVBT.T);
        (* LL = actions *)
        (* Update the configuration with data from the dialog, without checking
           it. *)
      setFonts(v: VBT.T);
        (* LL = VBT.mu *)
        (* Set text fonts within v and its descendents to match the
           configuration *)
  END;

END Config.
