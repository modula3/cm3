(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Jul  4 10:00:19 PDT 1995 by mhb                      *)

INTERFACE TextPortWithButtons;

IMPORT TextPortButton, TextPort;

TYPE
  T <: Public;
  Public = (*FVTypes.Port*) TextPort.T OBJECT
  METHODS
    init(readOnly: BOOLEAN): T;
    insertButton(b: TextPortButton.T);
    numberOfButtons(): CARDINAL;
    getButton(i: CARDINAL): TextPortButton.T;
    clearButtons();
  END;

END TextPortWithButtons.
