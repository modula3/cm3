(* Copyright 1997, Critical Mass, Inc.  All rights reserved. *)

INTERFACE PasswordFont;

(* Used in conjunction with a type-in, this font
   can create a disguised input field. *)

IMPORT Font;

PROCEDURE New (base: Font.T := Font.BuiltIn;
               ch  : CHAR   := '*'          ): Font.T;
(* Returns a new font that displays all characters as if they
   were character "ch" in font "base".  These fonts are useful
   for password type-in areas where over-the-shoulder browsing
   is not wanted, but alone they do not provide security.  If
   the workstation is unattended and the password field can
   be cut and pasted, the password can be stolen. *)

END PasswordFont.
