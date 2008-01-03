(* Copyright 1989, 1990, 1991, 1992 Digital Equipment Corporation.  *)
(* Distributed only by permission.                                  *)
(* Last modified on Tue Jul 27 13:31:45 PDT 1993 by birrell         *)
(*      modified on Fri Jun 14 11:31:43 PDT 1991 by chan            *)
(*      modified on Tue May 30  7:56:19 PDT 1989 by discolo         *)
(*      modified on Tue Jan  5 11:31:53 PST 1988 by mbrown          *)

INTERFACE MailUtilities;
  (* Mail aliasing and header processing code used by Postcard.
     Index: mail; mail headers; Postcard *)

IMPORT ASCII, Rd, Text, TextList;

EXCEPTION Error (Text.T);
  (* arg is an error message suitable for displaying to the user. *)


(**************)
(* WhiteSpace *)
(**************)

CONST
  DefaultWS = SET OF CHAR{' ', '\t', '\n', '\r', '\f', ASCII.VT, ASCII.NUL};
  (* blank, tab, newline (aka linefeed), carriage-return, form-feed, vertical
     tab, nul (aka chr(0) *)

PROCEDURE LTrim(s: Text.T; whiteSpace: SET OF CHAR := DefaultWS): Text.T;
  (* Delete the leading whiteSpace from s *)


(*********)
(* Lists *)
(*********)

PROCEDURE ConcatText(text1, text2: Text.T; separator: Text.T := "";
                     whiteSpace: SET OF CHAR := DefaultWS): Text.T RAISES {};
  (* If text1 and text2 both contain non-whitespace characters, returns
     Text.Cat(text1, separator, text2); otherwise, returns Text.Cat(text1,
     text2). *)


(************)
(* Messages *)
(************)

PROCEDURE NewMessage(to, cc, fcc, subject, inReplyTo, body: Text.T): Text.T;
  (* Returns a message draft using given fields, which should not contain
     any newlines (unless escaped by RFS822 rules).  If fcc or inReplyTo
     is empty, it is omitted entirely.  The body is prefixed by "\n------". *)

PROCEDURE GetHeader(rd: Rd.T): TEXT RAISES { Rd.Failure };
  (* Returns the header of the message on the given reader, leaving the
     reader positioned imediately after the header. *)

PROCEDURE FilterHeader(header: TEXT; fields: TextList.T): TEXT;
  (* Returns a header containing only the given fields of the old header *)

PROCEDURE GetFieldValue(header: TEXT; fieldname: TEXT; nth: INTEGER := 1): TEXT;
  (* 'fieldname' is the name of the field.  Returns the value of the nth
     occurrence of the field. Although the result may contain newlines (in
     the case of a multi-line value), the last character in the result will
     never be a newline. Returns "" if there are less than "nth" occurrences
     of the field in the header. *)

PROCEDURE GetAllFieldValues(header: Text.T; fieldname: Text.T; separator: Text.T := " "): Text.T
   RAISES {};
  (* 'fieldname' is the name of the field.  Returns the contatenation of the
     values of all occurrences of the field (with ", " separators). Returns
     "" if the field does not exist in the header. *)


(***********)
(* Aliases *)
(***********)

PROCEDURE ExpandAliases (message: Text.T): Text.T RAISES {Error};
  (* Expands the aliases embedded in the To:, Cc:, and Bcc: fields (as
     defined by GetField) of the header (as defined by GetHeader).
     Returns 'message' if no expansion was done. The client can simply
     compare pointers to determine whether expansion was done.
     The items in the field value is defined by
     TextList.FromText(field-value, ",", TextList.DefaultWS, TRUE). If the
     alias table contains an entry for an item, the item is replaced by its
     expansion. The alias is not case-sensitive.
     Before any expansions are done, if NeedToReadAliasFile(), then
     ReadMailAliasFile() *)

PROCEDURE SetAliasFile (filename: Text.T) RAISES {};
  (* By default, the mail alias file is "$HOME/.mail_aliases.sx". Sets the
     mail alias file to filename and ForceAliasFileRead(). *)

PROCEDURE GetAliasFile (): Text.T RAISES {};
  (* Returns the current alias file. *)

PROCEDURE NeedToReadAliasFile (): BOOLEAN RAISES {};
  (* Returns TRUE if the mail alias file needs to be read, either because
     some operation caused it (e.g. SetAliasFile) or because the file was
     modified. *)

PROCEDURE ReadAliasFile () RAISES {Error};
  (* Processes the alias file.   If successful, NeedToReadAliasFile() returns
     FALSE. *)

PROCEDURE ForceAliasFileRead () RAISES {};
  (* Causes NeedToReadAliasFile() to return TRUE. *)

PROCEDURE GetAlias (alias: Text.T): Text.T RAISES {Error};
  (* Resolves and returns the value of alias. *)

PROCEDURE ClearAliases () RAISES {};
  (* Clears the alias table. *)


END MailUtilities.


(* Created on Tue Dec  1 17:51:06 1987 by chan *)
