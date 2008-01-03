(* Last modified on Sat Mar 30 13:52:17 PST 1996 by heydon       *)

INTERFACE JunoConfig;

(* A centralized interface for Juno-2 configuration variables. *)

IMPORT Rd, Pathname, Font, OSError, VBT, Region;

TYPE
  Origin = { Center, SW };
  Orientation = { Portrait, Landscape };

CONST
  OrientName = ARRAY Orientation OF TEXT{"Portrait", "LandScape"};

VAR
  (* The following are the global configuration variables provided by this
     interface. The are initialized by the "Init" procedure below. *)

  textFont: Font.T;    (* default: helvetica-bold-10 *)
  (* The font used for the text in most of the Juno-2 user interface: the
     names of menus, names of tools in the tool palette, and error messages. *)

  codeFont: Font.T;    (* default: courier-bold-10 *)
  (* The font used to display Juno-2 source code. *)

  labelFont: Font.T;   (* default: helvetica-bold-12 *)
  (* The font used for point labels in the drawing view. *)

  dot, cross, crossBdry: Region.T;
  (* Regions derived from "DotSize" and "CrossSize". "DotSize" specifies the
     radius (in pixels) of the dots drawn with labeled points in the drawing
     view. "CrossSize" specifies the "radius" (in pixels) of the cross point-
     annotations in the drawing view. The width of each cross-hair increases
     by 1 pixel for each increase of 4 pixels in radius. *)

  chkptIntv: CARDINAL;  (* default: 30 *)
  (* The number of seconds between checkpoints of the current file. *)

  realPrec: CARDINAL;   (* default: 4 *)
  (* The number of digits of precision to which real numbers are unparsed. *)

  previewCmd: TEXT; (* default: "psview -d $Display -t $Title $Filename" *)
  (* The command to run to preview a PostScript file on-line. The command may
     include the $Display, $Title, and $Filename variables as described in
     "ParseCmd" below. It should display the file named by the $Filename
     variable. *)

  printCmd: TEXT;   (* default: "/usr/bin/lpr -J $Title" *)
  (* The command to run to print a PostScript file. The command may include
     the $Title variable as described in "ParseCmd" below. It should print the
     PostScript file piped into it on standard input. *)

  origin: Origin;   (* default: Origin.Center *)
  (* The location of the origin in the drawing view. *)

  orientation: Orientation; (* default: Orientation.Portrait *)
  (* The orientation of the drawing view and of PostScript output. The
     orientation effects the default bounding box and orientation of the image
     on the page. By default, the orientation is "Portrait", and the image is
     rendered on a vertically-oriented 8.5" x 11" page. If "orient" is
     "Landscape", the image is rendered on a horizontally-oriented 8.5" x 11"
     page. *)

EXCEPTION Error(TEXT);

PROCEDURE Init(filename: Pathname.T := NIL): Pathname.T
  RAISES {OSError.E, Error};
(* Read the Juno configuration file "filename", setting the values of the
   global variables above. The name of the file that was actually read is
   returned, or NIL if only the built-in default configuration file was read.
*)

(* If "filename" is non-NIL, "OSError.E" is raised if the file cannot be read.
   If "filename" is NIL, this procedure looks for the file ".juno-config.sx",
   first in the current directory, and then in the user's home directory. If
   neither file is found, the defaults are used.

   Before any configuration file is processed, suitable defaults are set for
   each of the above values. Any values specified in a configuration file
   replace these defaults. If the configuration file contains errors, the
   "Error" exception is raised with an error message indicating the problem.
*)

PROCEDURE ParseConfigFile(rd: Rd.T) RAISES {Error};
(* Parse the configuration file from "rd", setting the global variables above
   according to any settings found in the file. *)

(* Here is the grammar for the configuration file:

|    File    	  ::= { '(' Cmd ')' }*
|    Cmd     	  ::= TextCmd | RealCmd | CardCmd | FontCmd
|                   | OrigCmd | OrientCmd
|    TextCmd 	  ::= TextCmdNames TextVal
|    TextCmdNames ::= 'PreviewCmd' | 'PrintCmd'
|    TextVal      ::= <Id> | <Text>
|    RealCmd      ::= RealCmdNames <Real>
|    RealCmdNames ::= 'DotSize'
|    CardCmd      ::= CardCmdNames <Cardinal>
|    CardCmdNames ::= 'CrossSize' | 'CheckpointIntv' | 'RealPrec'
|    FontCmd      ::= FontCmdNames FontSpec*
|    FontCmdNames ::= 'CodeFont' | 'TextFont' | 'LabelFont'
|    FontSpec     ::= LFontSpec | XFontSpec
|    LFontSpec    ::= '(' FontName FontWeight FontSize ')'
|    XFontSpec    ::= <Text>
|    FontName     ::= TextVal
|    FontWeight   ::= 'medium' | 'bold'
|    FontSize     ::= <Cardinal>
|    OrigCmd      ::= 'center' | 'southwest'
|    OrientCmd    ::= 'portrait' | 'landscape'

   In this grammar, items in single quotes are literal characters. Items in
   angle brackets are terminals as recognized by the "Sx" interface. For
   example, a <Text> is a sequence of characters in double quotes. A
   <Cardinal> is a non-negative integer.

   The "FontName" should be the name of an X font, such as "helvetica" or
   "courier". See xfontsel(1) or xlsfonts(1) for a list of valid font names.
   If the font with the given name and given weight is not available in the
   given size, "Init" searches for an available version close to the specified
   size. If none is found, it repeats the search for the named font starting
   at the specified size, but on the second search, it ignores the weight
   specification. *)

PROCEDURE ParseCmd(cmdLine: TEXT; VAR (*OUT*) cmd: TEXT;
  VAR (*OUT*) args: REF ARRAY OF TEXT;
  titleVal, displayVal, filenameVal: TEXT := NIL)
  RAISES {Error};
(* Parse the command-line "cmdLine", producing arguments "cmd" and "args"
   suitable for passing to "Process.Create". *)

(* In particular, "ParseCmd" separates "cmdLine" into whitespace-separated
   words, setting "cmd" to the first one. It then sets "args" to a
   newly-allocated array of texts sufficient to hold the remaining words.
   However, before setting the arguments, it searches for the strings
   "$Title", "$Display", and "$Filename" in each word, replacing these
   characters by the texts "titleVal", "displayVal", and "filenameVal",
   respectively (performing at most one substitution per word). If any
   replaced value is "NIL", "Error" is raised with an appropriate error
   message. *)

PROCEDURE SetFonts(v: VBT.T);
(* Set the fonts of all text VBT's in the VBT tree rooted at "v" to
   "textFont", and of all "TextEditVBT.T's" to "codeFont". *)

END JunoConfig.
