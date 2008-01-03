(* Copyright (C) 1994, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(* Last modified on Wed Nov  9 17:05:36 PST 1994 by kalsow    *)

UNSAFE MODULE SilHelp;

IMPORT M3toC, WinDef, WinUser;

PROCEDURE GetMenuEntry (entry: INTEGER): TEXT =
  BEGIN
    IF (FIRST (Menu) <= entry) AND (entry <= LAST (Menu))
      THEN RETURN Menu [entry];
      ELSE RETURN NIL;
    END;
  END GetMenuEntry;

PROCEDURE Show (wnd: WinDef.HWND;  entry: INTEGER) =
  BEGIN
    IF (FIRST (Body) <= entry) AND (entry <= LAST (Body))
      AND (Body[entry] # NIL) THEN
      EVAL WinUser.MessageBox (wnd,
                               M3toC.TtoS (Body[entry]),
                               M3toC.TtoS ("WindowSIL Help"),
                               WinUser.MB_OK);
    END;
  END Show;

CONST
  Menu = ARRAY OF TEXT {
    "&Overview",
    "",
    "&File menu",
    "F&ont menu",
    "&Settings menu",
    "&Edit menu",
    "&Help menu",
    "",
    "&Marks\tLB",
    "&Lines\tMB",
    "&Selections\tRB",
    "&Arcs \t^A",
    "&Rectangles\t^R",
    "&Groups\t^X / ^Y",
    "S&trings",
    "",
    "Sc&rolling",
    "&Copy\t^C",
    "&Delete\t^D / ^U",
    "Mo&ve\t^V",
    "",
    "Misc. details"
  };

CONST
  Body = ARRAY OF TEXT {

  "WindowSIL"
  & "\n\n"
  & "C. Thacker, 5/14/94"
  & "\n\n"
  & "WindowSIL is a reincarnation of the Simple ILlustrator, "
  & "a program originally (1975) used to create logic "
  & "diagrams and other illustrations containing lines, text, "
  & "and simple geometric shapes.  It departs from "
  & "much of the MS-Windows user interface doctrine, in that many "
  & "commands may only be invoked from the "
  & "keyboard.  WindowSIL requires a three-button mouse.  The buttons "
  & "are named Mark (left), Line "
  & "(middle), and Select (right).  Clicking Mark plants tiny crosses "
  & "in the picture.  These marks may be used "
  & "as arguments to subsequent commands.  The Draw button draws lines. "
  & "By default, lines are either vertical "
  & "or horizontal, but by pressing the shift key before releasing Draw, "
  & "lines of arbitrary orientation may be "
  & "created.  WindowSIL can also draw rectangles,  circular arcs, and "
  & "text strings  in any (TrueType) font "
  & "installed on the computer.  The Select button is used to select "
  & "objects to be operated upon by subsequent "
  & "commands.",

  NIL,

  "File menu "
  & "\n\n"
  & "The File menu displays the standard Windows Open, Save, SaveAs, Print, "
  & "and Exit options. "
  & "\n\n"
  & "When a file is Opened, the current drawing (if any) is deleted unless "
  & "the Shift key is depressed at the time the Open dialog is selected. "
  & "If Shift is depressed, the new file is added to the drawing. "
  & "\n\n"
  & "Pages are printed with one pixel equal to two printer pixels, so the "
  & "standard A-sized drawing frame (from the file frameal.sil) fills an "
  & "8.5 by 11 inch landscape page when printed at 300 dots per inch. ",

  "Font menu "
  & "\n\n"
  & "The Font menu displays the current set of fonts.  The first item is "
  & "always 'Select...', which pops up a dialog displaying all the "
  & "installed fonts.  To select a font, click on one of the items in the "
  & "font menu, which will make it the current font.  When WindowSIL is "
  & "started, there are no default fonts.  You can have up to ten fonts. ",

  "Settings menu "
  & "\n\n"
  & "The Settings menu includes the Grid, Linewidth, and Zoom popups. "
  & "\n\n"
  & "The Grid constrains the mark to lie on a position whose coordinate is "
  & "a power of two -- 1, 2, 4, 8, 16, or 32. Objects are drawn on the "
  & "grid. "
  & "\n\n"
  & "LineWidth sets the width of lines.  If items are selected, their "
  & "line widths are set to the selected value.  Otherwise, the default "
  & "line width is set. "
  & "\n\n"
  & "Zoom magnifies things.  At zoom = 1, a landscape 8.5 by 11 inch page "
  & "fits comfortably on a VGA display.  A zoom of 2 is good for SVGA "
  & "(1280x1024) displays.  Higher levels of magnification are useful for "
  & "precise drawing.  Logic drawings are most easily drawn at a zoom of 3 "
  & "and a grid of 8, using an 8-point font for captions. ",

  "Edit menu "
  & "\n\n"
  & "The Edit menu contains entries to Copy to the "
  & "clipboard, form a Group from, "
  & "and Ungroup the selected items.  "
  & "It can also toggle the display of bounding "
  & "boxes and erase the entire display. ",

  "Help menu "
  & "\n\n"
  & "The Help menu provides brief descriptions (like this one) of "
  & "WindowSIL's features. ",

  NIL,

  "Mark - left mouse button"
  & "\n\n"
  & "The Mark button places a mark in the drawing.  "
  & "Up to four marks may exist at once.  "
  & "Adding a fifth erases the first.  When Mark is pressed, "
  & "the cursor disappears, and a mark appears.  The "
  & "mark may be dragged about using the mouse (while Mark "
  & "is depressed), but it is constrained to be on the "
  & "current grid.  When Mark is released, the mark is planted, "
  & "and cannot be moved thereafter.  Commands "
  & "use the mark(s) as arguments, and erase any marks they don't need.",

  "Line - middle mouse button"
  & "\n\n"
  & "The Line button draws a line from the position of the "
  & "last mark to the position of the "
  & "cursor.  When  Draw is pressed, the cursor disappears, "
  & "and a rubber-band line from the last mark to the "
  & "current cursor position appears.  When Draw is released, "
  & "a new line is drawn, all Marks are erased, all "
  & "other objects that may have been selected are deselected, "
  & "and a new mark is placed at the end of the new "
  & "line (so that another line can be drawn starting at the old "
  & "line's endpoint)."
  & "\n\n"
  & "If the Shift key is pressed at the "
  & "time Draw is released, the line will be drawn as shown. "
  & "Otherwise, the new line will be either vertical or "
  & "horizontal, depending on the distances between the original "
  & "mark and the final position of the line's "
  & "endpoint."
  & "\n\n"
  & "The new line is selected, so that if it was incorrectly "
  & "placed, it may be deleted by typing ^D.",

  "Select - right mouse button"
  & "\n\n"
  & "The Select button is used to select items in the picture.  "
  & "There are two types of selection -- point and sweep.  "
  & "If the Select button is held steady between the down and "
  & "up transitions, then the select mode is \"point\" selection.  "
  & "If the mouse is swept over a larger distance while Select "
  & "is depressed, then the mode is \"sweep\" selection.  "
  & "In \"point\" mode, any items whose bounding boxes contain "
  & "the point are selected.  "
  & "In \"sweep\" mode, any items whose bounding boxes are contained "
  & "in the sweep are selected.  "
  & "Lines (which have small bounding boxes) are selected by pointing "
  & "at their endpoints.  "
  & "\n\n"
  & "Selected items are shown in red (and are also dashed if they are "
  & "of width = 1).  "
  & "Except for showing selected items in red, WindowSIL makes no use "
  & "of color, nor can it create color drawings.  "
  & "\n\n"
  & "When an item is selected, other selected items are deselected, "
  & "unless the Shift key is depressed.  "
  & "If  Shift is pressed at the time Select is released, a selected "
  & "item is added to the set of selected items.  "
  & "\n\n"
  & "Clicking Select at a place on the "
  & "screen that has no items is a good way to delete all the marks.",

  "Arcs - ^A "
  & "\n\n"
  & "Control-A draws a circular arc.  There are two options: full circles "
  & "and partial arcs.  To draw a full circle, there must be only two marks "
  & "present on the screen (click on Select to eliminate other marks, then "
  & "begin).  The first mark specifies the center of the circle, the second "
  & "is a point on the radius.  Type control-A to draw the circle.  The "
  & "second option draws a portion of a circular arc, and needs three marks "
  & "on the screen: The first mark defines the center, the second defines a "
  & "point on the radius.  The third mark defines a line, which when cast "
  & "from the center, intercepts the arc segment.  The arc is drawn "
  & "counterclockwise from the second point to the line defined by the "
  & "third point. ",

  "Rectangles - ^R "
  & "\n\n"
  & "Control-R draws a rectangle.  The last two marks define the corners of "
  & "the rectangle. ",

  "Groups - ^X ^Y "
  & "\n\n"
  & "Control-X is used to group items.  Typing control-X "
  & "replaces the selected items with a single item containing the group. "
  & "This item may be copied or moved as a unit. "
  & "\n\n"
  & "Control-Y is used to ungroup items.  Typing control-Y "
  & "replaces any selected groups with the individual items contained "
  & "in the groups. "
  & "\n\n"
  & "A number of standard "
  & "logic symbols are provided as groups which may be added to drawings "
  & "using shift-OpenFile. ",

  "Strings "
  & "\n\n"
  & "Control characters are used to issue commands, and ordinary keys are "
  & "used to enter text strings into a picture. To add a text string, place "
  & "a mark at the start of the string, and type the string's contents. The "
  & "string will appear in the current font.  To end a string, select "
  & "something else, or type Enter.  Enter deselects the string, and moves "
  & "the mark down by one grid unit (handy for typing several signal names). "
  & "\n\n"
  & "Strings may be modified by selecting them.  When a string is selected, "
  & "characters may be added by typing, or characters may be deleted from "
  & "the end of the string using the backspace key.  No other editing is "
  & "provided. ",

  NIL,

  "Scrolling "
  & "\n\n"
  & "The arrow keys are used to scroll around the picure.  Each depresion "
  & "of 'left', 'right', 'up' or 'down' scrolls in the given dirction by "
  & "200 horizontal units or 150 vertical units (about a third of the "
  & "screen). 'Home' scrolls so that the upper left corner of the picture "
  & "is at the upper left corner of the screen.  It also refreshes the "
  & "display. ",

  "Copy - ^C "
  & "\n\n"
  & "Control-C is used to copy items.  Select the item or items to be "
  & "copied, place a mark to indicate a reference point, place another mark "
  & "to indicate the place the copy is to appear, and type control-C.  The "
  & "copy is made, the copied items are selected, and a mark is placed at "
  & "the target point.  This allows the newly copied items to be moved "
  & "(using control-V) or copied again. ",

  "Delete - ^D/^U "
  & "\n\n"
  & "Control-D deletes all selected items.  Items are never really lost. "
  & "When at item (or a number of selected items) is deleted, it is no "
  & "longer displayed, but is saved internally.  Control-U will undelete "
  & "items, in inverse order of their deletion.  Deleted items are not "
  & "saved when the output file is written. ",

  "Move - ^V "
  & "\n\n"
  & "Control-V is used to move selected items.  Select the item or items to "
  & "be moved, place a mark near the selected items to indicate a reference "
  & "point, place another mark to indicate the place to which the reference "
  & "point and the selected items are to be moved, and type control-V.  The "
  & "selected items will be moved to the new position. ",

  NIL,

  "Details "
  & "\n\n"
  & "The WindowSIL coordinate system is isotropic (square pixels, round "
  & "circles).  The origin is at the upper left corner of the window, the "
  & "X-axis increases to the right, the Y-axis increases downwards. "
  & "\n\n"
  & "The output file is an ASCII text file with the standard extension "
  & "'.sil'.  It contains a description of the fonts used, the groups "
  & "(called 'macros' for historical reasons), and the individual objects "
  & "in the drawing.  Although it is possible to edit a .sil file, "
  & "WindowSIL does no checking on the validity of a file when it reads it, "
  & "so be careful. "
  & "\n\n"
  & "A standard A-sized drawing frame is available as 'frameal.sil'.  The "
  & "Digital logo comes from a TrueType font called 'decfont.ttf', which "
  & "must be installed to use the logo.  The logo is a single character "
  & "('d') in this font. "
  & "\n\n"
  & "A drawing can be printed as a Postscript file, or on a HP LaserWriter "
  & "using the Microsoft Printing System.  Printing on a stock Laserwriter "
  & "doesn't seem to work."

  };

BEGIN
END SilHelp.
