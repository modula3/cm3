(* Copyright (C) 1994, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* by Stephen Harrison                                       *)
(*                                                           *)
(* Last modified on Wed Dec 21 09:16:26 PST 1994 by kalsow   *)
(*      modified on Thu Feb 11 13:29:15 PST 1993 by mjordan  *)
(*      modified on Wed Feb 10 19:58:51 PST 1993 by harrison *)

INTERFACE WinCon;

(* Corresponds to build version 0002 of "wincon.h".  See that file for
   details.

   This module contains the public data structures, data types, and
   procedures exported by the NT console subsystem. *)

IMPORT WinBase;

FROM WinDef IMPORT INT16, BOOL, UINT16, UINT32, PUINT32, PUINT16, PVOID;
FROM WinNT IMPORT WCHAR, HANDLE, PSTR, PWSTR;

TYPE
  PCOORD = UNTRACED REF COORD;
  COORD = RECORD
    X: INT16;
    Y: INT16;
  END;

  PSMALL_RECT = UNTRACED REF SMALL_RECT;
  SMALL_RECT = RECORD
    Left  : INT16;
    Top   : INT16;
    Right : INT16;
    Bottom: INT16;
  END;

  PKEY_EVENT_RECORD = UNTRACED REF KEY_EVENT_RECORD;
  KEY_EVENT_RECORD = RECORD
    bKeyDown        : BOOL;
    wRepeatCount    : UINT16;
    wVirtualKeyCode : UINT16;
    wVirtualScanCode: UINT16;
    uChar           : WCHAR; (* ??? *)
                     (* union { WCHAR UnicodeChar; CHAR AsciiChar; } uChar; *)
    dwControlKeyState: UINT32;
  END;

(* ControlKeyState flags *)

CONST
  RIGHT_ALT_PRESSED  = 16_0001; (* the right alt key is pressed. *)
  LEFT_ALT_PRESSED   = 16_0002; (* the left alt key is pressed. *)
  RIGHT_CTRL_PRESSED = 16_0004; (* the right ctrl key is pressed. *)
  LEFT_CTRL_PRESSED  = 16_0008; (* the left ctrl key is pressed. *)
  SHIFT_PRESSED      = 16_0010; (* the shift key is pressed. *)
  NUMLOCK_ON         = 16_0020; (* the numlock light is on. *)
  SCROLLLOCK_ON      = 16_0040; (* the scrolllock light is on. *)
  CAPSLOCK_ON        = 16_0080; (* the capslock light is on. *)
  ENHANCED_KEY       = 16_0100; (* the key is enhanced. *)

TYPE
  PMOUSE_EVENT_RECORD = UNTRACED REF MOUSE_EVENT_RECORD;
  MOUSE_EVENT_RECORD = RECORD
    dwMousePosition  : COORD;
    dwButtonState    : UINT32;
    dwControlKeyState: UINT32;
    dwEventFlags     : UINT32;
  END;

(* ButtonState flags *)

CONST
  FROM_LEFT_1ST_BUTTON_PRESSED = 16_0001;
  RIGHTMOST_BUTTON_PRESSED     = 16_0002;
  FROM_LEFT_2ND_BUTTON_PRESSED = 16_0004;
  FROM_LEFT_3RD_BUTTON_PRESSED = 16_0008;
  FROM_LEFT_4TH_BUTTON_PRESSED = 16_0010;

(* EventFlags *)

CONST
  MOUSE_MOVED  = 16_0001;
  DOUBLE_CLICK = 16_0002;

TYPE
  WINDOW_BUFFER_SIZE_RECORD = RECORD dwSize: COORD;  END;
  PWINDOW_BUFFER_SIZE_RECORD = UNTRACED REF WINDOW_BUFFER_SIZE_RECORD;

  MENU_EVENT_RECORD = RECORD dwCommandId: UINT32;  END;
  PMENU_EVENT_RECORD = UNTRACED REF MENU_EVENT_RECORD;

  FOCUS_EVENT_RECORD = RECORD bSetFocus: BOOL;  END;
  PFOCUS_EVENT_RECORD = UNTRACED REF FOCUS_EVENT_RECORD;

  PINPUT_RECORD = UNTRACED REF INPUT_RECORD;
  INPUT_RECORD = RECORD
    EventType: UINT16;
    Event    : KEY_EVENT_RECORD;  (* !!! *)
                   (*
                   union {
                       KEY_EVENT_RECORD KeyEvent;
                       MOUSE_EVENT_RECORD MouseEvent;
                       WINDOW_BUFFER_SIZE_RECORD WindowBufferSizeEvent;
                       MENU_EVENT_RECORD MenuEvent;
                       FOCUS_EVENT_RECORD FocusEvent;
                   } Event;
                   *)
  END;

(* EventType flags: *)

CONST
  KEY_EVENT   = 16_0001;        (* Event contains key event record *)
  MOUSE_EVENT = 16_0002;        (* Event contains mouse event record *)
  WINDOW_BUFFER_SIZE_EVENT = 16_0004; (* Event contains window change event
                                         record *)
  MENU_EVENT  = 16_0008;        (* Event contains menu event record *)
  FOCUS_EVENT = 16_0010;        (* event contains focus change *)

TYPE
  PCHAR_INFO = UNTRACED REF CHAR_INFO;
  CHAR_INFO = RECORD
    Char: WCHAR; (* ??? *)
                (* union { WCHAR UnicodeChar; CHAR AsciiChar; } Char; *)
    Attributes: UINT16;
  END;

(* Attributes flags: *)

CONST
  FOREGROUND_BLUE      = 16_0001; (* text color contains blue. *)
  FOREGROUND_GREEN     = 16_0002; (* text color contains green. *)
  FOREGROUND_RED       = 16_0004; (* text color contains red. *)
  FOREGROUND_INTENSITY = 16_0008; (* text color is intensified. *)
  BACKGROUND_BLUE      = 16_0010; (* background color contains blue. *)
  BACKGROUND_GREEN     = 16_0020; (* background color contains green. *)
  BACKGROUND_RED       = 16_0040; (* background color contains red. *)
  BACKGROUND_INTENSITY = 16_0080; (* background color is intensified. *)

TYPE
  PCONSOLE_SCREEN_BUFFER_INFO = UNTRACED REF CONSOLE_SCREEN_BUFFER_INFO;
  CONSOLE_SCREEN_BUFFER_INFO = RECORD
    dwSize             : COORD;
    dwCursorPosition   : COORD;
    wAttributes        : UINT16;
    srWindow           : SMALL_RECT;
    dwMaximumWindowSize: COORD;
  END;

  PCONSOLE_CURSOR_INFO = UNTRACED REF CONSOLE_CURSOR_INFO;
  CONSOLE_CURSOR_INFO = RECORD
    dwSize  : UINT32;
    bVisible: BOOL;
  END;

(* typedef for ctrl-c handler routines *)

TYPE PHANDLER_ROUTINE = <*WINAPI*> PROCEDURE (CtrlType: UINT32): BOOL;

CONST
  CTRL_C_EVENT     = 0;
  CTRL_BREAK_EVENT = 1;
  CTRL_CLOSE_EVENT = 2;
  (* 3 is reserved! *)
  (* 4 is reserved! *)
  CTRL_LOGOFF_EVENT   = 5;
  CTRL_SHUTDOWN_EVENT = 6;

(* Input Mode flags: *)

CONST
  ENABLE_PROCESSED_INPUT = 16_0001;
  ENABLE_LINE_INPUT      = 16_0002;
  ENABLE_ECHO_INPUT      = 16_0004;
  ENABLE_WINDOW_INPUT    = 16_0008;
  ENABLE_MOUSE_INPUT     = 16_0010;

(* Output Mode flags: *)

CONST
  ENABLE_PROCESSED_OUTPUT   = 16_0001;
  ENABLE_WRAP_AT_EOL_OUTPUT = 16_0002;

(* direct API definitions. *)

<*EXTERNAL PeekConsoleInputA:WINAPI*>
PROCEDURE PeekConsoleInputA (hConsoleInput       : HANDLE;
                             lpBuffer            : PINPUT_RECORD;
                             nLength             : UINT32;
                             lpNumberOfEventsRead: PUINT32        ): BOOL;

<*EXTERNAL PeekConsoleInputW:WINAPI*>
PROCEDURE PeekConsoleInputW (hConsoleInput       : HANDLE;
                             lpBuffer            : PINPUT_RECORD;
                             nLength             : UINT32;
                             lpNumberOfEventsRead: PUINT32        ): BOOL;

CONST PeekConsoleInput = PeekConsoleInputA;

<*EXTERNAL ReadConsoleInputA:WINAPI*>
PROCEDURE ReadConsoleInputA (hConsoleInput       : HANDLE;
                             lpBuffer            : PINPUT_RECORD;
                             nLength             : UINT32;
                             lpNumberOfEventsRead: PUINT32        ): BOOL;

<*EXTERNAL ReadConsoleInputW:WINAPI*>
PROCEDURE ReadConsoleInputW (hConsoleInput       : HANDLE;
                             lpBuffer            : PINPUT_RECORD;
                             nLength             : UINT32;
                             lpNumberOfEventsRead: PUINT32       ): BOOL;

CONST ReadConsoleInput = ReadConsoleInputA;

<*EXTERNAL WriteConsoleInputA:WINAPI*>
PROCEDURE WriteConsoleInputA (hConsoleInput          : HANDLE;
                              lpBuffer               : PINPUT_RECORD;
                              nLength                : UINT32;
                              lpNumberOfEventsWritten: PUINT32        ): BOOL;

<*EXTERNAL WriteConsoleInputW:WINAPI*>
PROCEDURE WriteConsoleInputW (hConsoleInput          : HANDLE;
                              lpBuffer               : PINPUT_RECORD;
                              nLength                : UINT32;
                              lpNumberOfEventsWritten: PUINT32        ): BOOL;

CONST WriteConsoleInput = WriteConsoleInputA;

<*EXTERNAL ReadConsoleOutputA:WINAPI*>
PROCEDURE ReadConsoleOutputA (hConsoleOutput: HANDLE;
                              lpBuffer      : PCHAR_INFO;
                              dwBufferSize  : COORD;
                              dwBufferCoord : COORD;
                              lpReadRegion  : PSMALL_RECT ): BOOL;

<*EXTERNAL ReadConsoleOutputW:WINAPI*>
PROCEDURE ReadConsoleOutputW (hConsoleOutput: HANDLE;
                              lpBuffer      : PCHAR_INFO;
                              dwBufferSize  : COORD;
                              dwBufferCoord : COORD;
                              lpReadRegion  : PSMALL_RECT ): BOOL;

CONST ReadConsoleOutput = ReadConsoleOutputA;

<*EXTERNAL WriteConsoleOutputA:WINAPI*>
PROCEDURE WriteConsoleOutputA (hConsoleOutput: HANDLE;
                               lpBuffer      : PCHAR_INFO;
                               dwBufferSize  : COORD;
                               dwBufferCoord : COORD;
                               lpWriteRegion : PSMALL_RECT ): BOOL;

<*EXTERNAL WriteConsoleOutputW:WINAPI*>
PROCEDURE WriteConsoleOutputW (hConsoleOutput: HANDLE;
                               lpBuffer      : PCHAR_INFO;
                               dwBufferSize  : COORD;
                               dwBufferCoord : COORD;
                               lpWriteRegion : PSMALL_RECT ): BOOL;

CONST WriteConsoleOutput = WriteConsoleOutputA;

<*EXTERNAL ReadConsoleOutputCharacterA:WINAPI*>
PROCEDURE ReadConsoleOutputCharacterA (hConsoleOutput     : HANDLE;
                                       lpCharacter        : PSTR;
                                       nLength            : UINT32;
                                       dwReadCoord        : COORD;
                                       lpNumberOfCharsRead: PUINT32 ): BOOL;

<*EXTERNAL ReadConsoleOutputCharacterW:WINAPI*>
PROCEDURE ReadConsoleOutputCharacterW (hConsoleOutput     : HANDLE;
                                       lpCharacter        : PWSTR;
                                       nLength            : UINT32;
                                       dwReadCoord        : COORD;
                                       lpNumberOfCharsRead: PUINT32 ): BOOL;

CONST ReadConsoleOutputCharacter = ReadConsoleOutputCharacterA;

<*EXTERNAL ReadConsoleOutputAttribute:WINAPI*>
PROCEDURE ReadConsoleOutputAttribute (hConsoleOutput     : HANDLE;
                                      lpAttribute        : PUINT16;
                                      nLength            : UINT32;
                                      dwReadCoord        : COORD;
                                      lpNumberOfAttrsRead: PUINT32 ): BOOL;

<*EXTERNAL WriteConsoleOutputCharacterA:WINAPI*>
PROCEDURE WriteConsoleOutputCharacterA (hConsoleOutput        : HANDLE;
                                        lpCharacter           : PSTR;
                                        nLength               : UINT32;
                                        dwWriteCoord          : COORD;
                                        lpNumberOfCharsWritten: PUINT32): BOOL;

<*EXTERNAL WriteConsoleOutputCharacterW:WINAPI*>
PROCEDURE WriteConsoleOutputCharacterW (hConsoleOutput        : HANDLE;
                                        lpCharacter           : PWSTR;
                                        nLength               : UINT32;
                                        dwWriteCoord          : COORD;
                                        lpNumberOfCharsWritten: PUINT32): BOOL;

CONST WriteConsoleOutputCharacter = WriteConsoleOutputCharacterA;

<*EXTERNAL WriteConsoleOutputAttribute:WINAPI*>
PROCEDURE WriteConsoleOutputAttribute (hConsoleOutput        : HANDLE;
                                       lpAttribute           : PUINT16;
                                       nLength               : UINT32;
                                       dwWriteCoord          : COORD;
                                       lpNumberOfAttrsWritten: PUINT32): BOOL;

<*EXTERNAL FillConsoleOutputCharacterA:WINAPI*>
PROCEDURE FillConsoleOutputCharacterA (hConsoleOutput        : HANDLE;
                                       cCharacter            : CHAR;
                                       nLength               : UINT32;
                                       dwWriteCoord          : COORD;
                                       lpNumberOfCharsWritten: PUINT32 ): BOOL;

<*EXTERNAL FillConsoleOutputCharacterW:WINAPI*>
PROCEDURE FillConsoleOutputCharacterW (hConsoleOutput        : HANDLE;
                                       cCharacter            : WCHAR;
                                       nLength               : UINT32;
                                       dwWriteCoord          : COORD;
                                       lpNumberOfCharsWritten: PUINT32 ): BOOL;

CONST FillConsoleOutputCharacter = FillConsoleOutputCharacterA;

<*EXTERNAL FillConsoleOutputAttribute:WINAPI*>
PROCEDURE FillConsoleOutputAttribute (hConsoleOutput        : HANDLE;
                                      wAttribute            : UINT16;
                                      nLength               : UINT32;
                                      dwWriteCoord          : COORD;
                                      lpNumberOfAttrsWritten: PUINT32 ): BOOL;

<*EXTERNAL GetConsoleMode:WINAPI*>
PROCEDURE GetConsoleMode (hConsoleHandle: HANDLE; lpMode: PUINT32): BOOL;

<*EXTERNAL GetNumberOfConsoleInputEvents:WINAPI*>
PROCEDURE GetNumberOfConsoleInputEvents (hConsoleInput   : HANDLE;
                                           lpNumberOfEvents: PUINT32 ): BOOL;

<*EXTERNAL GetConsoleScreenBufferInfo:WINAPI*>
PROCEDURE GetConsoleScreenBufferInfo (
              hConsoleOutput: HANDLE;
              lpConsoleScreenBufferInfo: PCONSOLE_SCREEN_BUFFER_INFO): BOOL;

<*EXTERNAL GetLargestConsoleWindowSize:WINAPI*>
PROCEDURE GetLargestConsoleWindowSize (hConsoleOutput: HANDLE): COORD;

<*EXTERNAL GetConsoleCursorInfo:WINAPI*>
PROCEDURE GetConsoleCursorInfo (
                hConsoleOutput     : HANDLE;
                lpConsoleCursorInfo: PCONSOLE_CURSOR_INFO): BOOL;

<*EXTERNAL GetNumberOfConsoleMouseButtons:WINAPI*>
PROCEDURE GetNumberOfConsoleMouseButtons (
              lpNumberOfMouseButtons: PUINT32): BOOL;

<*EXTERNAL SetConsoleMode:WINAPI*>
PROCEDURE SetConsoleMode (hConsoleHandle: HANDLE; dwMode: UINT32): BOOL;

<*EXTERNAL SetConsoleActiveScreenBuffer:WINAPI*>
PROCEDURE SetConsoleActiveScreenBuffer (hConsoleOutput: HANDLE): BOOL;

<*EXTERNAL FlushConsoleInputBuffer:WINAPI*>
PROCEDURE FlushConsoleInputBuffer (hConsoleInput: HANDLE): BOOL;

<*EXTERNAL SetConsoleScreenBufferSize:WINAPI*>
PROCEDURE SetConsoleScreenBufferSize (hConsoleOutput: HANDLE;
                                      dwSize        : COORD   ): BOOL;

<*EXTERNAL SetConsoleCursorPosition:WINAPI*>
PROCEDURE SetConsoleCursorPosition (hConsoleOutput  : HANDLE;
                                    dwCursorPosition: COORD   ): BOOL;

<*EXTERNAL SetConsoleCursorInfo:WINAPI*>
PROCEDURE SetConsoleCursorInfo (
              hConsoleOutput     : HANDLE;
              lpConsoleCursorInfo: PCONSOLE_CURSOR_INFO): BOOL;

<*EXTERNAL ScrollConsoleScreenBufferA:WINAPI*>
PROCEDURE ScrollConsoleScreenBufferA (hConsoleOutput     : HANDLE;
                                      lpScrollRectangle  : PSMALL_RECT;
                                      lpClipRectangle    : PSMALL_RECT;
                                      dwDestinationOrigin: COORD;
                                      lpFill             : PCHAR_INFO ): BOOL;

<*EXTERNAL ScrollConsoleScreenBufferW:WINAPI*>
PROCEDURE ScrollConsoleScreenBufferW (hConsoleOutput     : HANDLE;
                                      lpScrollRectangle  : PSMALL_RECT;
                                      lpClipRectangle    : PSMALL_RECT;
                                      dwDestinationOrigin: COORD;
                                      lpFill             : PCHAR_INFO ): BOOL;

CONST ScrollConsoleScreenBuffer = ScrollConsoleScreenBufferA;

<*EXTERNAL SetConsoleWindowInfo:WINAPI*>
PROCEDURE SetConsoleWindowInfo (hConsoleOutput : HANDLE;
                                bAbsolute      : BOOL;
                                lpConsoleWindow: PSMALL_RECT): BOOL;

<*EXTERNAL SetConsoleTextAttribute:WINAPI*>
PROCEDURE SetConsoleTextAttribute (hConsoleOutput: HANDLE;
                                   wAttributes   : UINT16    ): BOOL;

<*EXTERNAL SetConsoleCtrlHandler:WINAPI*>
PROCEDURE SetConsoleCtrlHandler (HandlerRoutine: PHANDLER_ROUTINE;
                                 Add           : BOOL           ): BOOL;

<*EXTERNAL GenerateConsoleCtrlEvent:WINAPI*>
PROCEDURE GenerateConsoleCtrlEvent (dwCtrlEvent     : UINT32;
                                    dwProcessGroupId: UINT32  ): BOOL;

<*EXTERNAL AllocConsole:WINAPI*>
PROCEDURE AllocConsole (): BOOL;

<*EXTERNAL FreeConsole:WINAPI*>
PROCEDURE FreeConsole (): BOOL;


<*EXTERNAL GetConsoleTitleA:WINAPI*>
PROCEDURE GetConsoleTitleA (lpConsoleTitle: PSTR; nSize: UINT32): UINT32;

<*EXTERNAL GetConsoleTitleW:WINAPI*>
PROCEDURE GetConsoleTitleW (lpConsoleTitle: PWSTR; nSize: UINT32): UINT32;

CONST GetConsoleTitle = GetConsoleTitleA;

<*EXTERNAL SetConsoleTitleA:WINAPI*>
PROCEDURE SetConsoleTitleA (lpConsoleTitle: PSTR): BOOL;

<*EXTERNAL SetConsoleTitleW:WINAPI*>
PROCEDURE SetConsoleTitleW (lpConsoleTitle: PWSTR): BOOL;

CONST SetConsoleTitle = SetConsoleTitleA;

<*EXTERNAL ReadConsoleA:WINAPI*>
PROCEDURE ReadConsoleA (hConsoleInput       : HANDLE;
                        lpBuffer            : PVOID;
                        nNumberOfCharsToRead: UINT32;
                        lpNumberOfCharsRead : PUINT32;
                        lpReserved          : PVOID   ): BOOL;

<*EXTERNAL ReadConsoleW:WINAPI*>
PROCEDURE ReadConsoleW (hConsoleInput       : HANDLE;
                        lpBuffer            : PVOID;
                        nNumberOfCharsToRead: UINT32;
                        lpNumberOfCharsRead : PUINT32;
                        lpReserved          : PVOID   ): BOOL;

CONST ReadConsole = ReadConsoleA;

<*EXTERNAL WriteConsoleA:WINAPI*>
PROCEDURE WriteConsoleA (hConsoleOutput        : HANDLE;
                         lpBuffer              : PVOID;
                         nNumberOfCharsToWrite : UINT32;
                         lpNumberOfCharsWritten: PUINT32;
                         lpReserved            : PVOID           ): BOOL;

<*EXTERNAL WriteConsoleW:WINAPI*>
PROCEDURE WriteConsoleW (hConsoleOutput        : HANDLE;
                         lpBuffer              : PVOID;
                         nNumberOfCharsToWrite : UINT32;
                         lpNumberOfCharsWritten: PUINT32;
                         lpReserved            : PVOID          ): BOOL;

CONST WriteConsole = WriteConsoleA;

CONST CONSOLE_TEXTMODE_BUFFER = 1;

<*EXTERNAL CreateConsoleScreenBuffer:WINAPI*>
PROCEDURE CreateConsoleScreenBuffer (
              dwDesiredAccess     : UINT32;
              dwShareMode         : UINT32;
              lpSecurityAttributes: WinBase.LPSECURITY_ATTRIBUTES;
              dwFlags             : UINT32;
              lpScreenBufferData  : PVOID  ): HANDLE;

<*EXTERNAL GetConsoleCP:WINAPI*>
PROCEDURE GetConsoleCP (): UINT32;

<*EXTERNAL SetConsoleCP:WINAPI*>
PROCEDURE SetConsoleCP (wCodePageID: UINT32): BOOL;


<*EXTERNAL GetConsoleOutputCP:WINAPI*>
PROCEDURE GetConsoleOutputCP (): UINT32;

<*EXTERNAL SetConsoleOutputCP:WINAPI*>
PROCEDURE SetConsoleOutputCP (wCodePageID: UINT32): BOOL;

END WinCon.
