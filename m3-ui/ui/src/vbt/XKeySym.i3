INTERFACE XKeySym;
(* Derived from: $XConsortium: keysymdef.h,v 1.13 89/12/12 16:23:30 rws Exp $ 
   by steveg Thu Mar 22 14:25:56 PST 1990 *)
(***********************************************************
Copyright 1987 by Digital Equipment Corporation, Maynard, Massachusetts,
and the Massachusetts Institute of Technology, Cambridge, Massachusetts.
                        All Rights Reserved
Permission to use, copy, modify, and distribute this software and its
documentation for any purpose and without fee is hereby granted,
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in
supporting documentation, and that the names of Digital or MIT not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.
DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.
******************************************************************)
<*PRAGMA LL*>

CONST
  VoidSymbol = 16_FFFFFF;	(* void symbol *)

(* MISCELLANY *)
(*
 * TTY Functions, cleverly chosen to map to ascii, for convenience of
 * programming, but could have been arbitrary (at the cost of lookup
 * tables in client code.
 *)

  BackSpace = 16_FF08;	(* back space, back char *)
  Tab = 16_FF09;
  Linefeed = 16_FF0A;	(* Linefeed, LF *)
  Clear = 16_FF0B;
  Return = 16_FF0D;	(* Return, enter *)
  Pause = 16_FF13;	(* Pause, hold *)
  Scroll_Lock = 16_FF14;
  Escape = 16_FF1B;
  Delete = 16_FFFF;	(* Delete, rubout *)



(* International & multi-key character composition *)

  Multi_key = 16_FF20;  (* Multi-key character compose *)

(* Japanese keyboard support *)

  Kanji = 16_FF21;	(* Kanji, Kanji convert *)
  Muhenkan = 16_FF22;  (* Cancel Conversion *)
  Henkan_Mode = 16_FF23;  (* Start/Stop Conversion *)
  Henkan = 16_FF23;  (* Alias for Henkan_Mode *)
  Romaji = 16_FF24;  (* to Romaji *)
  Hiragana = 16_FF25;  (* to Hiragana *)
  Katakana = 16_FF26;  (* to Katakana *)
  Hiragana_Katakana = 16_FF27;  (* Hiragana/Katakana toggle *)
  Zenkaku = 16_FF28;  (* to Zenkaku *)
  Hankaku = 16_FF29;  (* to Hankaku *)
  Zenkaku_Hankaku = 16_FF2A;  (* Zenkaku/Hankaku toggle *)
  Touroku = 16_FF2B;  (* Add to Dictionary *)
  Massyo = 16_FF2C;  (* Delete from Dictionary *)
  Kana_Lock = 16_FF2D;  (* Kana Lock *)
  Kana_Shift = 16_FF2E;  (* Kana Shift *)
  Eisu_Shift = 16_FF2F;  (* Alphanumeric Shift *)
  Eisu_toggle = 16_FF30;  (* Alphanumeric toggle *)

(* Cursor control & motion *)

  Home = 16_FF50;
  Left = 16_FF51;	(* Move left, left arrow *)
  Up = 16_FF52;	(* Move up, up arrow *)
  Right = 16_FF53;	(* Move right, right arrow *)
  Down = 16_FF54;	(* Move down, down arrow *)
  Prior = 16_FF55;	(* Prior, previous *)
  Next = 16_FF56;	(* Next *)
  End = 16_FF57;	(* EOL *)
  Begin = 16_FF58;	(* BOL *)


(* Misc Functions *)

  Select = 16_FF60;	(* Select, mark *)
  Print = 16_FF61;
  Execute = 16_FF62;	(* Execute, run, do *)
  Insert = 16_FF63;	(* Insert, insert here *)
  Undo = 16_FF65;	(* Undo, oops *)
  Redo = 16_FF66;	(* redo, again *)
  Menu = 16_FF67;
  Find = 16_FF68;	(* Find, search *)
  Cancel = 16_FF69;	(* Cancel, stop, abort, exit *)
  Help = 16_FF6A;	(* Help, ? *)
  Break = 16_FF6B;
  Mode_switch = 16_FF7E;	(* Character set switch *)
  script_switch = 16_FF7E;  (* Alias for mode_switch *)
  Num_Lock = 16_FF7F;

(* Keypad Functions, keypad numbers cleverly chosen to map to ascii *)

  KP_Space = 16_FF80;	(* space *)
  KP_Tab = 16_FF89;
  KP_Enter = 16_FF8D;	(* enter *)
  KP_F1 = 16_FF91;	(* PF1, KP_A, ... *)
  KP_F2 = 16_FF92;
  KP_F3 = 16_FF93;
  KP_F4 = 16_FF94;
  KP_Equal = 16_FFBD;	(* equals *)
  KP_Multiply = 16_FFAA;
  KP_Add = 16_FFAB;
  KP_Separator = 16_FFAC;	(* separator, often comma *)
  KP_Subtract = 16_FFAD;
  KP_Decimal = 16_FFAE;
  KP_Divide = 16_FFAF;

  KP_0 = 16_FFB0;
  KP_1 = 16_FFB1;
  KP_2 = 16_FFB2;
  KP_3 = 16_FFB3;
  KP_4 = 16_FFB4;
  KP_5 = 16_FFB5;
  KP_6 = 16_FFB6;
  KP_7 = 16_FFB7;
  KP_8 = 16_FFB8;
  KP_9 = 16_FFB9;



(*
 * Auxilliary Functions; note the duplicate definitions for left and right
 * function keys;  Sun keyboards and a few other manufactures have such
 * function key groups on the left and/or right sides of the keyboard.
 * We've not found a keyboard with more than 35 function keys total.
 *)

  F1 = 16_FFBE;
  F2 = 16_FFBF;
  F3 = 16_FFC0;
  F4 = 16_FFC1;
  F5 = 16_FFC2;
  F6 = 16_FFC3;
  F7 = 16_FFC4;
  F8 = 16_FFC5;
  F9 = 16_FFC6;
  F10 = 16_FFC7;
  F11 = 16_FFC8;
  L1 = 16_FFC8;
  F12 = 16_FFC9;
  L2 = 16_FFC9;
  F13 = 16_FFCA;
  L3 = 16_FFCA;
  F14 = 16_FFCB;
  L4 = 16_FFCB;
  F15 = 16_FFCC;
  L5 = 16_FFCC;
  F16 = 16_FFCD;
  L6 = 16_FFCD;
  F17 = 16_FFCE;
  L7 = 16_FFCE;
  F18 = 16_FFCF;
  L8 = 16_FFCF;
  F19 = 16_FFD0;
  L9 = 16_FFD0;
  F20 = 16_FFD1;
  L10 = 16_FFD1;
  F21 = 16_FFD2;
  R1 = 16_FFD2;
  F22 = 16_FFD3;
  R2 = 16_FFD3;
  F23 = 16_FFD4;
  R3 = 16_FFD4;
  F24 = 16_FFD5;
  R4 = 16_FFD5;
  F25 = 16_FFD6;
  R5 = 16_FFD6;
  F26 = 16_FFD7;
  R6 = 16_FFD7;
  F27 = 16_FFD8;
  R7 = 16_FFD8;
  F28 = 16_FFD9;
  R8 = 16_FFD9;
  F29 = 16_FFDA;
  R9 = 16_FFDA;
  F30 = 16_FFDB;
  R10 = 16_FFDB;
  F31 = 16_FFDC;
  R11 = 16_FFDC;
  F32 = 16_FFDD;
  R12 = 16_FFDD;
  R13 = 16_FFDE;
  F33 = 16_FFDE;
  F34 = 16_FFDF;
  R14 = 16_FFDF;
  F35 = 16_FFE0;
  R15 = 16_FFE0;

(* Modifiers *)

  Shift_L = 16_FFE1;	(* Left shift *)
  Shift_R = 16_FFE2;	(* Right shift *)
  Control_L = 16_FFE3;	(* Left control *)
  Control_R = 16_FFE4;	(* Right control *)
  Caps_Lock = 16_FFE5;	(* Caps lock *)
  Shift_Lock = 16_FFE6;	(* Shift lock *)

  Meta_L = 16_FFE7;	(* Left meta *)
  Meta_R = 16_FFE8;	(* Right meta *)
  Alt_L = 16_FFE9;	(* Left alt *)
  Alt_R = 16_FFEA;	(* Right alt *)
  Super_L = 16_FFEB;	(* Left super *)
  Super_R = 16_FFEC;	(* Right super *)
  Hyper_L = 16_FFED;	(* Left hyper *)
  Hyper_R = 16_FFEE;	(* Right hyper *)
(* end of  MISCELLANY *)

(*
 *  Latin 1
 *  Byte 3 = 0;
 *)
(* LATIN1 *)
  space = 16_020;
  exclam = 16_021;
  quotedbl = 16_022;
  numbersign = 16_023;
  dollar = 16_024;
  percent = 16_025;
  ampersand = 16_026;
  apostrophe = 16_027;
  quoteright = 16_027;	(* deprecated *)
  parenleft = 16_028;
  parenright = 16_029;
  asterisk = 16_02a;
  plus = 16_02b;
  comma = 16_02c;
  minus = 16_02d;
  period = 16_02e;
  slash = 16_02f;
  zero = 16_030;
  one = 16_031;
  two = 16_032;
  three = 16_033;
  four = 16_034;
  five = 16_035;
  six = 16_036;
  seven = 16_037;
  eight = 16_038;
  nine = 16_039;
  colon = 16_03a;
  semicolon = 16_03b;
  less = 16_03c;
  equal = 16_03d;
  greater = 16_03e;
  question = 16_03f;
  at = 16_040;
  A = 16_041;
  B = 16_042;
  C = 16_043;
  D = 16_044;
  E = 16_045;
  F = 16_046;
  G = 16_047;
  H = 16_048;
  I = 16_049;
  J = 16_04a;
  K = 16_04b;
  L = 16_04c;
  M = 16_04d;
  N = 16_04e;
  O = 16_04f;
  P = 16_050;
  Q = 16_051;
  R = 16_052;
  S = 16_053;
  T = 16_054;
  U = 16_055;
  V = 16_056;
  W = 16_057;
  X = 16_058;
  Y = 16_059;
  Z = 16_05a;
  bracketleft = 16_05b;
  backslash = 16_05c;
  bracketright = 16_05d;
  asciicircum = 16_05e;
  underscore = 16_05f;
  grave = 16_060;
  quoteleft = 16_060;	(* deprecated *)
  a = 16_061;
  b = 16_062;
  c = 16_063;
  d = 16_064;
  e = 16_065;
  f = 16_066;
  g = 16_067;
  h = 16_068;
  i = 16_069;
  j = 16_06a;
  k = 16_06b;
  l = 16_06c;
  m = 16_06d;
  n = 16_06e;
  o = 16_06f;
  p = 16_070;
  q = 16_071;
  r = 16_072;
  s = 16_073;
  t = 16_074;
  u = 16_075;
  v = 16_076;
  w = 16_077;
  x = 16_078;
  y = 16_079;
  z = 16_07a;
  braceleft = 16_07b;
  bar = 16_07c;
  braceright = 16_07d;
  asciitilde = 16_07e;

  nobreakspace = 16_0a0;
  exclamdown = 16_0a1;
  cent = 16_0a2;
  sterling = 16_0a3;
  currency = 16_0a4;
  yen = 16_0a5;
  brokenbar = 16_0a6;
  section = 16_0a7;
  diaeresis = 16_0a8;
  copyright = 16_0a9;
  ordfeminine = 16_0aa;
  guillemotleft = 16_0ab;	(* left angle quotation mark *)
  notsign = 16_0ac;
  hyphen = 16_0ad;
  registered = 16_0ae;
  macron = 16_0af;
  degree = 16_0b0;
  plusminus = 16_0b1;
  twosuperior = 16_0b2;
  threesuperior = 16_0b3;
  acute = 16_0b4;
  mu = 16_0b5;
  paragraph = 16_0b6;
  periodcentered = 16_0b7;
  cedilla = 16_0b8;
  onesuperior = 16_0b9;
  masculine = 16_0ba;
  guillemotright = 16_0bb;	(* right angle quotation mark *)
  onequarter = 16_0bc;
  onehalf = 16_0bd;
  threequarters = 16_0be;
  questiondown = 16_0bf;
  Agrave = 16_0c0;
  Aacute = 16_0c1;
  Acircumflex = 16_0c2;
  Atilde = 16_0c3;
  Adiaeresis = 16_0c4;
  Aring = 16_0c5;
  AE = 16_0c6;
  Ccedilla = 16_0c7;
  Egrave = 16_0c8;
  Eacute = 16_0c9;
  Ecircumflex = 16_0ca;
  Ediaeresis = 16_0cb;
  Igrave = 16_0cc;
  Iacute = 16_0cd;
  Icircumflex = 16_0ce;
  Idiaeresis = 16_0cf;
  ETH = 16_0d0;
  Eth = 16_0d0;	(* deprecated *)
  Ntilde = 16_0d1;
  Ograve = 16_0d2;
  Oacute = 16_0d3;
  Ocircumflex = 16_0d4;
  Otilde = 16_0d5;
  Odiaeresis = 16_0d6;
  multiply = 16_0d7;
  Ooblique = 16_0d8;
  Ugrave = 16_0d9;
  Uacute = 16_0da;
  Ucircumflex = 16_0db;
  Udiaeresis = 16_0dc;
  Yacute = 16_0dd;
  THORN = 16_0de;
  Thorn = 16_0de;	(* deprecated *)
  ssharp = 16_0df;
  agrave = 16_0e0;
  aacute = 16_0e1;
  acircumflex = 16_0e2;
  atilde = 16_0e3;
  adiaeresis = 16_0e4;
  aring = 16_0e5;
  ae = 16_0e6;
  ccedilla = 16_0e7;
  egrave = 16_0e8;
  eacute = 16_0e9;
  ecircumflex = 16_0ea;
  ediaeresis = 16_0eb;
  igrave = 16_0ec;
  iacute = 16_0ed;
  icircumflex = 16_0ee;
  idiaeresis = 16_0ef;
  eth = 16_0f0;
  ntilde = 16_0f1;
  ograve = 16_0f2;
  oacute = 16_0f3;
  ocircumflex = 16_0f4;
  otilde = 16_0f5;
  odiaeresis = 16_0f6;
  division = 16_0f7;
  oslash = 16_0f8;
  ugrave = 16_0f9;
  uacute = 16_0fa;
  ucircumflex = 16_0fb;
  udiaeresis = 16_0fc;
  yacute = 16_0fd;
  thorn = 16_0fe;
  ydiaeresis = 16_0ff;
(* end of  LATIN1 *)

(*
 *   Latin 2
 *   Byte 3 = 1;
 *)

(* LATIN2 *)
  Aogonek = 16_1a1;
  breve = 16_1a2;
  Lstroke = 16_1a3;
  Lcaron = 16_1a5;
  Sacute = 16_1a6;
  Scaron = 16_1a9;
  Scedilla = 16_1aa;
  Tcaron = 16_1ab;
  Zacute = 16_1ac;
  Zcaron = 16_1ae;
  Zabovedot = 16_1af;
  aogonek = 16_1b1;
  ogonek = 16_1b2;
  lstroke = 16_1b3;
  lcaron = 16_1b5;
  sacute = 16_1b6;
  caron = 16_1b7;
  scaron = 16_1b9;
  scedilla = 16_1ba;
  tcaron = 16_1bb;
  zacute = 16_1bc;
  doubleacute = 16_1bd;
  zcaron = 16_1be;
  zabovedot = 16_1bf;
  Racute = 16_1c0;
  Abreve = 16_1c3;
  Lacute = 16_1c5;
  Cacute = 16_1c6;
  Ccaron = 16_1c8;
  Eogonek = 16_1ca;
  Ecaron = 16_1cc;
  Dcaron = 16_1cf;
  Dstroke = 16_1d0;
  Nacute = 16_1d1;
  Ncaron = 16_1d2;
  Odoubleacute = 16_1d5;
  Rcaron = 16_1d8;
  Uring = 16_1d9;
  Udoubleacute = 16_1db;
  Tcedilla = 16_1de;
  racute = 16_1e0;
  abreve = 16_1e3;
  lacute = 16_1e5;
  cacute = 16_1e6;
  ccaron = 16_1e8;
  eogonek = 16_1ea;
  ecaron = 16_1ec;
  dcaron = 16_1ef;
  dstroke = 16_1f0;
  nacute = 16_1f1;
  ncaron = 16_1f2;
  odoubleacute = 16_1f5;
  udoubleacute = 16_1fb;
  rcaron = 16_1f8;
  uring = 16_1f9;
  tcedilla = 16_1fe;
  abovedot = 16_1ff;
(* end of  LATIN2 *)

(*
 *   Latin 3
 *   Byte 3 = 2;
 *)

(* LATIN3 *)
  Hstroke = 16_2a1;
  Hcircumflex = 16_2a6;
  Iabovedot = 16_2a9;
  Gbreve = 16_2ab;
  Jcircumflex = 16_2ac;
  hstroke = 16_2b1;
  hcircumflex = 16_2b6;
  idotless = 16_2b9;
  gbreve = 16_2bb;
  jcircumflex = 16_2bc;
  Cabovedot = 16_2c5;
  Ccircumflex = 16_2c6;
  Gabovedot = 16_2d5;
  Gcircumflex = 16_2d8;
  Ubreve = 16_2dd;
  Scircumflex = 16_2de;
  cabovedot = 16_2e5;
  ccircumflex = 16_2e6;
  gabovedot = 16_2f5;
  gcircumflex = 16_2f8;
  ubreve = 16_2fd;
  scircumflex = 16_2fe;
(* end of  LATIN3 *)


(*
 *   Latin 4
 *   Byte 3 = 3;
 *)

(* LATIN4 *)
  kra = 16_3a2;
  kappa = 16_3a2;	(* deprecated *)
  Rcedilla = 16_3a3;
  Itilde = 16_3a5;
  Lcedilla = 16_3a6;
  Emacron = 16_3aa;
  Gcedilla = 16_3ab;
  Tslash = 16_3ac;
  rcedilla = 16_3b3;
  itilde = 16_3b5;
  lcedilla = 16_3b6;
  emacron = 16_3ba;
  gcedilla = 16_3bb;
  tslash = 16_3bc;
  ENG = 16_3bd;
  eng = 16_3bf;
  Amacron = 16_3c0;
  Iogonek = 16_3c7;
  Eabovedot = 16_3cc;
  Imacron = 16_3cf;
  Ncedilla = 16_3d1;
  Omacron = 16_3d2;
  Kcedilla = 16_3d3;
  Uogonek = 16_3d9;
  Utilde = 16_3dd;
  Umacron = 16_3de;
  amacron = 16_3e0;
  iogonek = 16_3e7;
  eabovedot = 16_3ec;
  imacron = 16_3ef;
  ncedilla = 16_3f1;
  omacron = 16_3f2;
  kcedilla = 16_3f3;
  uogonek = 16_3f9;
  utilde = 16_3fd;
  umacron = 16_3fe;
(* end of  LATIN4 *)

(*
 * Katakana
 * Byte 3 = 4
 *)

(* KATAKANA *)
  overline = 16_47e;
  Kana_fullstop = 16_4a1;
  Kana_openingbracket = 16_4a2;
  Kana_closingbracket = 16_4a3;
  Kana_comma = 16_4a4;
  Kana_conjunctive = 16_4a5;
  Kana_middledot = 16_4a5;  (* deprecated *)
  Kana_WO = 16_4a6;
  Kana_a = 16_4a7;
  Kana_i = 16_4a8;
  Kana_u = 16_4a9;
  Kana_e = 16_4aa;
  Kana_o = 16_4ab;
  Kana_ya = 16_4ac;
  Kana_yu = 16_4ad;
  Kana_yo = 16_4ae;
  Kana_tsu = 16_4af;
  Kana_tu = 16_4af;  (* deprecated *)
  prolongedsound = 16_4b0;
  Kana_A = 16_4b1;
  Kana_I = 16_4b2;
  Kana_U = 16_4b3;
  Kana_E = 16_4b4;
  Kana_O = 16_4b5;
  Kana_KA = 16_4b6;
  Kana_KI = 16_4b7;
  Kana_KU = 16_4b8;
  Kana_KE = 16_4b9;
  Kana_KO = 16_4ba;
  Kana_SA = 16_4bb;
  Kana_SHI = 16_4bc;
  Kana_SU = 16_4bd;
  Kana_SE = 16_4be;
  Kana_SO = 16_4bf;
  Kana_TA = 16_4c0;
  Kana_CHI = 16_4c1;
  Kana_TI = 16_4c1;  (* deprecated *)
  Kana_TSU = 16_4c2;
  Kana_TU = 16_4c2;  (* deprecated *)
  Kana_TE = 16_4c3;
  Kana_TO = 16_4c4;
  Kana_NA = 16_4c5;
  Kana_NI = 16_4c6;
  Kana_NU = 16_4c7;
  Kana_NE = 16_4c8;
  Kana_NO = 16_4c9;
  Kana_HA = 16_4ca;
  Kana_HI = 16_4cb;
  Kana_FU = 16_4cc;
  Kana_HU = 16_4cc;  (* deprecated *)
  Kana_HE = 16_4cd;
  Kana_HO = 16_4ce;
  Kana_MA = 16_4cf;
  Kana_MI = 16_4d0;
  Kana_MU = 16_4d1;
  Kana_ME = 16_4d2;
  Kana_MO = 16_4d3;
  Kana_YA = 16_4d4;
  Kana_YU = 16_4d5;
  Kana_YO = 16_4d6;
  Kana_RA = 16_4d7;
  Kana_RI = 16_4d8;
  Kana_RU = 16_4d9;
  Kana_RE = 16_4da;
  Kana_RO = 16_4db;
  Kana_WA = 16_4dc;
  Kana_N = 16_4dd;
  voicedsound = 16_4de;
  semivoicedsound = 16_4df;
  Kana_switch = 16_FF7E;  (* Alias for mode_switch *)
(* end of  KATAKANA *)

(*
 *  Arabic
 *  Byte 3 = 5;
 *)

(* ARABIC *)
  Arabic_comma = 16_5ac;
  Arabic_semicolon = 16_5bb;
  Arabic_question_mark = 16_5bf;
  Arabic_hamza = 16_5c1;
  Arabic_maddaonalef = 16_5c2;
  Arabic_hamzaonalef = 16_5c3;
  Arabic_hamzaonwaw = 16_5c4;
  Arabic_hamzaunderalef = 16_5c5;
  Arabic_hamzaonyeh = 16_5c6;
  Arabic_alef = 16_5c7;
  Arabic_beh = 16_5c8;
  Arabic_tehmarbuta = 16_5c9;
  Arabic_teh = 16_5ca;
  Arabic_theh = 16_5cb;
  Arabic_jeem = 16_5cc;
  Arabic_hah = 16_5cd;
  Arabic_khah = 16_5ce;
  Arabic_dal = 16_5cf;
  Arabic_thal = 16_5d0;
  Arabic_ra = 16_5d1;
  Arabic_zain = 16_5d2;
  Arabic_seen = 16_5d3;
  Arabic_sheen = 16_5d4;
  Arabic_sad = 16_5d5;
  Arabic_dad = 16_5d6;
  Arabic_tah = 16_5d7;
  Arabic_zah = 16_5d8;
  Arabic_ain = 16_5d9;
  Arabic_ghain = 16_5da;
  Arabic_tatweel = 16_5e0;
  Arabic_feh = 16_5e1;
  Arabic_qaf = 16_5e2;
  Arabic_kaf = 16_5e3;
  Arabic_lam = 16_5e4;
  Arabic_meem = 16_5e5;
  Arabic_noon = 16_5e6;
  Arabic_ha = 16_5e7;
  Arabic_heh = 16_5e7;  (* deprecated *)
  Arabic_waw = 16_5e8;
  Arabic_alefmaksura = 16_5e9;
  Arabic_yeh = 16_5ea;
  Arabic_fathatan = 16_5eb;
  Arabic_dammatan = 16_5ec;
  Arabic_kasratan = 16_5ed;
  Arabic_fatha = 16_5ee;
  Arabic_damma = 16_5ef;
  Arabic_kasra = 16_5f0;
  Arabic_shadda = 16_5f1;
  Arabic_sukun = 16_5f2;
  Arabic_switch = 16_FF7E;  (* Alias for mode_switch *)
(* end of  ARABIC *)

(*
 * Cyrillic
 * Byte 3 = 6
 *)
(* CYRILLIC *)
  Serbian_dje = 16_6a1;
  Macedonia_gje = 16_6a2;
  Cyrillic_io = 16_6a3;
  Ukrainian_ie = 16_6a4;
  Ukranian_je = 16_6a4;  (* deprecated *)
  Macedonia_dse = 16_6a5;
  Ukrainian_i = 16_6a6;
  Ukranian_i = 16_6a6;  (* deprecated *)
  Ukrainian_yi = 16_6a7;
  Ukranian_yi = 16_6a7;  (* deprecated *)
  Cyrillic_je = 16_6a8;
  Serbian_je = 16_6a8;  (* deprecated *)
  Cyrillic_lje = 16_6a9;
  Serbian_lje = 16_6a9;  (* deprecated *)
  Cyrillic_nje = 16_6aa;
  Serbian_nje = 16_6aa;  (* deprecated *)
  Serbian_tshe = 16_6ab;
  Macedonia_kje = 16_6ac;
  Byelorussian_shortu = 16_6ae;
  Cyrillic_dzhe = 16_6af;
  Serbian_dze = 16_6af;  (* deprecated *)
  numerosign = 16_6b0;
  Serbian_DJE = 16_6b1;
  Macedonia_GJE = 16_6b2;
  Cyrillic_IO = 16_6b3;
  Ukrainian_IE = 16_6b4;
  Ukranian_JE = 16_6b4;  (* deprecated *)
  Macedonia_DSE = 16_6b5;
  Ukrainian_I = 16_6b6;
  Ukranian_I = 16_6b6;  (* deprecated *)
  Ukrainian_YI = 16_6b7;
  Ukranian_YI = 16_6b7;  (* deprecated *)
  Cyrillic_JE = 16_6b8;
  Serbian_JE = 16_6b8;  (* deprecated *)
  Cyrillic_LJE = 16_6b9;
  Serbian_LJE = 16_6b9;  (* deprecated *)
  Cyrillic_NJE = 16_6ba;
  Serbian_NJE = 16_6ba;  (* deprecated *)
  Serbian_TSHE = 16_6bb;
  Macedonia_KJE = 16_6bc;
  Byelorussian_SHORTU = 16_6be;
  Cyrillic_DZHE = 16_6bf;
  Serbian_DZE = 16_6bf;  (* deprecated *)
  Cyrillic_yu = 16_6c0;
  Cyrillic_a = 16_6c1;
  Cyrillic_be = 16_6c2;
  Cyrillic_tse = 16_6c3;
  Cyrillic_de = 16_6c4;
  Cyrillic_ie = 16_6c5;
  Cyrillic_ef = 16_6c6;
  Cyrillic_ghe = 16_6c7;
  Cyrillic_ha = 16_6c8;
  Cyrillic_i = 16_6c9;
  Cyrillic_shorti = 16_6ca;
  Cyrillic_ka = 16_6cb;
  Cyrillic_el = 16_6cc;
  Cyrillic_em = 16_6cd;
  Cyrillic_en = 16_6ce;
  Cyrillic_o = 16_6cf;
  Cyrillic_pe = 16_6d0;
  Cyrillic_ya = 16_6d1;
  Cyrillic_er = 16_6d2;
  Cyrillic_es = 16_6d3;
  Cyrillic_te = 16_6d4;
  Cyrillic_u = 16_6d5;
  Cyrillic_zhe = 16_6d6;
  Cyrillic_ve = 16_6d7;
  Cyrillic_softsign = 16_6d8;
  Cyrillic_yeru = 16_6d9;
  Cyrillic_ze = 16_6da;
  Cyrillic_sha = 16_6db;
  Cyrillic_e = 16_6dc;
  Cyrillic_shcha = 16_6dd;
  Cyrillic_che = 16_6de;
  Cyrillic_hardsign = 16_6df;
  Cyrillic_YU = 16_6e0;
  Cyrillic_A = 16_6e1;
  Cyrillic_BE = 16_6e2;
  Cyrillic_TSE = 16_6e3;
  Cyrillic_DE = 16_6e4;
  Cyrillic_IE = 16_6e5;
  Cyrillic_EF = 16_6e6;
  Cyrillic_GHE = 16_6e7;
  Cyrillic_HA = 16_6e8;
  Cyrillic_I = 16_6e9;
  Cyrillic_SHORTI = 16_6ea;
  Cyrillic_KA = 16_6eb;
  Cyrillic_EL = 16_6ec;
  Cyrillic_EM = 16_6ed;
  Cyrillic_EN = 16_6ee;
  Cyrillic_O = 16_6ef;
  Cyrillic_PE = 16_6f0;
  Cyrillic_YA = 16_6f1;
  Cyrillic_ER = 16_6f2;
  Cyrillic_ES = 16_6f3;
  Cyrillic_TE = 16_6f4;
  Cyrillic_U = 16_6f5;
  Cyrillic_ZHE = 16_6f6;
  Cyrillic_VE = 16_6f7;
  Cyrillic_SOFTSIGN = 16_6f8;
  Cyrillic_YERU = 16_6f9;
  Cyrillic_ZE = 16_6fa;
  Cyrillic_SHA = 16_6fb;
  Cyrillic_E = 16_6fc;
  Cyrillic_SHCHA = 16_6fd;
  Cyrillic_CHE = 16_6fe;
  Cyrillic_HARDSIGN = 16_6ff;
(* end of  CYRILLIC *)

(*
 * Greek
 * Byte 3 = 7
 *)

(* GREEK *)
  Greek_ALPHAaccent = 16_7a1;
  Greek_EPSILONaccent = 16_7a2;
  Greek_ETAaccent = 16_7a3;
  Greek_IOTAaccent = 16_7a4;
  Greek_IOTAdiaeresis = 16_7a5;
  Greek_OMICRONaccent = 16_7a7;
  Greek_UPSILONaccent = 16_7a8;
  Greek_UPSILONdieresis = 16_7a9;
  Greek_OMEGAaccent = 16_7ab;
  Greek_accentdieresis = 16_7ae;
  Greek_horizbar = 16_7af;
  Greek_alphaaccent = 16_7b1;
  Greek_epsilonaccent = 16_7b2;
  Greek_etaaccent = 16_7b3;
  Greek_iotaaccent = 16_7b4;
  Greek_iotadieresis = 16_7b5;
  Greek_iotaaccentdieresis = 16_7b6;
  Greek_omicronaccent = 16_7b7;
  Greek_upsilonaccent = 16_7b8;
  Greek_upsilondieresis = 16_7b9;
  Greek_upsilonaccentdieresis = 16_7ba;
  Greek_omegaaccent = 16_7bb;
  Greek_ALPHA = 16_7c1;
  Greek_BETA = 16_7c2;
  Greek_GAMMA = 16_7c3;
  Greek_DELTA = 16_7c4;
  Greek_EPSILON = 16_7c5;
  Greek_ZETA = 16_7c6;
  Greek_ETA = 16_7c7;
  Greek_THETA = 16_7c8;
  Greek_IOTA = 16_7c9;
  Greek_KAPPA = 16_7ca;
  Greek_LAMDA = 16_7cb;
  Greek_LAMBDA = 16_7cb;
  Greek_MU = 16_7cc;
  Greek_NU = 16_7cd;
  Greek_XI = 16_7ce;
  Greek_OMICRON = 16_7cf;
  Greek_PI = 16_7d0;
  Greek_RHO = 16_7d1;
  Greek_SIGMA = 16_7d2;
  Greek_TAU = 16_7d4;
  Greek_UPSILON = 16_7d5;
  Greek_PHI = 16_7d6;
  Greek_CHI = 16_7d7;
  Greek_PSI = 16_7d8;
  Greek_OMEGA = 16_7d9;
  Greek_alpha = 16_7e1;
  Greek_beta = 16_7e2;
  Greek_gamma = 16_7e3;
  Greek_delta = 16_7e4;
  Greek_epsilon = 16_7e5;
  Greek_zeta = 16_7e6;
  Greek_eta = 16_7e7;
  Greek_theta = 16_7e8;
  Greek_iota = 16_7e9;
  Greek_kappa = 16_7ea;
  Greek_lamda = 16_7eb;
  Greek_lambda = 16_7eb;
  Greek_mu = 16_7ec;
  Greek_nu = 16_7ed;
  Greek_xi = 16_7ee;
  Greek_omicron = 16_7ef;
  Greek_pi = 16_7f0;
  Greek_rho = 16_7f1;
  Greek_sigma = 16_7f2;
  Greek_finalsmallsigma = 16_7f3;
  Greek_tau = 16_7f4;
  Greek_upsilon = 16_7f5;
  Greek_phi = 16_7f6;
  Greek_chi = 16_7f7;
  Greek_psi = 16_7f8;
  Greek_omega = 16_7f9;
  Greek_switch = 16_FF7E;  (* Alias for mode_switch *)
(* end of  GREEK *)

(*
 * Technical
 * Byte 3 = 8
 *)

(* TECHNICAL *)
  leftradical = 16_8a1;
  topleftradical = 16_8a2;
  horizconnector = 16_8a3;
  topintegral = 16_8a4;
  botintegral = 16_8a5;
  vertconnector = 16_8a6;
  topleftsqbracket = 16_8a7;
  botleftsqbracket = 16_8a8;
  toprightsqbracket = 16_8a9;
  botrightsqbracket = 16_8aa;
  topleftparens = 16_8ab;
  botleftparens = 16_8ac;
  toprightparens = 16_8ad;
  botrightparens = 16_8ae;
  leftmiddlecurlybrace = 16_8af;
  rightmiddlecurlybrace = 16_8b0;
  topleftsummation = 16_8b1;
  botleftsummation = 16_8b2;
  topvertsummationconnector = 16_8b3;
  botvertsummationconnector = 16_8b4;
  toprightsummation = 16_8b5;
  botrightsummation = 16_8b6;
  rightmiddlesummation = 16_8b7;
  lessthanequal = 16_8bc;
  notequal = 16_8bd;
  greaterthanequal = 16_8be;
  integral = 16_8bf;
  therefore = 16_8c0;
  variation = 16_8c1;
  infinity = 16_8c2;
  nabla = 16_8c5;
  approximate = 16_8c8;
  similarequal = 16_8c9;
  ifonlyif = 16_8cd;
  implies = 16_8ce;
  identical = 16_8cf;
  radical = 16_8d6;
  includedin = 16_8da;
  includes = 16_8db;
  intersection = 16_8dc;
  union = 16_8dd;
  logicaland = 16_8de;
  logicalor = 16_8df;
  partialderivative = 16_8ef;
  function = 16_8f6;
  leftarrow = 16_8fb;
  uparrow = 16_8fc;
  rightarrow = 16_8fd;
  downarrow = 16_8fe;
(* end of  TECHNICAL *)

(*
 *  Special
 *  Byte 3 = 9;
 *)

(* SPECIAL *)
  blank = 16_9df;
  soliddiamond = 16_9e0;
  checkerboard = 16_9e1;
  ht = 16_9e2;
  ff = 16_9e3;
  cr = 16_9e4;
  lf = 16_9e5;
  nl = 16_9e8;
  vt = 16_9e9;
  lowrightcorner = 16_9ea;
  uprightcorner = 16_9eb;
  upleftcorner = 16_9ec;
  lowleftcorner = 16_9ed;
  crossinglines = 16_9ee;
  horizlinescan1 = 16_9ef;
  horizlinescan3 = 16_9f0;
  horizlinescan5 = 16_9f1;
  horizlinescan7 = 16_9f2;
  horizlinescan9 = 16_9f3;
  leftt = 16_9f4;
  rightt = 16_9f5;
  bott = 16_9f6;
  topt = 16_9f7;
  vertbar = 16_9f8;
(* end of  SPECIAL *)

(*
 *  Publishing
 *  Byte 3 = a;
 *)

(* PUBLISHING *)
  emspace = 16_aa1;
  enspace = 16_aa2;
  em3space = 16_aa3;
  em4space = 16_aa4;
  digitspace = 16_aa5;
  punctspace = 16_aa6;
  thinspace = 16_aa7;
  hairspace = 16_aa8;
  emdash = 16_aa9;
  endash = 16_aaa;
  signifblank = 16_aac;
  ellipsis = 16_aae;
  doubbaselinedot = 16_aaf;
  onethird = 16_ab0;
  twothirds = 16_ab1;
  onefifth = 16_ab2;
  twofifths = 16_ab3;
  threefifths = 16_ab4;
  fourfifths = 16_ab5;
  onesixth = 16_ab6;
  fivesixths = 16_ab7;
  careof = 16_ab8;
  figdash = 16_abb;
  leftanglebracket = 16_abc;
  decimalpoint = 16_abd;
  rightanglebracket = 16_abe;
  marker = 16_abf;
  oneeighth = 16_ac3;
  threeeighths = 16_ac4;
  fiveeighths = 16_ac5;
  seveneighths = 16_ac6;
  trademark = 16_ac9;
  signaturemark = 16_aca;
  trademarkincircle = 16_acb;
  leftopentriangle = 16_acc;
  rightopentriangle = 16_acd;
  emopencircle = 16_ace;
  emopenrectangle = 16_acf;
  leftsinglequotemark = 16_ad0;
  rightsinglequotemark = 16_ad1;
  leftdoublequotemark = 16_ad2;
  rightdoublequotemark = 16_ad3;
  prescription = 16_ad4;
  minutes = 16_ad6;
  seconds = 16_ad7;
  latincross = 16_ad9;
  hexagram = 16_ada;
  filledrectbullet = 16_adb;
  filledlefttribullet = 16_adc;
  filledrighttribullet = 16_add;
  emfilledcircle = 16_ade;
  emfilledrect = 16_adf;
  enopencircbullet = 16_ae0;
  enopensquarebullet = 16_ae1;
  openrectbullet = 16_ae2;
  opentribulletup = 16_ae3;
  opentribulletdown = 16_ae4;
  openstar = 16_ae5;
  enfilledcircbullet = 16_ae6;
  enfilledsqbullet = 16_ae7;
  filledtribulletup = 16_ae8;
  filledtribulletdown = 16_ae9;
  leftpointer = 16_aea;
  rightpointer = 16_aeb;
  club = 16_aec;
  diamond = 16_aed;
  heart = 16_aee;
  maltesecross = 16_af0;
  dagger = 16_af1;
  doubledagger = 16_af2;
  checkmark = 16_af3;
  ballotcross = 16_af4;
  musicalsharp = 16_af5;
  musicalflat = 16_af6;
  malesymbol = 16_af7;
  femalesymbol = 16_af8;
  telephone = 16_af9;
  telephonerecorder = 16_afa;
  phonographcopyright = 16_afb;
  caret = 16_afc;
  singlelowquotemark = 16_afd;
  doublelowquotemark = 16_afe;
  cursor = 16_aff;
(* end of  PUBLISHING *)

(*
 *  APL
 *  Byte 3 = b;
 *)

(* APL *)
  leftcaret = 16_ba3;
  rightcaret = 16_ba6;
  downcaret = 16_ba8;
  upcaret = 16_ba9;
  overbar = 16_bc0;
  downtack = 16_bc2;
  upshoe = 16_bc3;
  downstile = 16_bc4;
  underbar = 16_bc6;
  jot = 16_bca;
  quad = 16_bcc;
  uptack = 16_bce;
  circle = 16_bcf;
  upstile = 16_bd3;
  downshoe = 16_bd6;
  rightshoe = 16_bd8;
  leftshoe = 16_bda;
  lefttack = 16_bdc;
  righttack = 16_bfc;
(* end of  APL *)

(*
 * Hebrew
 * Byte 3 = c
 *)

(* HEBREW *)
  Hebrew_doublelowline = 16_cdf;
  Hebrew_aleph = 16_ce0;
  Hebrew_bet = 16_ce1;
  Hebrew_beth = 16_ce1;  (* deprecated *)
  Hebrew_gimel = 16_ce2;
  Hebrew_gimmel = 16_ce2;  (* deprecated *)
  Hebrew_dalet = 16_ce3;
  Hebrew_daleth = 16_ce3;  (* deprecated *)
  Hebrew_he = 16_ce4;
  Hebrew_waw = 16_ce5;
  Hebrew_zain = 16_ce6;
  Hebrew_zayin = 16_ce6;  (* deprecated *)
  Hebrew_chet = 16_ce7;
  Hebrew_het = 16_ce7;  (* deprecated *)
  Hebrew_tet = 16_ce8;
  Hebrew_teth = 16_ce8;  (* deprecated *)
  Hebrew_yod = 16_ce9;
  Hebrew_finalkaph = 16_cea;
  Hebrew_kaph = 16_ceb;
  Hebrew_lamed = 16_cec;
  Hebrew_finalmem = 16_ced;
  Hebrew_mem = 16_cee;
  Hebrew_finalnun = 16_cef;
  Hebrew_nun = 16_cf0;
  Hebrew_samech = 16_cf1;
  Hebrew_samekh = 16_cf1;  (* deprecated *)
  Hebrew_ayin = 16_cf2;
  Hebrew_finalpe = 16_cf3;
  Hebrew_pe = 16_cf4;
  Hebrew_finalzade = 16_cf5;
  Hebrew_finalzadi = 16_cf5;  (* deprecated *)
  Hebrew_zade = 16_cf6;
  Hebrew_zadi = 16_cf6;  (* deprecated *)
  Hebrew_qoph = 16_cf7;
  Hebrew_kuf = 16_cf7;  (* deprecated *)
  Hebrew_resh = 16_cf8;
  Hebrew_shin = 16_cf9;
  Hebrew_taw = 16_cfa;
  Hebrew_taf = 16_cfa;  (* deprecated *)
  Hebrew_switch = 16_FF7E;  (* Alias for mode_switch *)
(* end of  HEBREW *)

END XKeySym.
