(* Copyright (C) 1996-2000, Critical Mass, Inc.   All rights reserved.  *)
(* See file COPYRIGHT-CMASS for details.                                *)
(*                                                                      *)
(* Dr. Hans-Walter Latz, 24.10.1996: added support for non-US keyboards *)

MODULE WinKey;

IMPORT Word, VBT, WinDef;
IMPORT KeyboardKey AS KK, Latin1Key AS LK, WinUser AS WU;

PROCEDURE Translate (vk: [0 .. 255]): VBT.KeySym =
  VAR
    shifted   := Word.And (WU.GetKeyState (WU.VK_SHIFT  ), 16_8000) # 0;
    alt       := Word.And (WU.GetKeyState (WU.VK_CONTROL), 16_8000) # 0;
    control   := Word.And (WU.GetKeyState (WU.VK_MENU   ), 16_8000) # 0;
    caps_lock := Word.And (WU.GetKeyState (WU.VK_CAPITAL), 16_0001) # 0;
    ksym      : VBT.KeySym := XX;
  BEGIN
    IF    alt AND control     THEN ksym :=  Map[vk].altgr;
    ELSIF alt                 THEN ksym :=  Map[vk].alt;
    ELSIF control             THEN ksym :=  Map[vk].control;
    ELSIF shifted # caps_lock THEN ksym :=  Map[vk].shifted;
    END;
    IF ksym = XX THEN ksym := Map[vk].normal; END;
    RETURN ksym;
  END Translate;

CONST
  XX = KK.VoidSymbol;

TYPE
  KD = RECORD (*KeyDesc*)
    win_key : [0..255];
    normal  : [0..16_FFFFFF];
    shifted : [0..16_FFFFFF];
    control : [0..16_FFFFFF] := XX;
    alt     : [0..16_FFFFFF] := XX;
    altgr   : [0..16_FFFFFF] := XX;
  END;
  
VAR
  Map := ARRAY [0..255] OF KD {
  (* 00 *) KD{ 16_00,           XX,              XX               },
  (* 01 *) KD{ WU.VK_LBUTTON,   XX,              XX               },
  (* 02 *) KD{ WU.VK_RBUTTON,   XX,              XX               },
  (* 03 *) KD{ WU.VK_CANCEL,    KK.Cancel,       KK.Cancel        },
  (* 04 *) KD{ WU.VK_MBUTTON,   XX,              XX               },
  (* 05 *) KD{ 16_05,           XX,              XX               },
  (* 06 *) KD{ 16_06,           XX,              XX               },
  (* 07 *) KD{ 16_07,           XX,              XX               },
  (* 08 *) KD{ WU.VK_BACK,      KK.BackSpace,    KK.BackSpace     },
  (* 09 *) KD{ WU.VK_TAB,       KK.Tab,          KK.Tab           },
  (* 0A *) KD{ 16_0A,           XX,              XX               },
  (* 0B *) KD{ 16_0B,           XX,              XX               },
  (* 0C *) KD{ WU.VK_CLEAR,     KK.Clear,        KK.Clear         },
  (* 0D *) KD{ WU.VK_RETURN,    KK.Return,       KK.Return        },
  (* 0E *) KD{ 16_0E,           XX,              XX               },
  (* 0F *) KD{ 16_0F,           XX,              XX               },

  (* 10 *) KD{ WU.VK_SHIFT,     KK.Shift_L,      KK.Shift_L       },
  (* 11 *) KD{ WU.VK_CONTROL,   KK.Control_L,    KK.Control_L     },
  (* 12 *) KD{ WU.VK_MENU,      KK.Menu,         KK.Menu          },
  (* 13 *) KD{ WU.VK_PAUSE,     KK.Pause,        KK.Pause         },
  (* 14 *) KD{ WU.VK_CAPITAL,   KK.Caps_Lock,    KK.Caps_Lock     },
  (* 15 *) KD{ 16_15,           XX,              XX               },
  (* 16 *) KD{ 16_16,           XX,              XX               },
  (* 17 *) KD{ 16_17,           XX,              XX               },
  (* 18 *) KD{ 16_18,           XX,              XX               },
  (* 19 *) KD{ 16_19,           XX,              XX               },
  (* 1A *) KD{ 16_1A,           XX,              XX               },
  (* 1B *) KD{ WU.VK_ESCAPE,    KK.Escape,       KK.Escape        },
  (* 1C *) KD{ 16_1C,           XX,              XX               },
  (* 1D *) KD{ 16_1D,           XX,              XX               },
  (* 1E *) KD{ 16_1E,           XX,              XX               },
  (* 1F *) KD{ 16_1F,           XX,              XX               },

  (* 20 *) KD{ WU.VK_SPACE,     LK.space,        LK.space         },
  (* 21 *) KD{ WU.VK_PRIOR,     KK.Prior,        KK.Prior         },
  (* 22 *) KD{ WU.VK_NEXT,      KK.Next,         KK.Next          },
  (* 23 *) KD{ WU.VK_END,       KK.End,          KK.End           },
  (* 24 *) KD{ WU.VK_HOME,      KK.Home,         KK.Home          },
  (* 25 *) KD{ WU.VK_LEFT,      KK.Left,         KK.Left          },
  (* 26 *) KD{ WU.VK_UP,        KK.Up,           KK.Up            },
  (* 27 *) KD{ WU.VK_RIGHT,     KK.Right,        KK.Right         },
  (* 28 *) KD{ WU.VK_DOWN,      KK.Down,         KK.Down          },
  (* 29 *) KD{ WU.VK_SELECT,    KK.Select,       KK.Select        },
  (* 2A *) KD{ WU.VK_PRINT,     KK.Print,        KK.Print         },
  (* 2B *) KD{ WU.VK_EXECUTE,   KK.Execute,      KK.Execute       },
  (* 2C *) KD{ WU.VK_SNAPSHOT,  XX,              XX               },
  (* 2D *) KD{ WU.VK_INSERT,    KK.Insert,       KK.Insert        },
  (* 2E *) KD{ WU.VK_DELETE,    KK.Delete,       KK.Delete        },
  (* 2F *) KD{ WU.VK_HELP,      KK.Help,         KK.Help          },

  (* 30 *) KD{ 16_30,           LK.zero,         LK.parenright    },
  (* 31 *) KD{ 16_31,           LK.one,          LK.exclam        },
  (* 32 *) KD{ 16_32,           LK.two,          LK.at            },
  (* 33 *) KD{ 16_33,           LK.three,        LK.numbersign    },
  (* 34 *) KD{ 16_34,           LK.four,         LK.dollar        },
  (* 35 *) KD{ 16_35,           LK.five,         LK.percent       },
  (* 36 *) KD{ 16_36,           LK.six,          LK.asciicircum   },
  (* 37 *) KD{ 16_37,           LK.seven,        LK.ampersand     },
  (* 38 *) KD{ 16_38,           LK.eight,        LK.asterisk      },
  (* 39 *) KD{ 16_39,           LK.nine,         LK.parenleft     },
  (* 3A *) KD{ 16_3A,           XX,              XX               },
  (* 3B *) KD{ 16_3B,           XX,              XX               },
  (* 3C *) KD{ 16_3C,           XX,              XX               },
  (* 3D *) KD{ 16_3D,           XX,              XX               },
  (* 3E *) KD{ 16_3E,           XX,              XX               },
  (* 3F *) KD{ 16_3F,           XX,              XX               },

  (* 40 *) KD{ 16_40,           XX,              XX               },
  (* 41 *) KD{ 16_41,           LK.a,            LK.A             },
  (* 42 *) KD{ 16_42,           LK.b,            LK.B             },
  (* 43 *) KD{ 16_43,           LK.c,            LK.C             },
  (* 44 *) KD{ 16_44,           LK.d,            LK.D             },
  (* 45 *) KD{ 16_45,           LK.e,            LK.E             },
  (* 46 *) KD{ 16_46,           LK.f,            LK.F             },
  (* 47 *) KD{ 16_47,           LK.g,            LK.G             },
  (* 48 *) KD{ 16_48,           LK.h,            LK.H             },
  (* 49 *) KD{ 16_49,           LK.i,            LK.I             },
  (* 4A *) KD{ 16_4A,           LK.j,            LK.J             },
  (* 4B *) KD{ 16_4B,           LK.k,            LK.K             },
  (* 4C *) KD{ 16_4C,           LK.l,            LK.L             },
  (* 4D *) KD{ 16_4D,           LK.m,            LK.M             },
  (* 4E *) KD{ 16_4E,           LK.n,            LK.N             },
  (* 4F *) KD{ 16_4F,           LK.o,            LK.O             },

  (* 50 *) KD{ 16_50,           LK.p,            LK.P             },
  (* 51 *) KD{ 16_51,           LK.q,            LK.Q             },
  (* 52 *) KD{ 16_52,           LK.r,            LK.R             },
  (* 53 *) KD{ 16_53,           LK.s,            LK.S             },
  (* 54 *) KD{ 16_54,           LK.t,            LK.T             },
  (* 55 *) KD{ 16_55,           LK.u,            LK.U             },
  (* 56 *) KD{ 16_56,           LK.v,            LK.V             },
  (* 57 *) KD{ 16_57,           LK.w,            LK.W             },
  (* 58 *) KD{ 16_58,           LK.x,            LK.X             },
  (* 59 *) KD{ 16_59,           LK.y,            LK.Y             },
  (* 5A *) KD{ 16_5A,           LK.z,            LK.Z             },
  (* 5B *) KD{ 16_5B,           XX,              XX               },
  (* 5C *) KD{ 16_5C,           XX,              XX               },
  (* 5D *) KD{ 16_5D,           XX,              XX               },
  (* 5E *) KD{ 16_5E,           XX,              XX               },
  (* 5F *) KD{ 16_5F,           XX,              XX               },

  (* 60 *) KD{ WU.VK_NUMPAD0,   KK.KP_0,         KK.KP_0          },
  (* 61 *) KD{ WU.VK_NUMPAD1,   KK.KP_1,         KK.KP_1          },
  (* 62 *) KD{ WU.VK_NUMPAD2,   KK.KP_2,         KK.KP_2          },
  (* 63 *) KD{ WU.VK_NUMPAD3,   KK.KP_3,         KK.KP_3          },
  (* 64 *) KD{ WU.VK_NUMPAD4,   KK.KP_4,         KK.KP_4          },
  (* 65 *) KD{ WU.VK_NUMPAD5,   KK.KP_5,         KK.KP_5          },
  (* 66 *) KD{ WU.VK_NUMPAD6,   KK.KP_6,         KK.KP_6          },
  (* 67 *) KD{ WU.VK_NUMPAD7,   KK.KP_7,         KK.KP_7          },
  (* 68 *) KD{ WU.VK_NUMPAD8,   KK.KP_8,         KK.KP_8          },
  (* 69 *) KD{ WU.VK_NUMPAD9,   KK.KP_9,         KK.KP_9          },
  (* 6A *) KD{ WU.VK_MULTIPLY,  KK.KP_Multiply,  KK.KP_Multiply   },
  (* 6B *) KD{ WU.VK_ADD,       KK.KP_Add,       KK.KP_Add        },
  (* 6C *) KD{ WU.VK_SEPARATOR, KK.KP_Separator, KK.KP_Separator  },
  (* 6D *) KD{ WU.VK_SUBTRACT,  KK.KP_Subtract,  KK.KP_Subtract   },
  (* 6E *) KD{ WU.VK_DECIMAL,   KK.KP_Decimal,   KK.KP_Decimal    },
  (* 6F *) KD{ WU.VK_DIVIDE,    KK.KP_Divide,    KK.KP_Divide     },
        
  (* 70 *) KD{ WU.VK_F1,        KK.F1,           KK.F1            },
  (* 71 *) KD{ WU.VK_F2,        KK.F2,           KK.F2            },
  (* 72 *) KD{ WU.VK_F3,        KK.F3,           KK.F3            },
  (* 73 *) KD{ WU.VK_F4,        KK.F4,           KK.F4            },
  (* 74 *) KD{ WU.VK_F5,        KK.F5,           KK.F5            },
  (* 75 *) KD{ WU.VK_F6,        KK.F6,           KK.F6            },
  (* 76 *) KD{ WU.VK_F7,        KK.F7,           KK.F7            },
  (* 77 *) KD{ WU.VK_F8,        KK.F8,           KK.F8            },
  (* 78 *) KD{ WU.VK_F9,        KK.F9,           KK.F9            },
  (* 79 *) KD{ WU.VK_F10,       KK.F10,          KK.F10           },
  (* 7A *) KD{ WU.VK_F11,       KK.F11,          KK.F11           },
  (* 7B *) KD{ WU.VK_F12,       KK.F12,          KK.F12           },
  (* 7C *) KD{ WU.VK_F13,       KK.F13,          KK.F13           },
  (* 7D *) KD{ WU.VK_F14,       KK.F14,          KK.F14           },
  (* 7E *) KD{ WU.VK_F15,       KK.F15,          KK.F15           },
  (* 7F *) KD{ WU.VK_F16,       KK.F16,          KK.F16           },

  (* 80 *) KD{ WU.VK_F17,       KK.F17,          KK.F17           },
  (* 81 *) KD{ WU.VK_F18,       KK.F18,          KK.F18           },
  (* 82 *) KD{ WU.VK_F19,       KK.F19,          KK.F19           },
  (* 83 *) KD{ WU.VK_F20,       KK.F20,          KK.F20           },
  (* 84 *) KD{ WU.VK_F21,       KK.F21,          KK.F21           },
  (* 85 *) KD{ WU.VK_F22,       KK.F22,          KK.F22           },
  (* 86 *) KD{ WU.VK_F23,       KK.F23,          KK.F23           },
  (* 87 *) KD{ WU.VK_F24,       KK.F24,          KK.F24           },
  (* 88 *) KD{ 16_88,           XX,              XX               },
  (* 89 *) KD{ 16_89,           XX,              XX               },
  (* 8A *) KD{ 16_8A,           XX,              XX               },
  (* 8B *) KD{ 16_8B,           XX,              XX               },
  (* 8C *) KD{ 16_8C,           XX,              XX               },
  (* 8D *) KD{ 16_8D,           XX,              XX               },
  (* 8E *) KD{ 16_8E,           XX,              XX               },
  (* 8F *) KD{ 16_8F,           XX,              XX               },

  (* 90 *) KD{ WU.VK_NUMLOCK,   KK.Num_Lock,     KK.Num_Lock      },
  (* 91 *) KD{ WU.VK_SCROLL,    KK.Scroll_Lock,  KK.Scroll_Lock   },
  (* 92 *) KD{ 16_92,           XX,              XX               },
  (* 93 *) KD{ 16_93,           XX,              XX               },
  (* 94 *) KD{ 16_94,           XX,              XX               },
  (* 95 *) KD{ 16_95,           XX,              XX               },
  (* 96 *) KD{ 16_96,           XX,              XX               },
  (* 97 *) KD{ 16_97,           XX,              XX               },
  (* 98 *) KD{ 16_98,           XX,              XX               },
  (* 99 *) KD{ 16_99,           XX,              XX               },
  (* 9A *) KD{ 16_9A,           XX,              XX               },
  (* 9B *) KD{ 16_9B,           XX,              XX               },
  (* 9C *) KD{ 16_9C,           XX,              XX               },
  (* 9D *) KD{ 16_9D,           XX,              XX               },
  (* 9E *) KD{ 16_9E,           XX,              XX               },
  (* 9F *) KD{ 16_9F,           XX,              XX               },

  (* A0 *) KD{ WU.VK_LSHIFT,    KK.Shift_L,      KK.Shift_L       },
  (* A1 *) KD{ WU.VK_RSHIFT,    KK.Shift_R,      KK.Shift_R       },
  (* A2 *) KD{ WU.VK_LCONTROL,  KK.Control_L,    KK.Control_L     },
  (* A3 *) KD{ WU.VK_RCONTROL,  KK.Control_R,    KK.Control_R     },
  (* A4 *) KD{ WU.VK_LMENU,     KK.Alt_L,        KK.Alt_L         },
  (* A5 *) KD{ WU.VK_RMENU,     KK.Alt_R,        KK.Alt_R         },
  (* A6 *) KD{ 16_A6,           XX,              XX               },
  (* A7 *) KD{ 16_A7,           XX,              XX               },
  (* A8 *) KD{ 16_A8,           XX,              XX               },
  (* A9 *) KD{ 16_A9,           XX,              XX               },
  (* AA *) KD{ 16_AA,           XX,              XX               },
  (* AB *) KD{ 16_AB,           XX,              XX               },
  (* AC *) KD{ 16_AC,           XX,              XX               },
  (* AD *) KD{ 16_AD,           XX,              XX               },
  (* AE *) KD{ 16_AE,           XX,              XX               },
  (* AF *) KD{ 16_AF,           XX,              XX               },

  (* B0 *) KD{ 16_B0,           XX,              XX               },
  (* B1 *) KD{ 16_B1,           XX,              XX               },
  (* B2 *) KD{ 16_B2,           XX,              XX               },
  (* B3 *) KD{ 16_B3,           XX,              XX               },
  (* B4 *) KD{ 16_B4,           XX,              XX               },
  (* B5 *) KD{ 16_B5,           XX,              XX               },
  (* B6 *) KD{ 16_B6,           XX,              XX               },
  (* B7 *) KD{ 16_B7,           XX,              XX               },
  (* B8 *) KD{ 16_B8,           XX,              XX               },
  (* B9 *) KD{ 16_B9,           XX,              XX               },
  (* BA *) KD{ 16_BA,           LK.semicolon,    LK.colon         },
  (* BB *) KD{ 16_BB,           LK.equal,        LK.plus          },
  (* BC *) KD{ 16_BC,           LK.comma,        LK.less          },
  (* BD *) KD{ 16_BD,           LK.minus,        LK.underscore    },
  (* BE *) KD{ 16_BE,           LK.period,       LK.greater       },
  (* BF *) KD{ 16_BF,           LK.slash,        LK.question      },

  (* C0 *) KD{ 16_C0,           LK.grave,        LK.asciitilde    },
  (* C1 *) KD{ 16_C1,           XX,              XX               },
  (* C2 *) KD{ 16_C2,           XX,              XX               },
  (* C3 *) KD{ 16_C3,           XX,              XX               },
  (* C4 *) KD{ 16_C4,           XX,              XX               },
  (* C5 *) KD{ 16_C5,           XX,              XX               },
  (* C6 *) KD{ 16_C6,           XX,              XX               },
  (* C7 *) KD{ 16_C7,           XX,              XX               },
  (* C8 *) KD{ 16_C8,           XX,              XX               },
  (* C9 *) KD{ 16_C9,           XX,              XX               },
  (* CA *) KD{ 16_CA,           XX,              XX               },
  (* CB *) KD{ 16_CB,           XX,              XX               },
  (* CC *) KD{ 16_CC,           XX,              XX               },
  (* CD *) KD{ 16_CD,           XX,              XX               },
  (* CE *) KD{ 16_CE,           XX,              XX               },
  (* CF *) KD{ 16_CF,           XX,              XX               },

  (* D0 *) KD{ 16_D0,           XX,              XX               },
  (* D1 *) KD{ 16_D1,           XX,              XX               },
  (* D2 *) KD{ 16_D2,           XX,              XX               },
  (* D3 *) KD{ 16_D3,           XX,              XX               },
  (* D4 *) KD{ 16_D4,           XX,              XX               },
  (* D5 *) KD{ 16_D5,           XX,              XX               },
  (* D6 *) KD{ 16_D6,           XX,              XX               },
  (* D7 *) KD{ 16_D7,           XX,              XX               },
  (* D8 *) KD{ 16_D8,           XX,              XX               },
  (* D9 *) KD{ 16_D9,           XX,              XX               },
  (* DA *) KD{ 16_DA,           XX,              XX               },
  (* DB *) KD{ 16_DB,           LK.bracketleft,  LK.braceleft     },
  (* DC *) KD{ 16_DC,           LK.backslash,    LK.bar           },
  (* DD *) KD{ 16_DD,           LK.bracketright, LK.braceright    },
  (* DE *) KD{ 16_DE,           LK.apostrophe,   LK.quotedbl      },
  (* DF *) KD{ 16_DF,           XX,              XX               },

  (* E0 *) KD{ 16_E0,           XX,              XX               },
  (* E1 *) KD{ 16_E1,           XX,              XX               },
  (* E2 *) KD{ 16_E2,           XX,              XX               },
  (* E3 *) KD{ 16_E3,           XX,              XX               },
  (* E4 *) KD{ 16_E4,           XX,              XX               },
  (* E5 *) KD{ 16_E5,           XX,              XX               },
  (* E6 *) KD{ 16_E6,           XX,              XX               },
  (* E7 *) KD{ 16_E7,           XX,              XX               },
  (* E8 *) KD{ 16_E8,           XX,              XX               },
  (* E9 *) KD{ 16_E9,           XX,              XX               },
  (* EA *) KD{ 16_EA,           XX,              XX               },
  (* EB *) KD{ 16_EB,           XX,              XX               },
  (* EC *) KD{ 16_EC,           XX,              XX               },
  (* ED *) KD{ 16_ED,           XX,              XX               },
  (* EE *) KD{ 16_EE,           XX,              XX               },
  (* EF *) KD{ 16_EF,           XX,              XX               },

  (* F0 *) KD{ 16_F0,           XX,              XX               },
  (* F1 *) KD{ 16_F1,           XX,              XX               },
  (* F2 *) KD{ 16_F2,           XX,              XX               },
  (* F3 *) KD{ 16_F3,           XX,              XX               },
  (* F4 *) KD{ 16_F4,           XX,              XX               },
  (* F5 *) KD{ 16_F5,           XX,              XX               },
  (* F6 *) KD{ WU.VK_ATTN,      XX,              XX               },
  (* F7 *) KD{ WU.VK_CRSEL,     XX,              XX               },
  (* F8 *) KD{ WU.VK_EXSEL,     XX,              XX               },
  (* F9 *) KD{ WU.VK_EREOF,     XX,              XX               },
  (* FA *) KD{ WU.VK_PLAY,      XX,              XX               },
  (* FB *) KD{ WU.VK_ZOOM,      XX,              XX               },
  (* FC *) KD{ WU.VK_NONAME,    XX,              XX               },
  (* FD *) KD{ WU.VK_PA1,       XX,              XX               },
  (* FE *) KD{ WU.VK_OEM_CLEAR, XX,              XX               },
  (* FF *) KD{ 16_FF,           XX,              XX               }
  };

PROCEDURE MapChar(c: [0..255]) =
  VAR
    shift, control, alt: BOOLEAN;
    data: WinDef.SHORT;
    vk: [0..255];
  BEGIN
    (* get the virtual key data *)
    data := WU.VkKeyScan (VAL (c, CHAR));
    IF data = 16_FFFF THEN RETURN; END; (* 'c' can't be entered on Keyboard *)
  
    (* extract the interesting fields *)
    vk      := Word.And (data, 16_00FF);
    shift   := Word.And (data, 16_0100) # 0;
    control := Word.And (data, 16_0200) # 0;
    alt     := Word.And (data, 16_0400) # 0;
  
    (* store the characters data in the map *)
    IF    alt AND control THEN Map[vk].altgr   := c;
    ELSIF alt             THEN Map[vk].alt     := c;
    ELSIF control         THEN Map[vk].control := c;
    ELSIF shift           THEN Map[vk].shifted := c;
    ELSE                       Map[vk].normal  := c;
    END;
  END MapChar;

BEGIN
  (* get key codes / shift modes for standard printable ASCII chars *)
  FOR c :=  32 TO 127 DO  MapChar(c);  END;
    
  (* get key codes / shift modes for printable international chars *)
  FOR c := 160 TO 255 DO  MapChar(c);  END;

  (* verify that the Windows mapping didn't change... *)
  FOR i := FIRST (Map) TO LAST (Map) DO  <* ASSERT Map[i].win_key = i *>  END;
END WinKey.
