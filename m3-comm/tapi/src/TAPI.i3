(* The  Telephony  API  is jointly copyrighted by Intel and Microsoft.  You are *)
(* granted  a royalty free worldwide, unlimited license to make copies, and use *)
(* the   API/SPI  for  making  applications/drivers  that  interface  with  the *)
(* specification provided that this paragraph and the Intel/Microsoft copyright *)
(* statement is maintained as is in the text and source code files. *)

(* Copyright 1995 Microsoft, all rights reserved. *)
(* Portions copyright 1992, 1993 Intel/Microsoft, all rights reserved. *)

(* Translation to Modula-3 Copyright 1996 Critical Mass, Inc. All rights reserved. *)

UNSAFE INTERFACE TAPI;

FROM Ctypes IMPORT char, int, unsigned_char, unsigned_int;
IMPORT WinUser;


TYPE
  LONG = int;
  DWORD = unsigned_int;
  LPDWORD = ADDRESS;
  LPVOID = ADDRESS (* void star *);
  PVOID =  ADDRESS (* void *);
  HANDLE = UNTRACED REF ADDRESS (* void *);
  HICON = ADDRESS (* void *);
  HWND = ADDRESS (* void *);
  HINSTANCE = ADDRESS (* void *);
  UINT = unsigned_int;
  WPARAM = unsigned_int;
  (** CHAR = signed_char; **)
  LPSTR = UNTRACED REF CHAR;
  PSTR = UNTRACED REF CHAR;
  LPCSTR = UNTRACED REF CHAR;
  PCSTR = UNTRACED REF CHAR;

(* #pragma pack(1) *)
(* Type definitions of the data types used in tapi *)

  HCALL__ = RECORD
    unused        : int;
  END;
  HCALL = UNTRACED REF HCALL__;
  LPHCALL = UNTRACED REF HCALL;

  HLINE__ = RECORD
    unused        : int;
  END;
  HLINE = UNTRACED REF HLINE__;
  LPHLINE = UNTRACED REF HLINE;

  HPHONE__ = RECORD
    unused        : int;
  END;
  HPHONE = UNTRACED REF HPHONE__;
  LPHPHONE = UNTRACED REF HPHONE;

  HLINEAPP__ = RECORD
    unused        : int;
  END;
  HLINEAPP = UNTRACED REF HLINEAPP__;
  LPHLINEAPP = UNTRACED REF HLINEAPP;

  HPHONEAPP__ = RECORD
    unused        : int;
  END;
  HPHONEAPP = UNTRACED REF HPHONEAPP__;
  LPHPHONEAPP = UNTRACED REF HPHONEAPP;
  LPHICON = UNTRACED REF HICON;

(* typedef of the LINE callback procedure *)

TYPE
  LINECALLBACK = <*CALLBACK*> PROCEDURE (hDevice, dwMessage, dwInstance,
                                         dwParam1, dwParam2, dwParam3: DWORD);

  PHONECALLBACK = <*CALLBACK*> PROCEDURE (hDevice, dwMessage, dwInstance,
                                         dwParam1, dwParam2, dwParam3: DWORD);

CONST
  LINE_ADDRESSSTATE     = 0;
  LINE_CALLINFO         = 1;
  LINE_CALLSTATE        = 2;
  LINE_CLOSE            = 3;
  LINE_DEVSPECIFIC      = 4;
  LINE_DEVSPECIFICFEATURE= 5;
  LINE_GATHERDIGITS      = 6;
  LINE_GENERATE          = 7;
  LINE_LINEDEVSTATE      = 8;
  LINE_MONITORDIGITS     = 9;
  LINE_MONITORMEDIA      = 10;
  LINE_MONITORTONE       = 11;
  LINE_REPLY             = 12;
  LINE_REQUEST           = 13;
  PHONE_BUTTON           = 14;
  PHONE_CLOSE            = 15;
  PHONE_DEVSPECIFIC      = 16;
  PHONE_REPLY            = 17;
  PHONE_STATE            = 18;
  LINE_CREATE            = 19;
  PHONE_CREATE           = 20;

CONST
  TAPI_REPLY             = WinUser.WM_USER + 99;
  
  TAPIERR_CONNECTED         =  0;
  TAPIERR_DROPPED           = -1;
  TAPIERR_NOREQUESTRECIPIENT= -2;
  TAPIERR_REQUESTQUEUEFULL  = -3;
  TAPIERR_INVALDESTADDRESS  = -4;
  TAPIERR_INVALWINDOWHANDLE = -5;
  TAPIERR_INVALDEVICECLASS  = -6;
  TAPIERR_INVALDEVICEID     = -7;
  TAPIERR_DEVICECLASSUNAVAIL= -8;
  TAPIERR_DEVICEIDUNAVAIL   = -9;
  TAPIERR_DEVICEINUSE       = -10;
  TAPIERR_DESTBUSY          = -11;
  TAPIERR_DESTNOANSWER      = -12;
  TAPIERR_DESTUNAVAIL       = -13;
  TAPIERR_UNKNOWNWINHANDLE  = -14;
  TAPIERR_UNKNOWNREQUESTID  = -15;
  TAPIERR_REQUESTFAILED     = -16;
  TAPIERR_REQUESTCANCELLED  = -17;
  TAPIERR_INVALPOINTER      = -18;

CONST
  TAPIMAXDESTADDRESSSIZE    = 80;
  TAPIMAXAPPNAMESIZE        = 40;
  TAPIMAXCALLEDPARTYSIZE    = 40;
  TAPIMAXCOMMENTSIZE        = 80;
  TAPIMAXDEVICECLASSSIZE    = 40;
  TAPIMAXDEVICEIDSIZE       = 40;

(* Data types and values for Phones *)

  PHONEBUTTONFUNCTION_UNKNOWN   = 16_00000000;
  PHONEBUTTONFUNCTION_CONFERENCE= 16_00000001;
  PHONEBUTTONFUNCTION_TRANSFER  = 16_00000002;
  PHONEBUTTONFUNCTION_DROP      = 16_00000003;
  PHONEBUTTONFUNCTION_HOLD      = 16_00000004;
  PHONEBUTTONFUNCTION_RECALL    = 16_00000005;
  PHONEBUTTONFUNCTION_DISCONNECT= 16_00000006;
  PHONEBUTTONFUNCTION_CONNECT   = 16_00000007;
  PHONEBUTTONFUNCTION_MSGWAITON = 16_00000008;
  PHONEBUTTONFUNCTION_MSGWAITOFF= 16_00000009;
  PHONEBUTTONFUNCTION_SELECTRING= 16_0000000A;
  PHONEBUTTONFUNCTION_ABBREVDIAL= 16_0000000B;
  PHONEBUTTONFUNCTION_FORWARD   = 16_0000000C;
  PHONEBUTTONFUNCTION_PICKUP    = 16_0000000D;
  PHONEBUTTONFUNCTION_RINGAGAIN = 16_0000000E;
  PHONEBUTTONFUNCTION_PARK      = 16_0000000F;
  PHONEBUTTONFUNCTION_REJECT    = 16_00000010;
  PHONEBUTTONFUNCTION_REDIRECT  = 16_00000011;
  PHONEBUTTONFUNCTION_MUTE      = 16_00000012;
  PHONEBUTTONFUNCTION_VOLUMEUP  = 16_00000013;
  PHONEBUTTONFUNCTION_VOLUMEDOWN= 16_00000014;
  PHONEBUTTONFUNCTION_SPEAKERON = 16_00000015;
  PHONEBUTTONFUNCTION_SPEAKEROFF= 16_00000016;
  PHONEBUTTONFUNCTION_FLASH     = 16_00000017;
  PHONEBUTTONFUNCTION_DATAON    = 16_00000018;
  PHONEBUTTONFUNCTION_DATAOFF   = 16_00000019;
  PHONEBUTTONFUNCTION_DONOTDISTURB= 16_0000001A;
  PHONEBUTTONFUNCTION_INTERCOM    = 16_0000001B;
  PHONEBUTTONFUNCTION_BRIDGEDAPP  = 16_0000001C;
  PHONEBUTTONFUNCTION_BUSY        = 16_0000001D;
  PHONEBUTTONFUNCTION_CALLAPP     = 16_0000001E;
  PHONEBUTTONFUNCTION_DATETIME    = 16_0000001F;
  PHONEBUTTONFUNCTION_DIRECTORY   = 16_00000020;
  PHONEBUTTONFUNCTION_COVER       = 16_00000021;
  PHONEBUTTONFUNCTION_CALLID      = 16_00000022;
  PHONEBUTTONFUNCTION_LASTNUM     = 16_00000023;
  PHONEBUTTONFUNCTION_NIGHTSRV    = 16_00000024;
  PHONEBUTTONFUNCTION_SENDCALLS   = 16_00000025;
  PHONEBUTTONFUNCTION_MSGINDICATOR= 16_00000026;
  PHONEBUTTONFUNCTION_REPDIAL     = 16_00000027;
  PHONEBUTTONFUNCTION_SETREPDIAL  = 16_00000028;
  PHONEBUTTONFUNCTION_SYSTEMSPEED = 16_00000029;
  PHONEBUTTONFUNCTION_STATIONSPEED= 16_0000002A;
  PHONEBUTTONFUNCTION_CAMPON      = 16_0000002B;
  PHONEBUTTONFUNCTION_SAVEREPEAT  = 16_0000002C;
  PHONEBUTTONFUNCTION_QUEUECALL   = 16_0000002D;
  PHONEBUTTONFUNCTION_NONE        = 16_0000002E;
  
TYPE
  phonebuttoninfo_tag = RECORD
    dwTotalSize                : DWORD;
    dwNeededSize               : DWORD;
    dwUsedSize                 : DWORD;
    dwButtonMode               : DWORD;
    dwButtonFunction           : DWORD;
    dwButtonTextSize           : DWORD;
    dwButtonTextOffset         : DWORD;
    dwDevSpecificSize          : DWORD;
    dwDevSpecificOffset        : DWORD;
    dwButtonState              : DWORD;
  END;

  PHONEBUTTONINFO = phonebuttoninfo_tag;
  LPPHONEBUTTONINFO = UNTRACED REF phonebuttoninfo_tag;

CONST
  PHONEBUTTONMODE_DUMMY           = 16_00000001;
  PHONEBUTTONMODE_CALL            = 16_00000002;
  PHONEBUTTONMODE_FEATURE         = 16_00000004;
  PHONEBUTTONMODE_KEYPAD          = 16_00000008;
  PHONEBUTTONMODE_LOCAL           = 16_00000010;
  PHONEBUTTONMODE_DISPLAY         = 16_00000020;

  PHONEBUTTONSTATE_UP             = 16_00000001;
  PHONEBUTTONSTATE_DOWN           = 16_00000002;
  PHONEBUTTONSTATE_UNKNOWN        = 16_00000004;
  PHONEBUTTONSTATE_UNAVAIL        = 16_00000008;

  
TYPE
  phoneextensionid_tag = RECORD
    dwExtensionID0        : DWORD;
    dwExtensionID1        : DWORD;
    dwExtensionID2        : DWORD;
    dwExtensionID3        : DWORD;
  END;

  PHONEEXTENSIONID = phoneextensionid_tag;
  LPPHONEEXTENSIONID = UNTRACED REF phoneextensionid_tag;

  phonecaps_tag = RECORD
    dwTotalSize                     : DWORD;
    dwNeededSize                    : DWORD;
    dwUsedSize                      : DWORD;
    dwProviderInfoSize              : DWORD;
    dwProviderInfoOffset            : DWORD;
    dwPhoneInfoSize                 : DWORD;
    dwPhoneInfoOffset               : DWORD;
    dwPermanentPhoneID              : DWORD;
    dwPhoneNameSize                 : DWORD;
    dwPhoneNameOffset               : DWORD;
    dwStringFormat                  : DWORD;
    dwPhoneStates                   : DWORD;
    dwHookSwitchDevs                : DWORD;
    dwHandsetHookSwitchModes        : DWORD;
    dwSpeakerHookSwitchModes        : DWORD;
    dwHeadsetHookSwitchModes        : DWORD;
    dwVolumeFlags                   : DWORD;
    dwGainFlags                     : DWORD;
    dwDisplayNumRows                : DWORD;
    dwDisplayNumColumns             : DWORD;
    dwNumRingModes                  : DWORD;
    dwNumButtonLamps                : DWORD;
    dwButtonModesSize               : DWORD;
    dwButtonModesOffset             : DWORD;
    dwButtonFunctionsSize           : DWORD;
    dwButtonFunctionsOffset         : DWORD;
    dwLampModesSize                 : DWORD;
    dwLampModesOffset               : DWORD;
    dwNumSetData                    : DWORD;
    dwSetDataSize                   : DWORD;
    dwSetDataOffset                 : DWORD;
    dwNumGetData                    : DWORD;
    dwGetDataSize                   : DWORD;
    dwGetDataOffset                 : DWORD;
    dwDevSpecificSize               : DWORD;
    dwDevSpecificOffset             : DWORD;
  END;

  PHONECAPS = phonecaps_tag;
  LPPHONECAPS = UNTRACED REF phonecaps_tag;

CONST
  PHONEERR_ALLOCATED              = 16_90000001;
  PHONEERR_BADDEVICEID            = 16_90000002;
  PHONEERR_INCOMPATIBLEAPIVERSION = 16_90000003;
  PHONEERR_INCOMPATIBLEEXTVERSION = 16_90000004;
  PHONEERR_INIFILECORRUPT         = 16_90000005;
  PHONEERR_INUSE                  = 16_90000006;
  PHONEERR_INVALAPPHANDLE         = 16_90000007;
  PHONEERR_INVALAPPNAME           = 16_90000008;
  PHONEERR_INVALBUTTONLAMPID      = 16_90000009;
  PHONEERR_INVALBUTTONMODE        = 16_9000000A;
  PHONEERR_INVALBUTTONSTATE       = 16_9000000B;
  PHONEERR_INVALDATAID            = 16_9000000C;
  PHONEERR_INVALDEVICECLASS       = 16_9000000D;
  PHONEERR_INVALEXTVERSION        = 16_9000000E;
  PHONEERR_INVALHOOKSWITCHDEV     = 16_9000000F;
  PHONEERR_INVALHOOKSWITCHMODE    = 16_90000010;
  PHONEERR_INVALLAMPMODE          = 16_90000011;
  PHONEERR_INVALPARAM             = 16_90000012;
  PHONEERR_INVALPHONEHANDLE       = 16_90000013;
  PHONEERR_INVALPHONESTATE        = 16_90000014;
  PHONEERR_INVALPOINTER           = 16_90000015;
  PHONEERR_INVALPRIVILEGE         = 16_90000016;
  PHONEERR_INVALRINGMODE          = 16_90000017;
  PHONEERR_NODEVICE               = 16_90000018;
  PHONEERR_NODRIVER               = 16_90000019;
  PHONEERR_NOMEM                  = 16_9000001A;
  PHONEERR_NOTOWNER               = 16_9000001B;
  PHONEERR_OPERATIONFAILED        = 16_9000001C;
  PHONEERR_OPERATIONUNAVAIL       = 16_9000001D;
  PHONEERR_RESOURCEUNAVAIL        = 16_9000001F;
  PHONEERR_REQUESTOVERRUN         = 16_90000020;
  PHONEERR_STRUCTURETOOSMALL      = 16_90000021;
  PHONEERR_UNINITIALIZED          = 16_90000022;
  PHONEERR_REINIT                 = 16_90000023;

  PHONEHOOKSWITCHDEV_HANDSET      = 16_00000001;
  PHONEHOOKSWITCHDEV_SPEAKER      = 16_00000002;
  PHONEHOOKSWITCHDEV_HEADSET      = 16_00000004;
  PHONEHOOKSWITCHMODE_ONHOOK      = 16_00000001;
  PHONEHOOKSWITCHMODE_MIC         = 16_00000002;
  PHONEHOOKSWITCHMODE_SPEAKER     = 16_00000004;
  PHONEHOOKSWITCHMODE_MICSPEAKER  = 16_00000008;
  PHONEHOOKSWITCHMODE_UNKNOWN     = 16_00000010;
  PHONELAMPMODE_DUMMY             = 16_00000001;
  PHONELAMPMODE_OFF               = 16_00000002;
  PHONELAMPMODE_STEADY            = 16_00000004;
  PHONELAMPMODE_WINK              = 16_00000008;
  PHONELAMPMODE_FLASH             = 16_00000010;
  PHONELAMPMODE_FLUTTER           = 16_00000020;
  PHONELAMPMODE_BROKENFLUTTER     = 16_00000040;
  PHONELAMPMODE_UNKNOWN           = 16_00000080;
  PHONEPRIVILEGE_MONITOR          = 16_00000001;
  PHONEPRIVILEGE_OWNER            = 16_00000002;
  PHONESTATE_OTHER                = 16_00000001;
  PHONESTATE_CONNECTED            = 16_00000002;
  PHONESTATE_DISCONNECTED         = 16_00000004;
  PHONESTATE_OWNER                = 16_00000008;
  PHONESTATE_MONITORS             = 16_00000010;
  PHONESTATE_DISPLAY              = 16_00000020;
  PHONESTATE_LAMP                 = 16_00000040;
  PHONESTATE_RINGMODE             = 16_00000080;
  PHONESTATE_RINGVOLUME           = 16_00000100;
  PHONESTATE_HANDSETHOOKSWITCH    = 16_00000200;
  PHONESTATE_HANDSETVOLUME        = 16_00000400;
  PHONESTATE_HANDSETGAIN          = 16_00000800;
  PHONESTATE_SPEAKERHOOKSWITCH    = 16_00001000;
  PHONESTATE_SPEAKERVOLUME        = 16_00002000;
  PHONESTATE_SPEAKERGAIN          = 16_00004000;
  PHONESTATE_HEADSETHOOKSWITCH    = 16_00008000;
  PHONESTATE_HEADSETVOLUME        = 16_00010000;
  PHONESTATE_HEADSETGAIN          = 16_00020000;
  PHONESTATE_SUSPEND              = 16_00040000;
  PHONESTATE_RESUME               = 16_00080000;
  PHONESTATE_DEVSPECIFIC          = 16_00100000;
  PHONESTATE_REINIT               = 16_00200000;
  PHONESTATE_CAPSCHANGE           = 16_00400000;
  PHONESTATE_REMOVED              = 16_00800000;

  
TYPE
  phonestatus_tag = RECORD
    dwTotalSize                    : DWORD;
    dwNeededSize                   : DWORD;
    dwUsedSize                     : DWORD;
    dwStatusFlags                  : DWORD;
    dwNumOwners                    : DWORD;
    dwNumMonitors                  : DWORD;
    dwRingMode                     : DWORD;
    dwRingVolume                   : DWORD;
    dwHandsetHookSwitchMode        : DWORD;
    dwHandsetVolume                : DWORD;
    dwHandsetGain                  : DWORD;
    dwSpeakerHookSwitchMode        : DWORD;
    dwSpeakerVolume                : DWORD;
    dwSpeakerGain                  : DWORD;
    dwHeadsetHookSwitchMode        : DWORD;
    dwHeadsetVolume                : DWORD;
    dwHeadsetGain                  : DWORD;
    dwDisplaySize                  : DWORD;
    dwDisplayOffset                : DWORD;
    dwLampModesSize                : DWORD;
    dwLampModesOffset              : DWORD;
    dwOwnerNameSize                : DWORD;
    dwOwnerNameOffset              : DWORD;
    dwDevSpecificSize              : DWORD;
    dwDevSpecificOffset            : DWORD;
  END;

  PHONESTATUS = phonestatus_tag;
  LPPHONESTATUS = UNTRACED REF phonestatus_tag;

CONST
  PHONESTATUSFLAGS_CONNECTED      = 16_00000001;
  PHONESTATUSFLAGS_SUSPENDED      = 16_00000002;

  STRINGFORMAT_ASCII              = 16_00000001;
  STRINGFORMAT_DBCS               = 16_00000002;
  STRINGFORMAT_UNICODE            = 16_00000003;
  STRINGFORMAT_BINARY             = 16_00000004;

  
TYPE
  varstring_tag = RECORD
    dwTotalSize           : DWORD;
    dwNeededSize          : DWORD;
    dwUsedSize            : DWORD;
    dwStringFormat        : DWORD;
    dwStringSize          : DWORD;
    dwStringOffset        : DWORD;
  END;

  VARSTRING = varstring_tag;
  LPVARSTRING = UNTRACED REF varstring_tag;

(* Data types and values for Lines *)

CONST
  LINEADDRCAPFLAGS_FWDNUMRINGS    = 16_00000001;
  LINEADDRCAPFLAGS_PICKUPGROUPID  = 16_00000002;
  LINEADDRCAPFLAGS_SECURE         = 16_00000004;
  LINEADDRCAPFLAGS_BLOCKIDDEFAULT = 16_00000008;
  LINEADDRCAPFLAGS_BLOCKIDOVERRIDE= 16_00000010;
  LINEADDRCAPFLAGS_DIALED         = 16_00000020;
  LINEADDRCAPFLAGS_ORIGOFFHOOK    = 16_00000040;
  LINEADDRCAPFLAGS_DESTOFFHOOK    = 16_00000080;
  LINEADDRCAPFLAGS_FWDCONSULT     = 16_00000100;
  LINEADDRCAPFLAGS_SETUPCONFNULL  = 16_00000200;
  LINEADDRCAPFLAGS_AUTORECONNECT  = 16_00000400;
  LINEADDRCAPFLAGS_COMPLETIONID   = 16_00000800;
  LINEADDRCAPFLAGS_TRANSFERHELD   = 16_00001000;
  LINEADDRCAPFLAGS_TRANSFERMAKE   = 16_00002000;
  LINEADDRCAPFLAGS_CONFERENCEHELD = 16_00004000;
  LINEADDRCAPFLAGS_CONFERENCEMAKE = 16_00008000;
  LINEADDRCAPFLAGS_PARTIALDIAL    = 16_00010000;
  LINEADDRCAPFLAGS_FWDSTATUSVALID = 16_00020000;
  LINEADDRCAPFLAGS_FWDINTEXTADDR  = 16_00040000;
  LINEADDRCAPFLAGS_FWDBUSYNAADDR  = 16_00080000;
  LINEADDRCAPFLAGS_ACCEPTTOALERT  = 16_00100000;
  LINEADDRCAPFLAGS_CONFDROP       = 16_00200000;
  LINEADDRCAPFLAGS_PICKUPCALLWAIT = 16_00400000;

  
TYPE
  lineaddresscaps_tag = RECORD
    dwTotalSize                         : DWORD;
    dwNeededSize                        : DWORD;
    dwUsedSize                          : DWORD;
    dwLineDeviceID                      : DWORD;
    dwAddressSize                       : DWORD;
    dwAddressOffset                     : DWORD;
    dwDevSpecificSize                   : DWORD;
    dwDevSpecificOffset                 : DWORD;
    dwAddressSharing                    : DWORD;
    dwAddressStates                     : DWORD;
    dwCallInfoStates                    : DWORD;
    dwCallerIDFlags                     : DWORD;
    dwCalledIDFlags                     : DWORD;
    dwConnectedIDFlags                  : DWORD;
    dwRedirectionIDFlags                : DWORD;
    dwRedirectingIDFlags                : DWORD;
    dwCallStates                        : DWORD;
    dwDialToneModes                     : DWORD;
    dwBusyModes                         : DWORD;
    dwSpecialInfo                       : DWORD;
    dwDisconnectModes                   : DWORD;
    dwMaxNumActiveCalls                 : DWORD;
    dwMaxNumOnHoldCalls                 : DWORD;
    dwMaxNumOnHoldPendingCalls          : DWORD;
    dwMaxNumConference                  : DWORD;
    dwMaxNumTransConf                   : DWORD;
    dwAddrCapFlags                      : DWORD;
    dwCallFeatures                      : DWORD;
    dwRemoveFromConfCaps                : DWORD;
    dwRemoveFromConfState               : DWORD;
    dwTransferModes                     : DWORD;
    dwParkModes                         : DWORD;
    dwForwardModes                      : DWORD;
    dwMaxForwardEntries                 : DWORD;
    dwMaxSpecificEntries                : DWORD;
    dwMinFwdNumRings                    : DWORD;
    dwMaxFwdNumRings                    : DWORD;
    dwMaxCallCompletions                : DWORD;
    dwCallCompletionConds               : DWORD;
    dwCallCompletionModes               : DWORD;
    dwNumCompletionMessages             : DWORD;
    dwCompletionMsgTextEntrySize        : DWORD;
    dwCompletionMsgTextSize             : DWORD;
    dwCompletionMsgTextOffset           : DWORD;
    dwAddressFeatures                   : DWORD;
  END;

  LINEADDRESSCAPS = lineaddresscaps_tag;
  LPLINEADDRESSCAPS = UNTRACED REF lineaddresscaps_tag;

CONST
  LINEADDRESSMODE_ADDRESSID       = 16_00000001;
  LINEADDRESSMODE_DIALABLEADDR    = 16_00000002;


  LINEADDRESSSHARING_PRIVATE      = 16_00000001;
  LINEADDRESSSHARING_BRIDGEDEXCL  = 16_00000002;
  LINEADDRESSSHARING_BRIDGEDNEW   = 16_00000004;
  LINEADDRESSSHARING_BRIDGEDSHARED= 16_00000008;
  LINEADDRESSSHARING_MONITORED    = 16_00000010;

  LINEADDRESSSTATE_OTHER          = 16_00000001;
  LINEADDRESSSTATE_DEVSPECIFIC    = 16_00000002;
  LINEADDRESSSTATE_INUSEZERO      = 16_00000004;
  LINEADDRESSSTATE_INUSEONE       = 16_00000008;
  LINEADDRESSSTATE_INUSEMANY      = 16_00000010;
  LINEADDRESSSTATE_NUMCALLS       = 16_00000020;
  LINEADDRESSSTATE_FORWARD        = 16_00000040;
  LINEADDRESSSTATE_TERMINALS      = 16_00000080;
  LINEADDRESSSTATE_CAPSCHANGE     = 16_00000100;

  
TYPE
  lineaddressstatus_tag = RECORD
    dwTotalSize                  : DWORD;
    dwNeededSize                 : DWORD;
    dwUsedSize                   : DWORD;
    dwNumInUse                   : DWORD;
    dwNumActiveCalls             : DWORD;
    dwNumOnHoldCalls             : DWORD;
    dwNumOnHoldPendCalls         : DWORD;
    dwAddressFeatures            : DWORD;
    dwNumRingsNoAnswer           : DWORD;
    dwForwardNumEntries          : DWORD;
    dwForwardSize                : DWORD;
    dwForwardOffset              : DWORD;
    dwTerminalModesSize          : DWORD;
    dwTerminalModesOffset        : DWORD;
    dwDevSpecificSize            : DWORD;
    dwDevSpecificOffset          : DWORD;
  END;

  LINEADDRESSSTATUS = lineaddressstatus_tag;
  LPLINEADDRESSSTATUS = UNTRACED REF lineaddressstatus_tag;

CONST
  LINEADDRFEATURE_FORWARD         = 16_00000001;
  LINEADDRFEATURE_MAKECALL        = 16_00000002;
  LINEADDRFEATURE_PICKUP          = 16_00000004;
  LINEADDRFEATURE_SETMEDIACONTROL = 16_00000008;
  LINEADDRFEATURE_SETTERMINAL     = 16_00000010;
  LINEADDRFEATURE_SETUPCONF       = 16_00000020;
  LINEADDRFEATURE_UNCOMPLETECALL  = 16_00000040;
  LINEADDRFEATURE_UNPARK          = 16_00000080;

  LINEANSWERMODE_NONE             = 16_00000001;
  LINEANSWERMODE_DROP             = 16_00000002;
  LINEANSWERMODE_HOLD             = 16_00000004;


  LINEBEARERMODE_VOICE            = 16_00000001;
  LINEBEARERMODE_SPEECH           = 16_00000002;
  LINEBEARERMODE_MULTIUSE         = 16_00000004;
  LINEBEARERMODE_DATA             = 16_00000008;
  LINEBEARERMODE_ALTSPEECHDATA    = 16_00000010;
  LINEBEARERMODE_NONCALLSIGNALING = 16_00000020;
  LINEBEARERMODE_PASSTHROUGH      = 16_00000040;

  LINEBUSYMODE_STATION            = 16_00000001;
  LINEBUSYMODE_TRUNK              = 16_00000002;
  LINEBUSYMODE_UNKNOWN            = 16_00000004;
  LINEBUSYMODE_UNAVAIL            = 16_00000008;

  LINECALLCOMPLCOND_BUSY          = 16_00000001;
  LINECALLCOMPLCOND_NOANSWER      = 16_00000002;

  LINECALLCOMPLMODE_CAMPON        = 16_00000001;
  LINECALLCOMPLMODE_CALLBACK      = 16_00000002;
  LINECALLCOMPLMODE_INTRUDE       = 16_00000004;
  LINECALLCOMPLMODE_MESSAGE       = 16_00000008;

  LINECALLFEATURE_ACCEPT          = 16_00000001;
  LINECALLFEATURE_ADDTOCONF       = 16_00000002;
  LINECALLFEATURE_ANSWER          = 16_00000004;
  LINECALLFEATURE_BLINDTRANSFER   = 16_00000008;
  LINECALLFEATURE_COMPLETECALL    = 16_00000010;
  LINECALLFEATURE_COMPLETETRANSF  = 16_00000020;
  LINECALLFEATURE_DIAL            = 16_00000040;
  LINECALLFEATURE_DROP            = 16_00000080;
  LINECALLFEATURE_GATHERDIGITS    = 16_00000100;
  LINECALLFEATURE_GENERATEDIGITS  = 16_00000200;
  LINECALLFEATURE_GENERATETONE    = 16_00000400;
  LINECALLFEATURE_HOLD            = 16_00000800;
  LINECALLFEATURE_MONITORDIGITS   = 16_00001000;
  LINECALLFEATURE_MONITORMEDIA    = 16_00002000;
  LINECALLFEATURE_MONITORTONES    = 16_00004000;
  LINECALLFEATURE_PARK            = 16_00008000;
  LINECALLFEATURE_PREPAREADDCONF  = 16_00010000;
  LINECALLFEATURE_REDIRECT        = 16_00020000;
  LINECALLFEATURE_REMOVEFROMCONF  = 16_00040000;
  LINECALLFEATURE_SECURECALL      = 16_00080000;
  LINECALLFEATURE_SENDUSERUSER    = 16_00100000;
  LINECALLFEATURE_SETCALLPARAMS   = 16_00200000;
  LINECALLFEATURE_SETMEDIACONTROL = 16_00400000;
  LINECALLFEATURE_SETTERMINAL     = 16_00800000;
  LINECALLFEATURE_SETUPCONF       = 16_01000000;
  LINECALLFEATURE_SETUPTRANSFER   = 16_02000000;
  LINECALLFEATURE_SWAPHOLD        = 16_04000000;
  LINECALLFEATURE_UNHOLD          = 16_08000000;
  LINECALLFEATURE_RELEASEUSERUSERINFO= 16_10000000;

  
TYPE
  linedialparams_tag = RECORD
    dwDialPause              : DWORD;
    dwDialSpeed              : DWORD;
    dwDigitDuration          : DWORD;
    dwWaitForDialtone        : DWORD;
  END;

  LINEDIALPARAMS = linedialparams_tag;
  LPLINEDIALPARAMS = UNTRACED REF linedialparams_tag;

  linecallinfo_tag = RECORD
    dwTotalSize                       : DWORD;
    dwNeededSize                      : DWORD;
    dwUsedSize                        : DWORD;

    hLine                             : HLINE;
    dwLineDeviceID                    : DWORD;
    dwAddressID                       : DWORD;

    dwBearerMode                      : DWORD;
    dwRate                            : DWORD;
    dwMediaMode                       : DWORD;

    dwAppSpecific                     : DWORD;
    dwCallID                          : DWORD;
    dwRelatedCallID                   : DWORD;
    dwCallParamFlags                  : DWORD;
    dwCallStates                      : DWORD;
    dwMonitorDigitModes               : DWORD;
    dwMonitorMediaModes               : DWORD;
    DialParams                        : LINEDIALPARAMS;
    dwOrigin                          : DWORD;
    dwReason                          : DWORD;
    dwCompletionID                    : DWORD;
    dwNumOwners                       : DWORD;
    dwNumMonitors                     : DWORD;
    dwCountryCode                     : DWORD;
    dwTrunk                           : DWORD;
    dwCallerIDFlags                   : DWORD;
    dwCallerIDSize                    : DWORD;
    dwCallerIDOffset                  : DWORD;
    dwCallerIDNameSize                : DWORD;
    dwCallerIDNameOffset              : DWORD;
    dwCalledIDFlags                   : DWORD;
    dwCalledIDSize                    : DWORD;
    dwCalledIDOffset                  : DWORD;
    dwCalledIDNameSize                : DWORD;
    dwCalledIDNameOffset              : DWORD;
    dwConnectedIDFlags                : DWORD;
    dwConnectedIDSize                 : DWORD;
    dwConnectedIDOffset               : DWORD;
    dwConnectedIDNameSize             : DWORD;
    dwConnectedIDNameOffset           : DWORD;
    dwRedirectionIDFlags              : DWORD;
    dwRedirectionIDSize               : DWORD;
    dwRedirectionIDOffset             : DWORD;
    dwRedirectionIDNameSize           : DWORD;
    dwRedirectionIDNameOffset         : DWORD;
    dwRedirectingIDFlags              : DWORD;
    dwRedirectingIDSize               : DWORD;
    dwRedirectingIDOffset             : DWORD;
    dwRedirectingIDNameSize           : DWORD;
    dwRedirectingIDNameOffset         : DWORD;
    dwAppNameSize                     : DWORD;
    dwAppNameOffset                   : DWORD;
    dwDisplayableAddressSize          : DWORD;
    dwDisplayableAddressOffset        : DWORD;
    dwCalledPartySize                 : DWORD;
    dwCalledPartyOffset               : DWORD;
    dwCommentSize                     : DWORD;
    dwCommentOffset                   : DWORD;
    dwDisplaySize                     : DWORD;
    dwDisplayOffset                   : DWORD;
    dwUserUserInfoSize                : DWORD;
    dwUserUserInfoOffset              : DWORD;
    dwHighLevelCompSize               : DWORD;
    dwHighLevelCompOffset             : DWORD;
    dwLowLevelCompSize                : DWORD;
    dwLowLevelCompOffset              : DWORD;
    dwChargingInfoSize                : DWORD;
    dwChargingInfoOffset              : DWORD;
    dwTerminalModesSize               : DWORD;
    dwTerminalModesOffset             : DWORD;
    dwDevSpecificSize                 : DWORD;
    dwDevSpecificOffset               : DWORD;
  END;

  LINECALLINFO = linecallinfo_tag;
  LPLINECALLINFO = UNTRACED REF linecallinfo_tag;

CONST
  LINECALLINFOSTATE_OTHER            = 16_00000001;
  LINECALLINFOSTATE_DEVSPECIFIC      = 16_00000002;
  LINECALLINFOSTATE_BEARERMODE       = 16_00000004;
  LINECALLINFOSTATE_RATE             = 16_00000008;
  LINECALLINFOSTATE_MEDIAMODE        = 16_00000010;
  LINECALLINFOSTATE_APPSPECIFIC      = 16_00000020;
  LINECALLINFOSTATE_CALLID           = 16_00000040;
  LINECALLINFOSTATE_RELATEDCALLID    = 16_00000080;
  LINECALLINFOSTATE_ORIGIN           = 16_00000100;
  LINECALLINFOSTATE_REASON           = 16_00000200;
  LINECALLINFOSTATE_COMPLETIONID     = 16_00000400;
  LINECALLINFOSTATE_NUMOWNERINCR     = 16_00000800;
  LINECALLINFOSTATE_NUMOWNERDECR     = 16_00001000;
  LINECALLINFOSTATE_NUMMONITORS      = 16_00002000;
  LINECALLINFOSTATE_TRUNK            = 16_00004000;
  LINECALLINFOSTATE_CALLERID         = 16_00008000;
  LINECALLINFOSTATE_CALLEDID         = 16_00010000;
  LINECALLINFOSTATE_CONNECTEDID      = 16_00020000;
  LINECALLINFOSTATE_REDIRECTIONID    = 16_00040000;
  LINECALLINFOSTATE_REDIRECTINGID    = 16_00080000;
  LINECALLINFOSTATE_DISPLAY          = 16_00100000;
  LINECALLINFOSTATE_USERUSERINFO     = 16_00200000;
  LINECALLINFOSTATE_HIGHLEVELCOMP    = 16_00400000;
  LINECALLINFOSTATE_LOWLEVELCOMP     = 16_00800000;
  LINECALLINFOSTATE_CHARGINGINFO     = 16_01000000;
  LINECALLINFOSTATE_TERMINAL         = 16_02000000;
  LINECALLINFOSTATE_DIALPARAMS       = 16_04000000;
  LINECALLINFOSTATE_MONITORMODES     = 16_08000000;

  
TYPE
  linecalllist_tag = RECORD
    dwTotalSize              : DWORD;
    dwNeededSize             : DWORD;
    dwUsedSize               : DWORD;
    dwCallsNumEntries        : DWORD;
    dwCallsSize              : DWORD;
    dwCallsOffset            : DWORD;
  END;

  LINECALLLIST = linecalllist_tag;
  LPLINECALLLIST = UNTRACED REF linecalllist_tag;

CONST
  LINECALLORIGIN_OUTBOUND            = 16_00000001;
  LINECALLORIGIN_INTERNAL            = 16_00000002;
  LINECALLORIGIN_EXTERNAL            = 16_00000004;
  LINECALLORIGIN_UNKNOWN             = 16_00000010;
  LINECALLORIGIN_UNAVAIL             = 16_00000020;
  LINECALLORIGIN_CONFERENCE          = 16_00000040;
  LINECALLORIGIN_INBOUND             = 16_00000080;

  LINECALLPARAMFLAGS_SECURE          = 16_00000001;
  LINECALLPARAMFLAGS_IDLE            = 16_00000002;
  LINECALLPARAMFLAGS_BLOCKID         = 16_00000004;
  LINECALLPARAMFLAGS_ORIGOFFHOOK     = 16_00000008;
  LINECALLPARAMFLAGS_DESTOFFHOOK     = 16_00000010;

  
TYPE
  linecallparams_tag = RECORD                                   (* Defaults:  *)
    dwTotalSize                       : DWORD;                  (* --------- *)
    dwBearerMode                      : DWORD;                  (* voice  *)
    dwMinRate                         : DWORD;                  (* (3.1kHz) *)
    dwMaxRate                         : DWORD;                  (* (3.1kHz) *)
    dwMediaMode                       : DWORD;                  (* interactiveVoice *)
    dwCallParamFlags                  : DWORD;                  (* 0 *)
    dwAddressMode                     : DWORD;                  (* addressID  *)
    dwAddressID                       : DWORD;                  (* (any available)  *)
    DialParams                        : LINEDIALPARAMS;         (* (0, 0, 0, 0) *)
    dwOrigAddressSize                 : DWORD;                  (* 0 *)
    dwOrigAddressOffset               : DWORD;
    dwDisplayableAddressSize          : DWORD;
    dwDisplayableAddressOffset        : DWORD;
    dwCalledPartySize                 : DWORD;                  (* 0 *)
    dwCalledPartyOffset               : DWORD;
    dwCommentSize                     : DWORD;                  (* 0 *)
    dwCommentOffset                   : DWORD;
    dwUserUserInfoSize                : DWORD;                  (* 0 *)
    dwUserUserInfoOffset              : DWORD;
    dwHighLevelCompSize               : DWORD;                  (* 0 *)
    dwHighLevelCompOffset             : DWORD;
    dwLowLevelCompSize                : DWORD;                  (* 0 *)
    dwLowLevelCompOffset              : DWORD;
    dwDevSpecificSize                 : DWORD;                  (* 0 *)
    dwDevSpecificOffset               : DWORD;
  END;

  LINECALLPARAMS = linecallparams_tag;
  LPLINECALLPARAMS = UNTRACED REF linecallparams_tag;

CONST
  LINECALLPARTYID_BLOCKED            = 16_00000001;
  LINECALLPARTYID_OUTOFAREA          = 16_00000002;
  LINECALLPARTYID_NAME               = 16_00000004;
  LINECALLPARTYID_ADDRESS            = 16_00000008;
  LINECALLPARTYID_PARTIAL            = 16_00000010;
  LINECALLPARTYID_UNKNOWN            = 16_00000020;
  LINECALLPARTYID_UNAVAIL            = 16_00000040;

  LINECALLPRIVILEGE_NONE             = 16_00000001;
  LINECALLPRIVILEGE_MONITOR          = 16_00000002;
  LINECALLPRIVILEGE_OWNER            = 16_00000004;

  LINECALLREASON_DIRECT              = 16_00000001;
  LINECALLREASON_FWDBUSY             = 16_00000002;
  LINECALLREASON_FWDNOANSWER         = 16_00000004;
  LINECALLREASON_FWDUNCOND           = 16_00000008;
  LINECALLREASON_PICKUP              = 16_00000010;
  LINECALLREASON_UNPARK              = 16_00000020;
  LINECALLREASON_REDIRECT            = 16_00000040;
  LINECALLREASON_CALLCOMPLETION      = 16_00000080;
  LINECALLREASON_TRANSFER            = 16_00000100;
  LINECALLREASON_REMINDER            = 16_00000200;
  LINECALLREASON_UNKNOWN             = 16_00000400;
  LINECALLREASON_UNAVAIL             = 16_00000800;
  LINECALLREASON_INTRUDE             = 16_00001000;
  LINECALLREASON_PARKED              = 16_00002000;

  LINECALLSELECT_LINE                = 16_00000001;
  LINECALLSELECT_ADDRESS             = 16_00000002;
  LINECALLSELECT_CALL                = 16_00000004;

  LINECALLSTATE_IDLE                 = 16_00000001;
  LINECALLSTATE_OFFERING             = 16_00000002;
  LINECALLSTATE_ACCEPTED             = 16_00000004;
  LINECALLSTATE_DIALTONE             = 16_00000008;
  LINECALLSTATE_DIALING              = 16_00000010;
  LINECALLSTATE_RINGBACK             = 16_00000020;
  LINECALLSTATE_BUSY                 = 16_00000040;
  LINECALLSTATE_SPECIALINFO          = 16_00000080;
  LINECALLSTATE_CONNECTED            = 16_00000100;
  LINECALLSTATE_PROCEEDING           = 16_00000200;
  LINECALLSTATE_ONHOLD               = 16_00000400;
  LINECALLSTATE_CONFERENCED          = 16_00000800;
  LINECALLSTATE_ONHOLDPENDCONF       = 16_00001000;
  LINECALLSTATE_ONHOLDPENDTRANSFER   = 16_00002000;
  LINECALLSTATE_DISCONNECTED         = 16_00004000;
  LINECALLSTATE_UNKNOWN              = 16_00008000;

  LINECONNECTEDMODE_ACTIVE           = 16_00000001;
  LINECONNECTEDMODE_INACTIVE         = 16_00000002;

  LINEOFFERINGMODE_ACTIVE            = 16_00000001;
  LINEOFFERINGMODE_INACTIVE          = 16_00000002;

  
TYPE
  linecallstatus_tag = RECORD
    dwTotalSize                : DWORD;
    dwNeededSize               : DWORD;
    dwUsedSize                 : DWORD;
    dwCallState                : DWORD;
    dwCallStateMode            : DWORD;
    dwCallPrivilege            : DWORD;
    dwCallFeatures             : DWORD;
    dwDevSpecificSize          : DWORD;
    dwDevSpecificOffset        : DWORD;
  END;

  LINECALLSTATUS = linecallstatus_tag;
  LPLINECALLSTATUS = UNTRACED REF linecallstatus_tag;

CONST
  LINEDEVCAPFLAGS_CROSSADDRCONF      = 16_00000001;
  LINEDEVCAPFLAGS_HIGHLEVCOMP        = 16_00000002;
  LINEDEVCAPFLAGS_LOWLEVCOMP         = 16_00000004;
  LINEDEVCAPFLAGS_MEDIACONTROL       = 16_00000008;
  LINEDEVCAPFLAGS_MULTIPLEADDR       = 16_00000010;
  LINEDEVCAPFLAGS_CLOSEDROP          = 16_00000020;
  LINEDEVCAPFLAGS_DIALBILLING        = 16_00000040;
  LINEDEVCAPFLAGS_DIALQUIET          = 16_00000080;
  LINEDEVCAPFLAGS_DIALDIALTONE       = 16_00000100;

  
TYPE
  lineextensionid_tag = RECORD
    dwExtensionID0        : DWORD;
    dwExtensionID1        : DWORD;
    dwExtensionID2        : DWORD;
    dwExtensionID3        : DWORD;
  END;

  LINEEXTENSIONID = lineextensionid_tag;
  LPLINEEXTENSIONID = UNTRACED REF lineextensionid_tag;

  linedevcaps_tag = RECORD
    dwTotalSize                         : DWORD;
    dwNeededSize                        : DWORD;
    dwUsedSize                          : DWORD;
    dwProviderInfoSize                  : DWORD;
    dwProviderInfoOffset                : DWORD;
    dwSwitchInfoSize                    : DWORD;
    dwSwitchInfoOffset                  : DWORD;
    dwPermanentLineID                   : DWORD;
    dwLineNameSize                      : DWORD;
    dwLineNameOffset                    : DWORD;
    dwStringFormat                      : DWORD;
    dwAddressModes                      : DWORD;
    dwNumAddresses                      : DWORD;
    dwBearerModes                       : DWORD;
    dwMaxRate                           : DWORD;
    dwMediaModes                        : DWORD;
    dwGenerateToneModes                 : DWORD;
    dwGenerateToneMaxNumFreq            : DWORD;
    dwGenerateDigitModes                : DWORD;
    dwMonitorToneMaxNumFreq             : DWORD;
    dwMonitorToneMaxNumEntries          : DWORD;
    dwMonitorDigitModes                 : DWORD;
    dwGatherDigitsMinTimeout            : DWORD;
    dwGatherDigitsMaxTimeout            : DWORD;
    dwMedCtlDigitMaxListSize            : DWORD;
    dwMedCtlMediaMaxListSize            : DWORD;
    dwMedCtlToneMaxListSize             : DWORD;
    dwMedCtlCallStateMaxListSize        : DWORD;
    dwDevCapFlags                       : DWORD;
    dwMaxNumActiveCalls                 : DWORD;
    dwAnswerMode                        : DWORD;
    dwRingModes                         : DWORD;
    dwLineStates                        : DWORD;
    dwUUIAcceptSize                     : DWORD;
    dwUUIAnswerSize                     : DWORD;
    dwUUIMakeCallSize                   : DWORD;
    dwUUIDropSize                       : DWORD;
    dwUUISendUserUserInfoSize           : DWORD;
    dwUUICallInfoSize                   : DWORD;
    MinDialParams                       : LINEDIALPARAMS;
    MaxDialParams                       : LINEDIALPARAMS;
    DefaultDialParams                   : LINEDIALPARAMS;
    dwNumTerminals                      : DWORD;
    dwTerminalCapsSize                  : DWORD;
    dwTerminalCapsOffset                : DWORD;
    dwTerminalTextEntrySize             : DWORD;
    dwTerminalTextSize                  : DWORD;
    dwTerminalTextOffset                : DWORD;
    dwDevSpecificSize                   : DWORD;
    dwDevSpecificOffset                 : DWORD;
    dwLineFeatures                      : DWORD;
  END;

  LINEDEVCAPS = linedevcaps_tag;
  LPLINEDEVCAPS = UNTRACED REF linedevcaps_tag;

CONST
  LINEDEVSTATE_OTHER                 = 16_00000001;
  LINEDEVSTATE_RINGING               = 16_00000002;
  LINEDEVSTATE_CONNECTED             = 16_00000004;
  LINEDEVSTATE_DISCONNECTED          = 16_00000008;
  LINEDEVSTATE_MSGWAITON             = 16_00000010;
  LINEDEVSTATE_MSGWAITOFF            = 16_00000020;
  LINEDEVSTATE_INSERVICE             = 16_00000040;
  LINEDEVSTATE_OUTOFSERVICE          = 16_00000080;
  LINEDEVSTATE_MAINTENANCE           = 16_00000100;
  LINEDEVSTATE_OPEN                  = 16_00000200;
  LINEDEVSTATE_CLOSE                 = 16_00000400;
  LINEDEVSTATE_NUMCALLS              = 16_00000800;
  LINEDEVSTATE_NUMCOMPLETIONS        = 16_00001000;
  LINEDEVSTATE_TERMINALS             = 16_00002000;
  LINEDEVSTATE_ROAMMODE              = 16_00004000;
  LINEDEVSTATE_BATTERY               = 16_00008000;
  LINEDEVSTATE_SIGNAL                = 16_00010000;
  LINEDEVSTATE_DEVSPECIFIC           = 16_00020000;
  LINEDEVSTATE_REINIT                = 16_00040000;
  LINEDEVSTATE_LOCK                  = 16_00080000;
  LINEDEVSTATE_CAPSCHANGE            = 16_00100000;
  LINEDEVSTATE_CONFIGCHANGE          = 16_00200000;
  LINEDEVSTATE_TRANSLATECHANGE       = 16_00400000;
  LINEDEVSTATE_COMPLCANCEL           = 16_00800000;
  LINEDEVSTATE_REMOVED               = 16_01000000;

  
TYPE
  linedevstatus_tag = RECORD
    dwTotalSize                  : DWORD;
    dwNeededSize                 : DWORD;
    dwUsedSize                   : DWORD;
    dwNumOpens                   : DWORD;
    dwOpenMediaModes             : DWORD;
    dwNumActiveCalls             : DWORD;
    dwNumOnHoldCalls             : DWORD;
    dwNumOnHoldPendCalls         : DWORD;
    dwLineFeatures               : DWORD;
    dwNumCallCompletions         : DWORD;
    dwRingMode                   : DWORD;
    dwSignalLevel                : DWORD;
    dwBatteryLevel               : DWORD;
    dwRoamMode                   : DWORD;
    dwDevStatusFlags             : DWORD;
    dwTerminalModesSize          : DWORD;
    dwTerminalModesOffset        : DWORD;
    dwDevSpecificSize            : DWORD;
    dwDevSpecificOffset          : DWORD;
  END;

  LINEDEVSTATUS = linedevstatus_tag;
  LPLINEDEVSTATUS = UNTRACED REF linedevstatus_tag;

CONST
  LINEDEVSTATUSFLAGS_CONNECTED       = 16_00000001;
  LINEDEVSTATUSFLAGS_MSGWAIT         = 16_00000002;
  LINEDEVSTATUSFLAGS_INSERVICE       = 16_00000004;
  LINEDEVSTATUSFLAGS_LOCKED          = 16_00000008;

  LINEDIALTONEMODE_NORMAL            = 16_00000001;
  LINEDIALTONEMODE_SPECIAL           = 16_00000002;
  LINEDIALTONEMODE_INTERNAL          = 16_00000004;
  LINEDIALTONEMODE_EXTERNAL          = 16_00000008;
  LINEDIALTONEMODE_UNKNOWN           = 16_00000010;
  LINEDIALTONEMODE_UNAVAIL           = 16_00000020;

  LINEDIGITMODE_PULSE                = 16_00000001;
  LINEDIGITMODE_DTMF                 = 16_00000002;
  LINEDIGITMODE_DTMFEND              = 16_00000004;

  LINEDISCONNECTMODE_NORMAL          = 16_00000001;
  LINEDISCONNECTMODE_UNKNOWN         = 16_00000002;
  LINEDISCONNECTMODE_REJECT          = 16_00000004;
  LINEDISCONNECTMODE_PICKUP          = 16_00000008;
  LINEDISCONNECTMODE_FORWARDED       = 16_00000010;
  LINEDISCONNECTMODE_BUSY            = 16_00000020;
  LINEDISCONNECTMODE_NOANSWER        = 16_00000040;
  LINEDISCONNECTMODE_BADADDRESS      = 16_00000080;
  LINEDISCONNECTMODE_UNREACHABLE     = 16_00000100;
  LINEDISCONNECTMODE_CONGESTION      = 16_00000200;
  LINEDISCONNECTMODE_INCOMPATIBLE    = 16_00000400;
  LINEDISCONNECTMODE_UNAVAIL         = 16_00000800;
  LINEDISCONNECTMODE_NODIALTONE      = 16_00001000;

  LINEERR_ALLOCATED                  = 16_80000001;
  LINEERR_BADDEVICEID                = 16_80000002;
  LINEERR_BEARERMODEUNAVAIL          = 16_80000003;
  LINEERR_CALLUNAVAIL                = 16_80000005;
  LINEERR_COMPLETIONOVERRUN          = 16_80000006;
  LINEERR_CONFERENCEFULL             = 16_80000007;
  LINEERR_DIALBILLING                = 16_80000008;
  LINEERR_DIALDIALTONE               = 16_80000009;
  LINEERR_DIALPROMPT                 = 16_8000000A;
  LINEERR_DIALQUIET                  = 16_8000000B;
  LINEERR_INCOMPATIBLEAPIVERSION     = 16_8000000C;
  LINEERR_INCOMPATIBLEEXTVERSION     = 16_8000000D;
  LINEERR_INIFILECORRUPT             = 16_8000000E;
  LINEERR_INUSE                      = 16_8000000F;
  LINEERR_INVALADDRESS               = 16_80000010;
  LINEERR_INVALADDRESSID             = 16_80000011;
  LINEERR_INVALADDRESSMODE           = 16_80000012;
  LINEERR_INVALADDRESSSTATE          = 16_80000013;
  LINEERR_INVALAPPHANDLE             = 16_80000014;
  LINEERR_INVALAPPNAME               = 16_80000015;
  LINEERR_INVALBEARERMODE            = 16_80000016;
  LINEERR_INVALCALLCOMPLMODE         = 16_80000017;
  LINEERR_INVALCALLHANDLE            = 16_80000018;
  LINEERR_INVALCALLPARAMS            = 16_80000019;
  LINEERR_INVALCALLPRIVILEGE         = 16_8000001A;
  LINEERR_INVALCALLSELECT            = 16_8000001B;
  LINEERR_INVALCALLSTATE             = 16_8000001C;
  LINEERR_INVALCALLSTATELIST         = 16_8000001D;
  LINEERR_INVALCARD                  = 16_8000001E;
  LINEERR_INVALCOMPLETIONID          = 16_8000001F;
  LINEERR_INVALCONFCALLHANDLE        = 16_80000020;
  LINEERR_INVALCONSULTCALLHANDLE     = 16_80000021;
  LINEERR_INVALCOUNTRYCODE           = 16_80000022;
  LINEERR_INVALDEVICECLASS           = 16_80000023;
  LINEERR_INVALDEVICEHANDLE          = 16_80000024;
  LINEERR_INVALDIALPARAMS            = 16_80000025;
  LINEERR_INVALDIGITLIST             = 16_80000026;
  LINEERR_INVALDIGITMODE             = 16_80000027;
  LINEERR_INVALDIGITS                = 16_80000028;
  LINEERR_INVALEXTVERSION            = 16_80000029;
  LINEERR_INVALGROUPID               = 16_8000002A;
  LINEERR_INVALLINEHANDLE            = 16_8000002B;
  LINEERR_INVALLINESTATE             = 16_8000002C;
  LINEERR_INVALLOCATION              = 16_8000002D;
  LINEERR_INVALMEDIALIST             = 16_8000002E;
  LINEERR_INVALMEDIAMODE             = 16_8000002F;
  LINEERR_INVALMESSAGEID             = 16_80000030;
  LINEERR_INVALPARAM                 = 16_80000032;
  LINEERR_INVALPARKID                = 16_80000033;
  LINEERR_INVALPARKMODE              = 16_80000034;
  LINEERR_INVALPOINTER               = 16_80000035;
  LINEERR_INVALPRIVSELECT            = 16_80000036;
  LINEERR_INVALRATE                  = 16_80000037;
  LINEERR_INVALREQUESTMODE           = 16_80000038;
  LINEERR_INVALTERMINALID            = 16_80000039;
  LINEERR_INVALTERMINALMODE          = 16_8000003A;
  LINEERR_INVALTIMEOUT               = 16_8000003B;
  LINEERR_INVALTONE                  = 16_8000003C;
  LINEERR_INVALTONELIST              = 16_8000003D;
  LINEERR_INVALTONEMODE              = 16_8000003E;
  LINEERR_INVALTRANSFERMODE          = 16_8000003F;
  LINEERR_LINEMAPPERFAILED           = 16_80000040;
  LINEERR_NOCONFERENCE               = 16_80000041;
  LINEERR_NODEVICE                   = 16_80000042;
  LINEERR_NODRIVER                   = 16_80000043;
  LINEERR_NOMEM                      = 16_80000044;
  LINEERR_NOREQUEST                  = 16_80000045;
  LINEERR_NOTOWNER                   = 16_80000046;
  LINEERR_NOTREGISTERED              = 16_80000047;
  LINEERR_OPERATIONFAILED            = 16_80000048;
  LINEERR_OPERATIONUNAVAIL           = 16_80000049;
  LINEERR_RATEUNAVAIL                = 16_8000004A;
  LINEERR_RESOURCEUNAVAIL            = 16_8000004B;
  LINEERR_REQUESTOVERRUN             = 16_8000004C;
  LINEERR_STRUCTURETOOSMALL          = 16_8000004D;
  LINEERR_TARGETNOTFOUND             = 16_8000004E;
  LINEERR_TARGETSELF                 = 16_8000004F;
  LINEERR_UNINITIALIZED              = 16_80000050;
  LINEERR_USERUSERINFOTOOBIG         = 16_80000051;
  LINEERR_REINIT                     = 16_80000052;
  LINEERR_ADDRESSBLOCKED             = 16_80000053;
  LINEERR_BILLINGREJECTED            = 16_80000054;
  LINEERR_INVALFEATURE               = 16_80000055;
  LINEERR_NOMULTIPLEINSTANCE         = 16_80000056;

  LINEFEATURE_DEVSPECIFIC            = 16_00000001;
  LINEFEATURE_DEVSPECIFICFEAT        = 16_00000002;
  LINEFEATURE_FORWARD                = 16_00000004;
  LINEFEATURE_MAKECALL               = 16_00000008;
  LINEFEATURE_SETMEDIACONTROL        = 16_00000010;
  LINEFEATURE_SETTERMINAL            = 16_00000020;

  
TYPE
  lineforward_tag = RECORD
    dwForwardMode                : DWORD;
    dwCallerAddressSize          : DWORD;
    dwCallerAddressOffset        : DWORD;
    dwDestCountryCode            : DWORD;
    dwDestAddressSize            : DWORD;
    dwDestAddressOffset          : DWORD;
  END;

  LINEFORWARD = lineforward_tag;
  LPLINEFORWARD = UNTRACED REF lineforward_tag;

  ARRAY_OF_LINEFORWARD = ARRAY 
    [0..0] OF LINEFORWARD (* Check array boundaries! *) ;

  lineforwardlist_tag = RECORD
    dwTotalSize         : DWORD;
    dwNumEntries        : DWORD;
    ForwardList         : ARRAY_OF_LINEFORWARD;
  END;

  LINEFORWARDLIST = lineforwardlist_tag;
  LPLINEFORWARDLIST = UNTRACED REF lineforwardlist_tag;

CONST
  LINEFORWARDMODE_UNCOND             = 16_00000001;
  LINEFORWARDMODE_UNCONDINTERNAL     = 16_00000002;
  LINEFORWARDMODE_UNCONDEXTERNAL     = 16_00000004;
  LINEFORWARDMODE_UNCONDSPECIFIC     = 16_00000008;
  LINEFORWARDMODE_BUSY               = 16_00000010;
  LINEFORWARDMODE_BUSYINTERNAL       = 16_00000020;
  LINEFORWARDMODE_BUSYEXTERNAL       = 16_00000040;
  LINEFORWARDMODE_BUSYSPECIFIC       = 16_00000080;
  LINEFORWARDMODE_NOANSW             = 16_00000100;
  LINEFORWARDMODE_NOANSWINTERNAL     = 16_00000200;
  LINEFORWARDMODE_NOANSWEXTERNAL     = 16_00000400;
  LINEFORWARDMODE_NOANSWSPECIFIC     = 16_00000800;
  LINEFORWARDMODE_BUSYNA             = 16_00001000;
  LINEFORWARDMODE_BUSYNAINTERNAL     = 16_00002000;
  LINEFORWARDMODE_BUSYNAEXTERNAL     = 16_00004000;
  LINEFORWARDMODE_BUSYNASPECIFIC     = 16_00008000;
  LINEFORWARDMODE_UNKNOWN            = 16_00010000;
  LINEFORWARDMODE_UNAVAIL            = 16_00020000;

  LINEGATHERTERM_BUFFERFULL          = 16_00000001;
  LINEGATHERTERM_TERMDIGIT           = 16_00000002;
  LINEGATHERTERM_FIRSTTIMEOUT        = 16_00000004;
  LINEGATHERTERM_INTERTIMEOUT        = 16_00000008;
  LINEGATHERTERM_CANCEL              = 16_00000010;

  LINEGENERATETERM_DONE              = 16_00000001;
  LINEGENERATETERM_CANCEL            = 16_00000002;

  
TYPE
  linegeneratetone_tag = RECORD
    dwFrequency         : DWORD;
    dwCadenceOn         : DWORD;
    dwCadenceOff        : DWORD;
    dwVolume            : DWORD;
  END;

  LINEGENERATETONE = linegeneratetone_tag;
  LPLINEGENERATETONE = UNTRACED REF linegeneratetone_tag;

CONST
  LINEMAPPER = 16_FFFFFFFF;

  
TYPE
  linemediacontrolcallstate_tag = RECORD
    dwCallStates          : DWORD;
    dwMediaControl        : DWORD;
  END;

  LINEMEDIACONTROLCALLSTATE = linemediacontrolcallstate_tag;
  LPLINEMEDIACONTROLCALLSTATE = UNTRACED REF linemediacontrolcallstate_tag;

  linemediacontroldigit_tag = RECORD
    dwDigit               : DWORD;
    dwDigitModes          : DWORD;
    dwMediaControl        : DWORD;
  END;

  LINEMEDIACONTROLDIGIT = linemediacontroldigit_tag;
  LPLINEMEDIACONTROLDIGIT = UNTRACED REF linemediacontroldigit_tag;

  linemediacontrolmedia_tag = RECORD
    dwMediaModes          : DWORD;
    dwDuration            : DWORD;
    dwMediaControl        : DWORD;
  END;

  LINEMEDIACONTROLMEDIA = linemediacontrolmedia_tag;
  LPLINEMEDIACONTROLMEDIA = UNTRACED REF linemediacontrolmedia_tag;

  linemediacontroltone_tag = RECORD
    dwAppSpecific         : DWORD;
    dwDuration            : DWORD;
    dwFrequency1          : DWORD;
    dwFrequency2          : DWORD;
    dwFrequency3          : DWORD;
    dwMediaControl        : DWORD;
  END;

  LINEMEDIACONTROLTONE = linemediacontroltone_tag;
  LPLINEMEDIACONTROLTONE = UNTRACED REF linemediacontroltone_tag;

CONST
  LINEMEDIACONTROL_NONE              = 16_00000001;
  LINEMEDIACONTROL_START             = 16_00000002;
  LINEMEDIACONTROL_RESET             = 16_00000004;
  LINEMEDIACONTROL_PAUSE             = 16_00000008;
  LINEMEDIACONTROL_RESUME            = 16_00000010;
  LINEMEDIACONTROL_RATEUP            = 16_00000020;
  LINEMEDIACONTROL_RATEDOWN          = 16_00000040;
  LINEMEDIACONTROL_RATENORMAL        = 16_00000080;
  LINEMEDIACONTROL_VOLUMEUP          = 16_00000100;
  LINEMEDIACONTROL_VOLUMEDOWN        = 16_00000200;
  LINEMEDIACONTROL_VOLUMENORMAL      = 16_00000400;

  LINEMEDIAMODE_UNKNOWN              = 16_00000002;
  LINEMEDIAMODE_INTERACTIVEVOICE     = 16_00000004;
  LINEMEDIAMODE_AUTOMATEDVOICE       = 16_00000008;
  LINEMEDIAMODE_DATAMODEM            = 16_00000010;
  LINEMEDIAMODE_G3FAX                = 16_00000020;
  LINEMEDIAMODE_TDD                  = 16_00000040;
  LINEMEDIAMODE_G4FAX                = 16_00000080;
  LINEMEDIAMODE_DIGITALDATA          = 16_00000100;
  LINEMEDIAMODE_TELETEX              = 16_00000200;
  LINEMEDIAMODE_VIDEOTEX             = 16_00000400;
  LINEMEDIAMODE_TELEX                = 16_00000800;
  LINEMEDIAMODE_MIXED                = 16_00001000;
  LINEMEDIAMODE_ADSI                 = 16_00002000;
  LINEMEDIAMODE_VOICEVIEW            = 16_00004000;

  LAST_LINEMEDIAMODE                 = 16_00004000;

  
TYPE
  linemonitortone_tag = RECORD
    dwAppSpecific        : DWORD;
    dwDuration           : DWORD;
    dwFrequency1         : DWORD;
    dwFrequency2         : DWORD;
    dwFrequency3         : DWORD;
  END;

  LINEMONITORTONE = linemonitortone_tag;
  LPLINEMONITORTONE = UNTRACED REF linemonitortone_tag;

CONST
  LINEPARKMODE_DIRECTED              = 16_00000001;
  LINEPARKMODE_NONDIRECTED           = 16_00000002;

  LINEREMOVEFROMCONF_NONE            = 16_00000001;
  LINEREMOVEFROMCONF_LAST            = 16_00000002;
  LINEREMOVEFROMCONF_ANY             = 16_00000003;

  
TYPE
  linereqmakecall_tag = RECORD
    szDestAddress : ARRAY [0..TAPIMAXDESTADDRESSSIZE-1] OF char;
    szAppName     : ARRAY [0..TAPIMAXAPPNAMESIZE-1] OF char;
    szCalledParty : ARRAY [0..TAPIMAXCALLEDPARTYSIZE-1] OF char;
    szComment     : ARRAY [0..TAPIMAXCOMMENTSIZE-1] OF char;
  END;

  LINEREQMAKECALL = linereqmakecall_tag;
  LPLINEREQMAKECALL = UNTRACED REF linereqmakecall_tag;

  linereqmediacall_tag = RECORD
    hWnd                 : HWND;
    wRequestID           : WPARAM;
    szDeviceClass        : ARRAY [0..TAPIMAXDEVICECLASSSIZE-1] OF char;
    ucDeviceID           : ARRAY [0..TAPIMAXDEVICEIDSIZE-1] OF unsigned_char;
    dwSize               : DWORD;
    dwSecure             : DWORD;
    szDestAddress        : ARRAY [0..TAPIMAXDESTADDRESSSIZE-1] OF char;
    szAppName            : ARRAY [0..TAPIMAXAPPNAMESIZE-1] OF char;
    szCalledParty        : ARRAY [0..TAPIMAXCALLEDPARTYSIZE-1] OF char;
    szComment            : ARRAY [0..TAPIMAXCOMMENTSIZE-1] OF char;
  END;

  LINEREQMEDIACALL = linereqmediacall_tag;
  LPLINEREQMEDIACALL = UNTRACED REF linereqmediacall_tag;

CONST
  LINEREQUESTMODE_MAKECALL           = 16_1;
  LINEREQUESTMODE_MEDIACALL          = 16_2;
  LINEREQUESTMODE_DROP               = 16_4;

  LAST_LINEREQUESTMODE               = LINEREQUESTMODE_MEDIACALL;


  LINEROAMMODE_UNKNOWN               = 16_00000001;
  LINEROAMMODE_UNAVAIL               = 16_00000002;
  LINEROAMMODE_HOME                  = 16_00000004;
  LINEROAMMODE_ROAMA                 = 16_00000008;
  LINEROAMMODE_ROAMB                 = 16_00000010;
  LINESPECIALINFO_NOCIRCUIT          = 16_00000001;
  LINESPECIALINFO_CUSTIRREG          = 16_00000002;
  LINESPECIALINFO_REORDER            = 16_00000004;
  LINESPECIALINFO_UNKNOWN            = 16_00000008;
  LINESPECIALINFO_UNAVAIL            = 16_00000010;

  
TYPE
  linetermcaps_tag = RECORD
    dwTermDev            : DWORD;
    dwTermModes          : DWORD;
    dwTermSharing        : DWORD;
  END;

  LINETERMCAPS = linetermcaps_tag;
  LPLINETERMCAPS = UNTRACED REF linetermcaps_tag;

CONST
  LINETERMDEV_PHONE                  = 16_00000001;
  LINETERMDEV_HEADSET                = 16_00000002;
  LINETERMDEV_SPEAKER                = 16_00000004;

  LINETERMMODE_BUTTONS               = 16_00000001;
  LINETERMMODE_LAMPS                 = 16_00000002;
  LINETERMMODE_DISPLAY               = 16_00000004;
  LINETERMMODE_RINGER                = 16_00000008;
  LINETERMMODE_HOOKSWITCH            = 16_00000010;
  LINETERMMODE_MEDIATOLINE           = 16_00000020;
  LINETERMMODE_MEDIAFROMLINE         = 16_00000040;
  LINETERMMODE_MEDIABIDIRECT         = 16_00000080;

  LINETERMSHARING_PRIVATE            = 16_0000001;
  LINETERMSHARING_SHAREDEXCL         = 16_0000002;
  LINETERMSHARING_SHAREDCONF         = 16_0000004;

  LINETONEMODE_CUSTOM                = 16_0000001;
  LINETONEMODE_RINGBACK              = 16_0000002;
  LINETONEMODE_BUSY                  = 16_0000004;
  LINETONEMODE_BEEP                  = 16_0000008;
  LINETONEMODE_BILLING               = 16_0000010;

  LINETRANSFERMODE_TRANSFER          = 16_0000001;
  LINETRANSFERMODE_CONFERENCE        = 16_0000002;

  
TYPE
  linetranslateoutput_tag = RECORD
    dwTotalSize                      : DWORD;
    dwNeededSize                     : DWORD;
    dwUsedSize                       : DWORD;
    dwDialableStringSize             : DWORD;
    dwDialableStringOffset           : DWORD;
    dwDisplayableStringSize          : DWORD;
    dwDisplayableStringOffset        : DWORD;
    dwCurrentCountry                 : DWORD;
    dwDestCountry                    : DWORD;
    dwTranslateResults               : DWORD;
  END;

  LINETRANSLATEOUTPUT = linetranslateoutput_tag;
  LPLINETRANSLATEOUTPUT = UNTRACED REF linetranslateoutput_tag;

  linetranslatecaps_tag = RECORD
    dwTotalSize                     : DWORD;
    dwNeededSize                    : DWORD;
    dwUsedSize                      : DWORD;
    dwNumLocations                  : DWORD;
    dwLocationListSize              : DWORD;
    dwLocationListOffset            : DWORD;
    dwCurrentLocationID             : DWORD;
    dwNumCards                      : DWORD;
    dwCardListSize                  : DWORD;
    dwCardListOffset                : DWORD;
    dwCurrentPreferredCardID        : DWORD;
  END;

  LINETRANSLATECAPS = linetranslatecaps_tag;
  LPLINETRANSLATECAPS = UNTRACED REF linetranslatecaps_tag;

  linelocationentry_tag = RECORD
    dwPermanentLocationID                 : DWORD;
    dwLocationNameSize                    : DWORD;
    dwLocationNameOffset                  : DWORD;
    dwCountryCode                         : DWORD;
    dwCityCodeSize                        : DWORD;
    dwCityCodeOffset                      : DWORD;
    dwPreferredCardID                     : DWORD;
    dwLocalAccessCodeSize                 : DWORD;
    dwLocalAccessCodeOffset               : DWORD;
    dwLongDistanceAccessCodeSize          : DWORD;
    dwLongDistanceAccessCodeOffset        : DWORD;
    dwTollPrefixListSize                  : DWORD;
    dwTollPrefixListOffset                : DWORD;
    dwCountryID                           : DWORD;
    dwOptions                             : DWORD;
    dwCancelCallWaitingSize               : DWORD;
    dwCancelCallWaitingOffset             : DWORD;
  END;

  LINELOCATIONENTRY = linelocationentry_tag;
  LPLINELOCATIONENTRY = UNTRACED REF linelocationentry_tag;

  linecardentry_tag = RECORD
    dwPermanentCardID                : DWORD;
    dwCardNameSize                   : DWORD;
    dwCardNameOffset                 : DWORD;
    dwCardNumberDigits               : DWORD;
    dwSameAreaRuleSize               : DWORD;
    dwSameAreaRuleOffset             : DWORD;
    dwLongDistanceRuleSize           : DWORD;
    dwLongDistanceRuleOffset         : DWORD;
    dwInternationalRuleSize          : DWORD;
    dwInternationalRuleOffset        : DWORD;
    dwOptions                        : DWORD;
  END;

  LINECARDENTRY = linecardentry_tag;
  LPLINECARDENTRY = UNTRACED REF linecardentry_tag;

  linecountrylist_tag = RECORD
    dwTotalSize                : DWORD;
    dwNeededSize               : DWORD;
    dwUsedSize                 : DWORD;
    dwNumCountries             : DWORD;
    dwCountryListSize          : DWORD;
    dwCountryListOffset        : DWORD;
  END;

  LINECOUNTRYLIST = linecountrylist_tag;
  LPLINECOUNTRYLIST = UNTRACED REF linecountrylist_tag;

  linecountryentry_tag = RECORD
    dwCountryID                      : DWORD;
    dwCountryCode                    : DWORD;
    dwNextCountryID                  : DWORD;
    dwCountryNameSize                : DWORD;
    dwCountryNameOffset              : DWORD;
    dwSameAreaRuleSize               : DWORD;
    dwSameAreaRuleOffset             : DWORD;
    dwLongDistanceRuleSize           : DWORD;
    dwLongDistanceRuleOffset         : DWORD;
    dwInternationalRuleSize          : DWORD;
    dwInternationalRuleOffset        : DWORD;
  END;

  LINECOUNTRYENTRY = linecountryentry_tag;
  LPLINECOUNTRYENTRY = UNTRACED REF linecountryentry_tag;

  lineproviderlist_tag = RECORD
    dwTotalSize                 : DWORD;
    dwNeededSize                : DWORD;
    dwUsedSize                  : DWORD;
    dwNumProviders              : DWORD;
    dwProviderListSize          : DWORD;
    dwProviderListOffset        : DWORD;
  END;

  LINEPROVIDERLIST = lineproviderlist_tag;
  LPLINEPROVIDERLIST = UNTRACED REF lineproviderlist_tag;

  lineproviderentry_tag = RECORD
    dwPermanentProviderID           : DWORD;
    dwProviderFilenameSize          : DWORD;
    dwProviderFilenameOffset        : DWORD;
  END;

  LINEPROVIDERENTRY = lineproviderentry_tag;
  LPLINEPROVIDERENTRY = UNTRACED REF lineproviderentry_tag;

CONST
  LINETOLLLISTOPTION_ADD             = 16_00000001;
  LINETOLLLISTOPTION_REMOVE          = 16_00000002;

  LINETRANSLATEOPTION_CARDOVERRIDE     = 16_00000001;
  LINETRANSLATEOPTION_CANCELCALLWAITING= 16_00000002;
  LINETRANSLATEOPTION_FORCELOCAL       = 16_00000004;
  LINETRANSLATEOPTION_FORCELD          = 16_00000008;
  LINETRANSLATERESULT_CANONICAL        = 16_00000001;
  LINETRANSLATERESULT_INTERNATIONAL    = 16_00000002;
  LINETRANSLATERESULT_LONGDISTANCE     = 16_00000004;
  LINETRANSLATERESULT_LOCAL            = 16_00000008;
  LINETRANSLATERESULT_INTOLLLIST       = 16_00000010;
  LINETRANSLATERESULT_NOTINTOLLLIST    = 16_00000020;
  LINETRANSLATERESULT_DIALBILLING      = 16_00000040;
  LINETRANSLATERESULT_DIALQUIET        = 16_00000080;
  LINETRANSLATERESULT_DIALDIALTONE     = 16_00000100;
  LINETRANSLATERESULT_DIALPROMPT       = 16_00000200;

  LINELOCATIONOPTION_PULSEDIAL         = 16_00000001;

  LINECARDOPTION_PREDEFINED            = 16_00000001;
  LINECARDOPTION_HIDDEN                = 16_00000002;

(* Simple Telephony prototypes *)

<*EXTERNAL tapiRequestMakeCall : WINAPI *>
PROCEDURE tapiRequestMakeCall(lpszDestAddress: LPCSTR;
                              lpszAppName    : LPCSTR;
                              lpszCalledParty: LPCSTR;
                              lpszComment    : LPCSTR): LONG;

<*EXTERNAL tapiRequestMediaCall : WINAPI *>
PROCEDURE tapiRequestMediaCall(hWnd           : HWND;
                               wRequestID     : WPARAM;
                               lpszDeviceClass: LPCSTR;
                               lpDeviceID     : LPCSTR;
                               dwSize         : DWORD;
                               dwSecure       : DWORD;
                               lpszDestAddress: LPCSTR;
                               lpszAppName    : LPCSTR;
                               lpszCalledParty: LPCSTR;
                               lpszComment    : LPCSTR): LONG;

<*EXTERNAL tapiRequestDrop : WINAPI *>
PROCEDURE tapiRequestDrop(hWnd      : HWND;
                          wRequestID: WPARAM): LONG;

<*EXTERNAL lineRegisterRequestRecipient : WINAPI *>
PROCEDURE lineRegisterRequestRecipient(hLineApp              : HLINEAPP;
                                       dwRegistrationInstance: DWORD;
                                       dwRequestMode         : DWORD;
                                       bEnable               : DWORD): LONG;

<*EXTERNAL tapiGetLocationInfo : WINAPI *>
PROCEDURE tapiGetLocationInfo(lpszCountryCode: LPSTR;
                              lpszCityCode   : LPSTR): LONG;

<*EXTERNAL lineSetCurrentLocation : WINAPI *>
PROCEDURE lineSetCurrentLocation(hLineApp  : HLINEAPP;
                                 dwLocation: DWORD): LONG;

<*EXTERNAL lineSetTollList : WINAPI *>
PROCEDURE lineSetTollList(hLineApp        : HLINEAPP;
                          dwDeviceID      : DWORD;
                          lpszAddressIn   : LPCSTR;
                          dwTollListOption: DWORD): LONG;

<*EXTERNAL lineTranslateAddress : WINAPI *>
PROCEDURE lineTranslateAddress(hLineApp          : HLINEAPP;
                               dwDeviceID        : DWORD;
                               dwAPIVersion      : DWORD;
                               lpszAddressIn     : LPCSTR;
                               dwCard            : DWORD;
                               dwTranslateOptions: DWORD;
                               lpTranslateOutput : LPLINETRANSLATEOUTPUT): LONG;

<*EXTERNAL lineGetTranslateCaps : WINAPI *>
PROCEDURE lineGetTranslateCaps(hLineApp       : HLINEAPP;
                               dwAPIVersion   : DWORD;
                               lpTranslateCaps: LPLINETRANSLATECAPS): LONG;

(* Tapi function prototypes *)

<*EXTERNAL lineAccept : WINAPI *>
PROCEDURE lineAccept(hCall          : HCALL;
                     lpsUserUserInfo: LPCSTR;
                     dwSize         : DWORD): LONG;

<*EXTERNAL lineAddToConference : WINAPI *>
PROCEDURE lineAddToConference(hConfCall   : HCALL;
                              hConsultCall: HCALL): LONG;

<*EXTERNAL lineAnswer : WINAPI *>
PROCEDURE lineAnswer(hCall          : HCALL;
                     lpsUserUserInfo: LPCSTR;
                     dwSize         : DWORD): LONG;

<*EXTERNAL lineBlindTransfer : WINAPI *>
PROCEDURE lineBlindTransfer(hCall          : HCALL;
                            lpszDestAddress: LPCSTR;
                            dwCountryCode  : DWORD): LONG;

<*EXTERNAL lineClose : WINAPI *>
PROCEDURE lineClose(hLine: HLINE): LONG;

<*EXTERNAL lineCompleteCall : WINAPI *>
PROCEDURE lineCompleteCall(hCall           : HCALL;
                           lpdwCompletionID: LPDWORD;
                           dwCompletionMode: DWORD;
                           dwMessageID     : DWORD): LONG;

<*EXTERNAL lineCompleteTransfer : WINAPI *>
PROCEDURE lineCompleteTransfer(hCall         : HCALL;
                               hConsultCall  : HCALL;
                               lphConfCall   : LPHCALL;
                               dwTransferMode: DWORD): LONG;

<*EXTERNAL lineConfigDialog : WINAPI *>
PROCEDURE lineConfigDialog(dwDeviceID     : DWORD;
                           hwndOwner      : HWND;
                           lpszDeviceClass: LPCSTR): LONG;

<*EXTERNAL lineConfigDialogEdit : WINAPI *>
PROCEDURE lineConfigDialogEdit(dwDeviceID       : DWORD;
                               hwndOwner        : HWND;
                               lpszDeviceClass  : LPCSTR;
                               lpDeviceConfigIn : LPVOID;
                               dwSize           : DWORD;
                               lpDeviceConfigOut: LPVARSTRING): LONG;

<*EXTERNAL lineDeallocateCall : WINAPI *>
PROCEDURE lineDeallocateCall(hCall: HCALL): LONG;

<*EXTERNAL lineDevSpecific : WINAPI *>
PROCEDURE lineDevSpecific(hLine      : HLINE;
                          dwAddressID: DWORD;
                          hCall      : HCALL;
                          lpParams   : LPVOID;
                          dwSize     : DWORD): LONG;

<*EXTERNAL lineDevSpecificFeature : WINAPI *>
PROCEDURE lineDevSpecificFeature(hLine    : HLINE;
                                 dwFeature: DWORD;
                                 lpParams : LPVOID;
                                 dwSize   : DWORD): LONG;

<*EXTERNAL lineDial : WINAPI *>
PROCEDURE lineDial(hCall          : HCALL;
                   lpszDestAddress: LPCSTR;
                   dwCountryCode  : DWORD): LONG;

<*EXTERNAL lineDrop : WINAPI *>
PROCEDURE lineDrop(hCall          : HCALL;
                   lpsUserUserInfo: LPCSTR;
                   dwSize         : DWORD): LONG;

<*EXTERNAL lineForward : WINAPI *>
PROCEDURE lineForward(hLine             : HLINE;
                      bAllAddresses     : DWORD;
                      dwAddressID       : DWORD;
                      lpForwardList     : LPLINEFORWARDLIST;
                      dwNumRingsNoAnswer: DWORD;
                      lphConsultCall    : LPHCALL;
                      lpCallParams      : LPLINECALLPARAMS): LONG;

<*EXTERNAL lineGatherDigits : WINAPI *>
PROCEDURE lineGatherDigits(hCall                : HCALL;
                           dwDigitModes         : DWORD;
                           lpsDigits            : LPSTR;
                           dwNumDigits          : DWORD;
                           lpszTerminationDigits: LPCSTR;
                           dwFirstDigitTimeout  : DWORD;
                           dwInterDigitTimeout  : DWORD): LONG;

<*EXTERNAL lineGenerateDigits : WINAPI *>
PROCEDURE lineGenerateDigits(hCall      : HCALL;
                             dwDigitMode: DWORD;
                             lpszDigits : LPCSTR;
                             dwDuration : DWORD): LONG;

<*EXTERNAL lineGenerateTone : WINAPI *>
PROCEDURE lineGenerateTone(hCall     : HCALL;
                           dwToneMode: DWORD;
                           dwDuration: DWORD;
                           dwNumTones: DWORD;
                           lpTones   : LPLINEGENERATETONE): LONG;

<*EXTERNAL lineGetAddressCaps : WINAPI *>
PROCEDURE lineGetAddressCaps(hLineApp     : HLINEAPP;
                             dwDeviceID   : DWORD;
                             dwAddressID  : DWORD;
                             dwAPIVersion : DWORD;
                             dwExtVersion : DWORD;
                             lpAddressCaps: LPLINEADDRESSCAPS): LONG;

<*EXTERNAL lineGetAddressID : WINAPI *>
PROCEDURE lineGetAddressID(hLine        : HLINE;
                           lpdwAddressID: LPDWORD;
                           dwAddressMode: DWORD;
                           lpsAddress   : LPCSTR;
                           dwSize       : DWORD): LONG;

<*EXTERNAL lineGetAddressStatus : WINAPI *>
PROCEDURE lineGetAddressStatus(hLine          : HLINE;
                               dwAddressID    : DWORD;
                               lpAddressStatus: LPLINEADDRESSSTATUS): LONG;

<*EXTERNAL lineGetCallInfo : WINAPI *>
PROCEDURE lineGetCallInfo(hCall     : HCALL;
                          lpCallInfo: LPLINECALLINFO): LONG;

<*EXTERNAL lineGetCallStatus : WINAPI *>
PROCEDURE lineGetCallStatus(hCall       : HCALL;
                            lpCallStatus: LPLINECALLSTATUS): LONG;

<*EXTERNAL lineGetConfRelatedCalls : WINAPI *>
PROCEDURE lineGetConfRelatedCalls(hCall     : HCALL;
                                  lpCallList: LPLINECALLLIST): LONG;

<*EXTERNAL lineGetDevCaps : WINAPI *>
PROCEDURE lineGetDevCaps(hLineApp     : HLINEAPP;
                         dwDeviceID   : DWORD;
                         dwAPIVersion : DWORD;
                         dwExtVersion : DWORD;
                         lpLineDevCaps: LPLINEDEVCAPS): LONG;

<*EXTERNAL lineGetDevConfig : WINAPI *>
PROCEDURE lineGetDevConfig(dwDeviceID     : DWORD;
                           lpDeviceConfig : LPVARSTRING;
                           lpszDeviceClass: LPCSTR): LONG;

<*EXTERNAL lineGetNewCalls : WINAPI *>
PROCEDURE lineGetNewCalls(hLine      : HLINE;
                          dwAddressID: DWORD;
                          dwSelect   : DWORD;
                          lpCallList : LPLINECALLLIST): LONG;

<*EXTERNAL lineGetIcon : WINAPI *>
PROCEDURE lineGetIcon(dwDeviceID     : DWORD;
                      lpszDeviceClass: LPCSTR;
                      lphIcon        : LPHICON): LONG;

<*EXTERNAL lineGetID : WINAPI *>
PROCEDURE lineGetID(hLine          : HLINE;
                    dwAddressID    : DWORD;
                    hCall          : HCALL;
                    dwSelect       : DWORD;
                    lpDeviceID     : LPVARSTRING;
                    lpszDeviceClass: LPCSTR): LONG;

<*EXTERNAL lineGetLineDevStatus : WINAPI *>
PROCEDURE lineGetLineDevStatus(hLine          : HLINE;
                               lpLineDevStatus: LPLINEDEVSTATUS): LONG;

<*EXTERNAL lineGetNumRings : WINAPI *>
PROCEDURE lineGetNumRings(hLine       : HLINE;
                          dwAddressID : DWORD;
                          lpdwNumRings: LPDWORD): LONG;

<*EXTERNAL lineGetRequest : WINAPI *>
PROCEDURE lineGetRequest(hLineApp       : HLINEAPP;
                         dwRequestMode  : DWORD;
                         lpRequestBuffer: LPVOID): LONG;

<*EXTERNAL lineGetStatusMessages : WINAPI *>
PROCEDURE lineGetStatusMessages(hLine            : HLINE;
                                lpdwLineStates   : LPDWORD;
                                lpdwAddressStates: LPDWORD): LONG;

<*EXTERNAL lineHandoff : WINAPI *>
PROCEDURE lineHandoff(hCall       : HCALL;
                      lpszFileName: LPCSTR;
                      dwMediaMode : DWORD): LONG;

<*EXTERNAL lineHold : WINAPI *>
PROCEDURE lineHold(hCall: HCALL): LONG;

<*EXTERNAL lineInitialize : WINAPI *>
PROCEDURE lineInitialize(lphLineApp  : LPHLINEAPP;
                         hInstance   : HINSTANCE;
                         lpfnCallback: LINECALLBACK;
                         lpszAppName : LPCSTR;
                         lpdwNumDevs : LPDWORD): LONG;

<*EXTERNAL lineMakeCall : WINAPI *>
PROCEDURE lineMakeCall(hLine          : HLINE;
                       lphCall        : LPHCALL;
                       lpszDestAddress: LPCSTR;
                       dwCountryCode  : DWORD;
                       lpCallParams   : LPLINECALLPARAMS): LONG;

<*EXTERNAL lineMonitorDigits : WINAPI *>
PROCEDURE lineMonitorDigits(hCall       : HCALL;
                            dwDigitModes: DWORD): LONG;

<*EXTERNAL lineMonitorMedia : WINAPI *>
PROCEDURE lineMonitorMedia(hCall       : HCALL;
                           dwMediaModes: DWORD): LONG;

<*EXTERNAL lineMonitorTones : WINAPI *>
PROCEDURE lineMonitorTones(hCall       : HCALL;
                           lpToneList  : LPLINEMONITORTONE;
                           dwNumEntries: DWORD): LONG;

<*EXTERNAL lineNegotiateAPIVersion : WINAPI *>
PROCEDURE lineNegotiateAPIVersion(hLineApp        : HLINEAPP;
                                  dwDeviceID      : DWORD;
                                  dwAPILowVersion : DWORD;
                                  dwAPIHighVersion: DWORD;
                                  lpdwAPIVersion  : LPDWORD;
                                  lpExtensionID   : LPLINEEXTENSIONID): LONG;

<*EXTERNAL lineNegotiateExtVersion : WINAPI *>
PROCEDURE lineNegotiateExtVersion(hLineApp        : HLINEAPP;
                                  dwDeviceID      : DWORD;
                                  dwAPIVersion    : DWORD;
                                  dwExtLowVersion : DWORD;
                                  dwExtHighVersion: DWORD;
                                  lpdwExtVersion  : LPDWORD): LONG;

<*EXTERNAL lineOpen : WINAPI *>
PROCEDURE lineOpen(hLineApp          : HLINEAPP;
                   dwDeviceID        : DWORD;
                   lphLine           : LPHLINE;
                   dwAPIVersion      : DWORD;
                   dwExtVersion      : DWORD;
                   dwCallbackInstance: DWORD;
                   dwPrivileges      : DWORD;
                   dwMediaModes      : DWORD;
                   lpCallParams      : LPLINECALLPARAMS): LONG;

<*EXTERNAL linePark : WINAPI *>
PROCEDURE linePark(hCall          : HCALL;
                   dwParkMode     : DWORD;
                   lpszDirAddress : LPCSTR;
                   lpNonDirAddress: LPVARSTRING): LONG;

<*EXTERNAL linePickup : WINAPI *>
PROCEDURE linePickup(hLine          : HLINE;
                     dwAddressID    : DWORD;
                     lphCall        : LPHCALL;
                     lpszDestAddress: LPCSTR;
                     lpszGroupID    : LPCSTR): LONG;

<*EXTERNAL linePrepareAddToConference : WINAPI *>
PROCEDURE linePrepareAddToConference(hConfCall     : HCALL;
                                     lphConsultCall: LPHCALL;
                                     lpCallParams  : LPLINECALLPARAMS): LONG;

<*EXTERNAL lineRedirect : WINAPI *>
PROCEDURE lineRedirect(hCall          : HCALL;
                       lpszDestAddress: LPCSTR;
                       dwCountryCode  : DWORD): LONG;

<*EXTERNAL lineRemoveFromConference : WINAPI *>
PROCEDURE lineRemoveFromConference(hCall: HCALL): LONG;

<*EXTERNAL lineSecureCall : WINAPI *>
PROCEDURE lineSecureCall(hCall: HCALL): LONG;

<*EXTERNAL lineSendUserUserInfo : WINAPI *>
PROCEDURE lineSendUserUserInfo(hCall          : HCALL;
                               lpsUserUserInfo: LPCSTR;
                               dwSize         : DWORD): LONG;

<*EXTERNAL lineSetAppSpecific : WINAPI *>
PROCEDURE lineSetAppSpecific(hCall        : HCALL;
                             dwAppSpecific: DWORD): LONG;

<*EXTERNAL lineSetCallParams : WINAPI *>
PROCEDURE lineSetCallParams(hCall       : HCALL;
                            dwBearerMode: DWORD;
                            dwMinRate   : DWORD;
                            dwMaxRate   : DWORD;
                            lpDialParams: LPLINEDIALPARAMS): LONG;

<*EXTERNAL lineSetCallPrivilege : WINAPI *>
PROCEDURE lineSetCallPrivilege(hCall          : HCALL;
                               dwCallPrivilege: DWORD): LONG;

<*EXTERNAL lineSetDevConfig : WINAPI *>
PROCEDURE lineSetDevConfig(dwDeviceID     : DWORD;
                           lpDeviceConfig : LPVOID;
                           dwSize         : DWORD;
                           lpszDeviceClass: LPCSTR): LONG;

<*EXTERNAL lineSetMediaControl : WINAPI *>
PROCEDURE lineSetMediaControl(hLine                : HLINE;
                              dwAddressID          : DWORD;
                              hCall                : HCALL;
                              dwSelect             : DWORD;
                              lpDigitList          : LPLINEMEDIACONTROLDIGIT;
                              dwDigitNumEntries    : DWORD;
                              lpMediaList          : LPLINEMEDIACONTROLMEDIA;
                              dwMediaNumEntries    : DWORD;
                              lpToneList           : LPLINEMEDIACONTROLTONE;
                              dwToneNumEntries     : DWORD;
                              lpCallStateList      : LPLINEMEDIACONTROLCALLSTATE;
                              dwCallStateNumEntries: DWORD): LONG;

<*EXTERNAL lineSetMediaMode : WINAPI *>
PROCEDURE lineSetMediaMode(hCall       : HCALL;
                           dwMediaModes: DWORD): LONG;

<*EXTERNAL lineSetNumRings : WINAPI *>
PROCEDURE lineSetNumRings(hLine      : HLINE;
                          dwAddressID: DWORD;
                          dwNumRings : DWORD): LONG;

<*EXTERNAL lineSetStatusMessages : WINAPI *>
PROCEDURE lineSetStatusMessages(hLine          : HLINE;
                                dwLineStates   : DWORD;
                                dwAddressStates: DWORD): LONG;

<*EXTERNAL lineSetTerminal : WINAPI *>
PROCEDURE lineSetTerminal(hLine          : HLINE;
                          dwAddressID    : DWORD;
                          hCall          : HCALL;
                          dwSelect       : DWORD;
                          dwTerminalModes: DWORD;
                          dwTerminalID   : DWORD;
                          bEnable        : DWORD): LONG;

<*EXTERNAL lineSetupConference : WINAPI *>
PROCEDURE lineSetupConference(hCall         : HCALL;
                              hLine         : HLINE;
                              lphConfCall   : LPHCALL;
                              lphConsultCall: LPHCALL;
                              dwNumParties  : DWORD;
                              lpCallParams  : LPLINECALLPARAMS): LONG;

<*EXTERNAL lineSetupTransfer : WINAPI *>
PROCEDURE lineSetupTransfer(hCall         : HCALL;
                            lphConsultCall: LPHCALL;
                            lpCallParams  : LPLINECALLPARAMS): LONG;

<*EXTERNAL lineShutdown : WINAPI *>
PROCEDURE lineShutdown(hLineApp: HLINEAPP): LONG;

<*EXTERNAL lineSwapHold : WINAPI *>
PROCEDURE lineSwapHold(hActiveCall: HCALL;
                       hHeldCall  : HCALL): LONG;

<*EXTERNAL lineUncompleteCall : WINAPI *>
PROCEDURE lineUncompleteCall(hLine         : HLINE;
                             dwCompletionID: DWORD): LONG;

<*EXTERNAL lineUnhold : WINAPI *>
PROCEDURE lineUnhold(hCall: HCALL): LONG;

<*EXTERNAL lineUnpark : WINAPI *>
PROCEDURE lineUnpark(hLine          : HLINE;
                     dwAddressID    : DWORD;
                     lphCall        : LPHCALL;
                     lpszDestAddress: LPCSTR): LONG;

<*EXTERNAL lineReleaseUserUserInfo : WINAPI *>
PROCEDURE lineReleaseUserUserInfo(hCall: HCALL): LONG;

<*EXTERNAL phoneClose : WINAPI *>
PROCEDURE phoneClose(hPhone: HPHONE): LONG;

<*EXTERNAL phoneConfigDialog : WINAPI *>
PROCEDURE phoneConfigDialog(dwDeviceID     : DWORD;
                            hwndOwner      : HWND;
                            lpszDeviceClass: LPCSTR): LONG;

<*EXTERNAL phoneDevSpecific : WINAPI *>
PROCEDURE phoneDevSpecific(hPhone  : HPHONE;
                           lpParams: LPVOID;
                           dwSize  : DWORD): LONG;

<*EXTERNAL phoneGetButtonInfo : WINAPI *>
PROCEDURE phoneGetButtonInfo(hPhone        : HPHONE;
                             dwButtonLampID: DWORD;
                             lpButtonInfo  : LPPHONEBUTTONINFO): LONG;

<*EXTERNAL phoneGetData : WINAPI *>
PROCEDURE phoneGetData(hPhone  : HPHONE;
                       dwDataID: DWORD;
                       lpData  : LPVOID;
                       dwSize  : DWORD): LONG;

<*EXTERNAL phoneGetDevCaps : WINAPI *>
PROCEDURE phoneGetDevCaps(hPhoneApp   : HPHONEAPP;
                          dwDeviceID  : DWORD;
                          dwAPIVersion: DWORD;
                          dwExtVersion: DWORD;
                          lpPhoneCaps : LPPHONECAPS): LONG;

<*EXTERNAL phoneGetDisplay : WINAPI *>
PROCEDURE phoneGetDisplay(hPhone   : HPHONE;
                          lpDisplay: LPVARSTRING): LONG;

<*EXTERNAL phoneGetGain : WINAPI *>
PROCEDURE phoneGetGain(hPhone         : HPHONE;
                       dwHookSwitchDev: DWORD;
                       lpdwGain       : LPDWORD): LONG;

<*EXTERNAL phoneGetHookSwitch : WINAPI *>
PROCEDURE phoneGetHookSwitch(hPhone            : HPHONE;
                             lpdwHookSwitchDevs: LPDWORD): LONG;

<*EXTERNAL phoneGetIcon : WINAPI *>
PROCEDURE phoneGetIcon(dwDeviceID     : DWORD;
                       lpszDeviceClass: LPCSTR;
                       lphIcon        : LPHICON): LONG;

<*EXTERNAL phoneGetID : WINAPI *>
PROCEDURE phoneGetID(hPhone         : HPHONE;
                     lpDeviceID     : LPVARSTRING;
                     lpszDeviceClass: LPCSTR): LONG;

<*EXTERNAL phoneGetLamp : WINAPI *>
PROCEDURE phoneGetLamp(hPhone        : HPHONE;
                       dwButtonLampID: DWORD;
                       lpdwLampMode  : LPDWORD): LONG;

<*EXTERNAL phoneGetRing : WINAPI *>
PROCEDURE phoneGetRing(hPhone      : HPHONE;
                       lpdwRingMode: LPDWORD;
                       lpdwVolume  : LPDWORD): LONG;

<*EXTERNAL phoneGetStatus : WINAPI *>
PROCEDURE phoneGetStatus(hPhone       : HPHONE;
                         lpPhoneStatus: LPPHONESTATUS): LONG;

<*EXTERNAL phoneGetStatusMessages : WINAPI *>
PROCEDURE phoneGetStatusMessages(hPhone          : HPHONE;
                                 lpdwPhoneStates : LPDWORD;
                                 lpdwButtonModes : LPDWORD;
                                 lpdwButtonStates: LPDWORD): LONG;

<*EXTERNAL phoneGetVolume : WINAPI *>
PROCEDURE phoneGetVolume(hPhone         : HPHONE;
                         dwHookSwitchDev: DWORD;
                         lpdwVolume     : LPDWORD): LONG;

<*EXTERNAL phoneInitialize : WINAPI *>
PROCEDURE phoneInitialize(lphPhoneApp : LPHPHONEAPP;
                          hInstance   : HINSTANCE;
                          lpfnCallback: PHONECALLBACK;
                          lpszAppName : LPCSTR;
                          lpdwNumDevs : LPDWORD): LONG;

<*EXTERNAL phoneNegotiateAPIVersion : WINAPI *>
PROCEDURE phoneNegotiateAPIVersion(hPhoneApp       : HPHONEAPP;
                                   dwDeviceID      : DWORD;
                                   dwAPILowVersion : DWORD;
                                   dwAPIHighVersion: DWORD;
                                   lpdwAPIVersion  : LPDWORD;
                                   lpExtensionID   : LPPHONEEXTENSIONID): LONG;

<*EXTERNAL phoneNegotiateExtVersion : WINAPI *>
PROCEDURE phoneNegotiateExtVersion(hPhoneApp       : HPHONEAPP;
                                   dwDeviceID      : DWORD;
                                   dwAPIVersion    : DWORD;
                                   dwExtLowVersion : DWORD;
                                   dwExtHighVersion: DWORD;
                                   lpdwExtVersion  : LPDWORD): LONG;

<*EXTERNAL phoneOpen : WINAPI *>
PROCEDURE phoneOpen(hPhoneApp         : HPHONEAPP;
                    dwDeviceID        : DWORD;
                    lphPhone          : LPHPHONE;
                    dwAPIVersion      : DWORD;
                    dwExtVersion      : DWORD;
                    dwCallbackInstance: DWORD;
                    dwPrivilege       : DWORD): LONG;

<*EXTERNAL phoneSetButtonInfo : WINAPI *>
PROCEDURE phoneSetButtonInfo(hPhone        : HPHONE;
                             dwButtonLampID: DWORD;
                             lpButtonInfo  : LPPHONEBUTTONINFO): LONG;

<*EXTERNAL phoneSetData : WINAPI *>
PROCEDURE phoneSetData(hPhone  : HPHONE;
                       dwDataID: DWORD;
                       lpData  : LPVOID;
                       dwSize  : DWORD): LONG;

<*EXTERNAL phoneSetDisplay : WINAPI *>
PROCEDURE phoneSetDisplay(hPhone    : HPHONE;
                          dwRow     : DWORD;
                          dwColumn  : DWORD;
                          lpsDisplay: LPCSTR;
                          dwSize    : DWORD): LONG;

<*EXTERNAL phoneSetGain : WINAPI *>
PROCEDURE phoneSetGain(hPhone         : HPHONE;
                       dwHookSwitchDev: DWORD;
                       dwGain         : DWORD): LONG;

<*EXTERNAL phoneSetHookSwitch : WINAPI *>
PROCEDURE phoneSetHookSwitch(hPhone          : HPHONE;
                             dwHookSwitchDevs: DWORD;
                             dwHookSwitchMode: DWORD): LONG;

<*EXTERNAL phoneSetLamp : WINAPI *>
PROCEDURE phoneSetLamp(hPhone        : HPHONE;
                       dwButtonLampID: DWORD;
                       dwLampMode    : DWORD): LONG;

<*EXTERNAL phoneSetRing : WINAPI *>
PROCEDURE phoneSetRing(hPhone    : HPHONE;
                       dwRingMode: DWORD;
                       dwVolume  : DWORD): LONG;

<*EXTERNAL phoneSetStatusMessages : WINAPI *>
PROCEDURE phoneSetStatusMessages(hPhone        : HPHONE;
                                 dwPhoneStates : DWORD;
                                 dwButtonModes : DWORD;
                                 dwButtonStates: DWORD): LONG;

<*EXTERNAL phoneSetVolume : WINAPI *>
PROCEDURE phoneSetVolume(hPhone         : HPHONE;
                         dwHookSwitchDev: DWORD;
                         dwVolume       : DWORD): LONG;

<*EXTERNAL phoneShutdown : WINAPI *>
PROCEDURE phoneShutdown(hPhoneApp: HPHONEAPP): LONG;

<*EXTERNAL lineTranslateDialog : WINAPI *>
PROCEDURE lineTranslateDialog(hLineApp     : HLINEAPP;
                              dwDeviceID   : DWORD;
                              dwAPIVersion : DWORD;
                              hwndOwner    : HWND;
                              lpszAddressIn: LPCSTR): LONG;

<*EXTERNAL lineGetCountry : WINAPI *>
PROCEDURE lineGetCountry(dwCountryID      : DWORD;
                         dwAPIVersion     : DWORD;
                         lpLineCountryList: LPLINECOUNTRYLIST): LONG;

<*EXTERNAL lineGetAppPriority : WINAPI *>
PROCEDURE lineGetAppPriority(lpszAppFilename: LPCSTR;
                             dwMediaMode    : DWORD;
                             lpExtensionID  : LPLINEEXTENSIONID;
                             dwRequestMode  : DWORD;
                             lpExtensionName: LPVARSTRING;
                             lpdwPriority   : LPDWORD): LONG;

<*EXTERNAL lineSetAppPriority : WINAPI *>
PROCEDURE lineSetAppPriority(lpszAppFilename  : LPCSTR;
                             dwMediaMode      : DWORD;
                             lpExtensionID    : LPLINEEXTENSIONID;
                             dwRequestMode    : DWORD;
                             lpszExtensionName: LPCSTR;
                             dwPriority       : DWORD): LONG;

<*EXTERNAL lineAddProvider : WINAPI *>
PROCEDURE lineAddProvider(lpszProviderFilename   : LPCSTR;
                          hwndOwner              : HWND;
                          lpdwPermanentProviderID: LPDWORD): LONG;

<*EXTERNAL lineConfigProvider : WINAPI *>
PROCEDURE lineConfigProvider(hwndOwner            : HWND;
                             dwPermanentProviderID: DWORD): LONG;

<*EXTERNAL lineRemoveProvider : WINAPI *>
PROCEDURE lineRemoveProvider(dwPermanentProviderID: DWORD;
                             hwndOwner            : HWND): LONG;

<*EXTERNAL lineGetProviderList : WINAPI *>
PROCEDURE lineGetProviderList(dwAPIVersion  : DWORD;
                              lpProviderList: LPLINEPROVIDERLIST): LONG;

(* #pragma pack() *)

END TAPI.
