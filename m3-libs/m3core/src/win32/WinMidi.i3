INTERFACE WinMidi;


(* 
	Based on mmsystem.h version 4.00
	Copyright 1992-1998, Microsoft Corp. All rights reserved.
	Copyright Darko Volaric 2002 darko@peter.com.au
*)


IMPORT
	WinDef;

(* definitions *)

CONST
	CALLBACK_TYPEMASK = 16_00070000;
	CALLBACK_NULL = 16_00000000;
	CALLBACK_WINDOW = 16_00010000;
	CALLBACK_TASK = 16_00020000;
	CALLBACK_FUNCTION = 16_00030000;
	CALLBACK_THREAD = CALLBACK_TASK;
	CALLBACK_EVENT = 16_00050000;
	MIDI_IO_STATUS = 16_00000020;

	MIM_OPEN         = 16_3C1;
	MIM_CLOSE        = 16_3C2;
	MIM_DATA         = 16_3C3;
	MIM_LONGDATA     = 16_3C4;
	MIM_ERROR        = 16_3C5;
	MIM_LONGERROR    = 16_3C6;
	MIM_MOREDATA     = 16_3CC;

	MOM_OPEN         = 16_3C7;
	MOM_CLOSE        = 16_3C8;
	MOM_DONE         = 16_3C9;
	MOM_POSITIONCB   = 16_3CA;
	


  MMSYSERR_NOERROR     = 0;  (* no error *)
  MMSYSERR_ERROR       = 1;  (* unspecified error *)
  MMSYSERR_BADDEVICEID = 2;  (* device ID out of range *)
  MMSYSERR_NOTENABLED  = 3;  (* driver failed enable *)
  MMSYSERR_ALLOCATED   = 4;  (* device already allocated *)
  MMSYSERR_INVALHANDLE = 5;  (* device handle is invalid *)
  MMSYSERR_NODRIVER    = 6;  (* no device driver present *)
  MMSYSERR_NOMEM       = 7;  (* memory allocation error *)
  MMSYSERR_NOTSUPPORTED= 8;  (* function isn't supported *)
  MMSYSERR_BADERRNUM   = 9;  (* error value out of range *)
  MMSYSERR_INVALFLAG   = 10; (* invalid flag passed *)
  MMSYSERR_INVALPARAM  = 11; (* invalid parameter passed *)
  MMSYSERR_HANDLEBUSY  = 12; (* handle being used simultaneously on another thread (eg callback) *)
  MMSYSERR_INVALIDALIAS= 13; (* specified alias not found *)
  MMSYSERR_BADDB       = 14; (* bad registry database *)
  MMSYSERR_KEYNOTFOUND = 15; (* registry key not found *)
  MMSYSERR_READERROR   = 16; (* registry read error *)
  MMSYSERR_WRITEERROR  = 17; (* registry write error *)
  MMSYSERR_DELETEERROR = 18; (* registry delete error *)
  MMSYSERR_VALNOTFOUND = 19; (* registry value not found *)
  MMSYSERR_NODRIVERCB  = 20; (* driver does not call DriverCallback *)
  MMSYSERR_LASTERROR   = 20; (* last error in range *)

	MMSYSERR = ARRAY [MMSYSERR_NOERROR..MMSYSERR_LASTERROR] OF TEXT {
		"no error",
		"unspecified error",
		"device ID out of range",
		"driver failed enable",
		"device already allocated",
		"device handle is invalid",
		"no device driver present",
		"memory allocation error",
		"function isn't supported",
		"error value out of range",
		"invalid flag passed",
		"invalid parameter passed",
		"handle being used simultaneously on another thread (eg callback)",
		"specified alias not found",
		"bad registry database",
		"registry key not found",
		"registry read error",
		"registry write error",
		"registry delete error",
		"registry value not found",
		"driver does not call DriverCallback"
	};

CONST
	MAXPNAMELEN = 32;

TYPE
	HMIDIOUT = ADDRESS;
	HMIDIIN = ADDRESS;

	UINT = WinDef.UINT;
	DWORD = WinDef.DWORD;
	WORD = WinDef.WORD;

	MMRESULT = UINT;
	MMVERSION = UINT;
	
(* functions *)

<*EXTERNAL midiOutOpen:WINAPI*>
PROCEDURE midiOutOpen(
	VAR phmo: HMIDIOUT;
	uDeviceID: UINT;
  dwCallback: DWORD;
  dwInstance: DWORD;
  fdwOpen: DWORD;
): MMRESULT;
<*EXTERNAL midiOutClose:WINAPI*>
PROCEDURE midiOutClose(hmo: HMIDIOUT): MMRESULT;
<*EXTERNAL midiInOpen:WINAPI*>
PROCEDURE midiInOpen(
	VAR phmi: HMIDIIN;
	uDeviceID: UINT;
	dwCallback: DWORD;
	dwInstance: DWORD;
	fdwOpen: DWORD
): MMRESULT;
<*EXTERNAL midiInClose:WINAPI*>
PROCEDURE midiInClose(hmi: HMIDIIN): MMRESULT;
<*EXTERNAL midiOutShortMsg:WINAPI*>
PROCEDURE midiOutShortMsg(hmo: HMIDIOUT; dwMsg: DWORD): MMRESULT;
<*EXTERNAL midiInGetNumDevs:WINAPI*>
PROCEDURE midiInGetNumDevs(): UINT;
<*EXTERNAL midiOutGetNumDevs:WINAPI*>
PROCEDURE midiOutGetNumDevs(): UINT;

<*EXTERNAL midiOutGetDevCapsA:WINAPI*>
PROCEDURE midiOutGetDevCapsA(uDeviceID: UINT; pmoc: LPMIDIOUTCAPS; cbmoc: UINT): MMRESULT;
<*EXTERNAL midiInGetDevCapsA:WINAPI*>
PROCEDURE midiInGetDevCapsA(uDeviceID: UINT; pmic: LPMIDIINCAPS; cbmic: UINT): MMRESULT;

<*EXTERNAL midiOutGetDevCapsW:WINAPI*>
PROCEDURE midiOutGetDevCapsW(uDeviceID: UINT; pmoc: LPMIDIOUTCAPS; cbmoc: UINT): MMRESULT;
<*EXTERNAL midiInGetDevCapsW:WINAPI*>
PROCEDURE midiInGetDevCapsW(uDeviceID: UINT; pmic: LPMIDIINCAPS; cbmic: UINT): MMRESULT;

<*EXTERNAL midiInStart:WINAPI*>
PROCEDURE midiInStart(hmi: HMIDIIN): MMRESULT;
<*EXTERNAL midiInStop:WINAPI*>
PROCEDURE midiInStop(hmi: HMIDIIN): MMRESULT;
<*EXTERNAL midiOutReset:WINAPI*>
PROCEDURE midiOutReset(hmo: HMIDIOUT): MMRESULT;

<*EXTERNAL midiOutPrepareHeader:WINAPI*>
PROCEDURE midiOutPrepareHeader(hmo: HMIDIOUT;  pmh: LPMIDIHDR; cbmh: UINT): MMRESULT;
<*EXTERNAL midiOutLongMsg:WINAPI*>
PROCEDURE midiOutLongMsg(hmo: HMIDIOUT;  pmh: LPMIDIHDR; cbmh: UINT): MMRESULT;
<*EXTERNAL midiOutUnprepareHeader:WINAPI*>
PROCEDURE midiOutUnprepareHeader(hmo: HMIDIOUT;  pmh: LPMIDIHDR; cbmh: UINT): MMRESULT;


(* structures *)

TYPE
	LPMIDIOUTCAPS = UNTRACED REF MIDIOUTCAPS;
	LPMIDIINCAPS = UNTRACED REF MIDIINCAPS;
	LPMIDIHDR = UNTRACED REF MIDIHDR;

	MIDIOUTCAPS = RECORD
    wMid: WORD; 
    wPid: WORD; 
    vDriverVersion: MMVERSION; 
    szPname: ARRAY[0..MAXPNAMELEN-1] OF CHAR; 
    wTechnology: WORD; 
    wVoices: WORD; 
    wNotes: WORD; 
    wChannelMask: WORD; 
    dwSupport: DWORD; 
	END;
	
	MIDIINCAPS = RECORD
    wMid: WORD; 
    wPid: WORD; 
    vDriverVersion: MMVERSION; 
    szPname: ARRAY[0..MAXPNAMELEN-1] OF CHAR; 
    dwSupport: DWORD; 
	END;

	MIDIHDR = RECORD
		lpData: ADDRESS;
		dwBufferLength: DWORD;
		dwBytesRecorded: DWORD;
		dwUser: DWORD;
		dwFlags: DWORD;
		lpNext: LPMIDIHDR;
		reserved: DWORD;
		dwOffset: DWORD;
		dwReserved: ARRAY [0..3] OF DWORD;
	END;

END WinMidi.